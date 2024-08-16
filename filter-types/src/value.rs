#![allow(clippy::enum_glob_use)]
use std::{collections::BTreeMap, convert::Infallible, fmt, sync::Arc};

use serde::Serialize;

use super::{MessageExt, Result};

/// Type alias representing a key-value structure of [`Value`]
pub type Structure<T> = BTreeMap<Arc<str>, Value<T>>;

/// Type alias of the pipeline value of a [`PValue`] iterator, representing a sequence.
pub type ListIter<'a> = Box<dyn Iterator<Item = PValue<'a>> + 'a>;

/// Type alias of the pipeline value of a (`Arc<str>`, [`PValue`]) iterator, representing a structure.
pub type StructIter<'a> = Box<dyn Iterator<Item = (Arc<str>, PValue<'a>)> + 'a>;

/// Trait for attempting to unwrap a [`Value`] into a concrete type.
pub trait TryFromValue<T>: Sized {
    /// Try to unwrap a [`Value`] variant into an instance of type `Self`.
    ///
    /// # Errors
    ///
    /// Implementors should return an `Err` if the input value cannot be unwrapped
    /// into `Self.`
    fn try_from_value(value: Value<T>) -> Result<Self>;
    /// Try to unwrap a [`Option<Value>`] into `Self`.  The default implementation
    /// is often sufficient, but sometimes it is helpful to be able to express
    /// this.
    ///
    /// # Errors
    ///
    /// Implementors should return an `Err` if the input value cannot be unwrapped
    /// into `Self.`
    fn try_from_option(value: Option<Value<T>>) -> Result<Self> {
        Self::try_from_value(value.msg("Expected a value, found null.")?)
    }
}

/// A variant-typed value of any type of value that can be represented in a scrapelect
/// program.
///
/// # Extension Type
///
/// It is possible to store other fields in the `Extra` variant of this enum, by
/// changing the type parameter `T`.  Note that this will affect the size of the
/// `Value`, and `Value<X>` is not necessarily transmutable with `Value<Y>`.  The
/// core `scrapelect` interpreter uses the [`Data`] (never type, no extra), [`Element`]
/// (contains element references), and [`Pipeline`] (contains element references and
/// intermediate iterators) extensions.
#[derive(Debug, Serialize, Clone, PartialEq)]
#[serde(untagged)]
pub enum Value<T = Data> {
    /// A value of `null`.
    #[serde(serialize_with = "serialize_null_as_option")]
    Null,
    /// A floating-point value.
    Float(f64),
    /// A signed integer value.
    Int(i64),
    /// A boolean value.  Note that this is distinct from the integer type.
    Bool(bool),
    /// A UTF-8 string value, stored as an `Arc<str>` for cheaper cloning.
    String(Arc<str>),
    /// A list of other values, not necessarily of the same type.
    List(Vec<Value<T>>),
    /// A String-value nested mapping of values.
    Structure(Structure<T>),
    /// Any extensions variants to this type.  See the main struct for more.
    Extra(T),
}

/// Helper trait to implement [`TryFromValue<T>` on all [`Value<X>`] for T if T is a
/// common data type that doesn't depend on T.
trait TryFromData: Sized {
    fn try_from_data(value: Value) -> Result<Self>;
}

macro_rules! generate_impls {
    ($($variant:ident ($ty:ty)$(,)?)*) => {
        $(
            impl TryFromData for $ty {
                fn try_from_data(value: Value) -> Result<Self> {
                    let Value::$variant(x) = value else {
                        bail!("expected a {}, got {}", stringify!($variant), value);
                    };
                    Ok(x)
                }

            }
            impl <X> From<$ty> for Value<X> {
                #[inline]
                fn from(x: $ty) -> Self {
                    Self::$variant(x)
                }
            }
        )*
    };
}

generate_impls! {
    Float(f64),
    Int(i64),
    Bool(bool),
    String(Arc<str>),
}

impl<X> Value<X> {
    /// Convert from a `Value<Data>` (no extensions) to `Self`.  This is always
    /// possible because `Value<Data>` is a subset of `Value<X>`.
    #[must_use]
    pub fn from_data(value: Value) -> Self {
        use Value::*;

        match value {
            Null => Null,
            Float(f) => Float(f),
            Int(i) => Int(i),
            Bool(b) => Bool(b),
            String(s) => String(s),
            List(l) => List(l.into_iter().map(Self::from_data).collect()),
            Structure(s) => Structure(
                s.into_iter()
                    .map(|(k, v)| (k, Self::from_data(v)))
                    .collect(),
            ),
            #[allow(unreachable_patterns)]
            Extra(never) => match never.0 {},
        }
    }

    /// Try to convert this Value into `Value<Data>`.
    ///
    /// This is a **lossy operation**.  If `self` is `Value::Extra`, it is not
    /// possible to convert into just data, and this will return `None`.
    #[must_use]
    pub fn into_data(self) -> Option<Value> {
        use Value::*;

        match self {
            Null => Some(Null),
            Float(f) => Some(Float(f)),
            Int(i) => Some(Int(i)),
            Bool(b) => Some(Bool(b)),
            String(s) => Some(String(s)),
            List(l) => Some(List(l.into_iter().filter_map(Self::into_data).collect())),
            Structure(s) => Some(Structure(
                s.into_iter()
                    .filter_map(|(k, v)| v.into_data().map(|v| (k, v)))
                    .collect(),
            )),
            Extra(_) => None,
        }
    }
}

impl<T: TryFromData, X> TryFromValue<X> for T {
    fn try_from_value(value: Value<X>) -> Result<Self> {
        T::try_from_data(
            value
                .into_data()
                .msg("unsupported data type with default TryFromData conversion")?,
        )
    }
}

/// The default extension for a [`Value`], marking that it is not possible to
/// have a `Value::Extra` variant.
#[allow(unreachable_code)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Data(Infallible);

impl Serialize for Data {
    fn serialize<S>(&self, _: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self.0 {}
    }
}

impl fmt::Display for Data {
    fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {}
    }
}

/// [`Value`] in an element block context, valid for that context.
pub type EValue<'a> = Value<Element<'a>>;

/// Extension to hold element context in a [`Value`].  Valid as long as the
/// element context is valid.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Element<'a> {
    /// An reference to an HTML element.
    Element(scraper::ElementRef<'a>),
}

impl fmt::Display for Element<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Element(elem) => write!(f, "`{}`", elem.html()),
        }
    }
}

/// Extension to hold traits about a value in a pipeline, like lazy iterators
/// and element context.
pub type PValue<'a> = Value<Pipeline<'a>>;

/// Extension to hold traits about a value in a pipeline, like lazy iterators
/// and element context.
#[non_exhaustive]
pub enum Pipeline<'a> {
    /// Any element context from [`Element`]
    Element(Element<'a>),
    /// A lazy iterator of a sequence of [`PValue`]s, collectable into a list.
    ListIter(Box<dyn Iterator<Item = Value<Pipeline<'a>>> + 'a>),
    /// A lazy iterator of a sequence of (`Arc<str>`, [`PValue`])s, collectable into a structure.
    StructIter(Box<dyn Iterator<Item = (Arc<str>, Value<Pipeline<'a>>)> + 'a>),
}

/// Convert from a [`PValue`] to a [`EValue`].  This is a lossless operation, and
/// will collect any lazy iterators into structures.
impl<'a> From<PValue<'a>> for EValue<'a> {
    fn from(value: PValue<'a>) -> Self {
        use Value::*;

        match value {
            Extra(Pipeline::Element(e)) => Extra(e),
            Extra(Pipeline::ListIter(it)) => List(it.map(Self::from).collect()),
            Extra(Pipeline::StructIter(it)) => Structure(it.map(|(k, v)| (k, v.into())).collect()),
            Null => Null,
            Float(f) => Float(f),
            Int(i) => Int(i),
            Bool(b) => Bool(b),
            String(s) => String(s),
            List(l) => List(l.into_iter().map(Self::from).collect()),
            Structure(s) => Structure(s.into_iter().map(|(k, v)| (k, v.into())).collect()),
        }
    }
}

/// Convert from an [`EValue`] to a [`PValue`].  This is a lossless operation, and
/// will prefer using iterators over collected data structures.
impl<'a> From<EValue<'a>> for PValue<'a> {
    fn from(value: EValue<'a>) -> Self {
        use Value::*;

        match value {
            Null => Null,
            Float(f) => Float(f),
            Int(i) => Int(i),
            Bool(b) => Bool(b),
            String(s) => String(s),
            List(l) => List(l.into_iter().map(Self::from).collect()),
            Structure(s) => Structure(s.into_iter().map(|(k, v)| (k, v.into())).collect()),
            Extra(e) => Extra(Pipeline::Element(e)),
        }
    }
}

/// Helper function to serialize `Value::Null` as `Option::None`, which is understood
/// as a null value by e.g., `serde_json`.
#[inline]
fn serialize_null_as_option<S: serde::Serializer>(se: S) -> core::result::Result<S::Ok, S::Error> {
    None::<()>.serialize(se)
}

/// Convert from an [`EValue`] to a [`PValue`].  This is a lossless operation, and
/// will prefer using iterators over collected data structures.
impl<T: fmt::Display> fmt::Display for Value<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::Int(n) => write!(f, "{n}"),
            Self::Float(x) => write!(f, "{x}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::String(s) => write!(f, r#""{s}""#),
            Self::List(ls) => {
                write!(f, "[")?;
                for x in ls {
                    write!(f, "{x}, ")?;
                }
                write!(f, "]")
            }
            Self::Structure(map) => {
                write!(f, "{{ ")?;
                for (k, v) in map {
                    write!(f, r#""{k}": {v}, "#)?;
                }
                write!(f, " }}")
            }
            Self::Extra(t) => write!(f, "{t}"),
        }
    }
}

impl<T, X> From<Option<T>> for Value<X>
where
    T: Into<Value<X>>,
{
    fn from(value: Option<T>) -> Self {
        match value {
            None => Self::Null,
            Some(x) => x.into(),
        }
    }
}

impl<'a, X> From<&'a str> for Value<X> {
    fn from(value: &'a str) -> Self {
        Self::String(Arc::from(value))
    }
}

impl<'a> From<scraper::ElementRef<'a>> for EValue<'a> {
    fn from(value: scraper::ElementRef<'a>) -> Self {
        Self::Extra(Element::Element(value))
    }
}

impl<X> Value<X> {
    /// Try to unwrap a value that implements [`TryFromValue<X>`].
    ///
    /// # Errors
    ///
    /// Returns an `Err` if it is not possible to unwrap `self` to an instance
    /// of type `T`.
    #[inline]
    pub fn try_unwrap<T: TryFromValue<X>>(self) -> Result<T> {
        T::try_from_value(self)
    }
}

impl<X> TryFromValue<X> for Value<X> {
    #[inline]
    fn try_from_value(value: Value<X>) -> Result<Self> {
        Ok(value)
    }
}

impl<X, T: TryFromValue<X>> TryFromValue<X> for Option<T> {
    fn try_from_value(value: Value<X>) -> Result<Self> {
        match value {
            Value::Null => Ok(None),
            other => T::try_from_value(other).map(Some),
        }
    }

    fn try_from_option(value: Option<Value<X>>) -> Result<Self> {
        // TODO: should we distinguish Some(Null) from None here?
        match value {
            // or T::try_from_data() to ignore None
            Some(v) => Self::try_from_value(v),
            None => Ok(None),
        }
    }
}

impl<'a> TryFromValue<Element<'a>> for scraper::ElementRef<'a> {
    fn try_from_value(value: Value<Element<'a>>) -> Result<Self> {
        match value {
            Value::Extra(Element::Element(e)) => Ok(e),
            _ => bail!("expected element, got {value}"),
        }
    }
}

impl<'a> TryFromValue<Pipeline<'a>> for scraper::ElementRef<'a> {
    fn try_from_value(value: Value<Pipeline<'a>>) -> Result<Self> {
        match value {
            Value::Extra(Pipeline::Element(Element::Element(e))) => Ok(e),
            _ => bail!("expected an element, got {}", EValue::from(value)),
        }
    }
}

impl<'a> TryFromValue<Pipeline<'a>> for ListIter<'a> {
    fn try_from_value(value: Value<Pipeline<'a>>) -> Result<Self> {
        match value {
            Value::Extra(Pipeline::ListIter(i)) => Ok(i),
            Value::List(v) => Ok(Box::new(v.into_iter())),
            _ => bail!("expected a List, got {}", EValue::from(value)),
        }
    }
}

impl<'a> TryFromValue<Pipeline<'a>> for Vec<PValue<'a>> {
    fn try_from_value(value: Value<Pipeline<'a>>) -> Result<Self> {
        match value {
            Value::Extra(Pipeline::ListIter(i)) => Ok(i.collect()),
            Value::List(v) => Ok(v),
            _ => bail!("expected a List, got {}", EValue::from(value)),
        }
    }
}

impl<'a> TryFromValue<Pipeline<'a>> for StructIter<'a> {
    fn try_from_value(value: Value<Pipeline<'a>>) -> Result<Self> {
        match value {
            Value::Extra(Pipeline::StructIter(i)) => Ok(i),
            Value::Structure(s) => Ok(Box::new(s.into_iter())),
            _ => bail!("expected a Structure, got {}", EValue::from(value)),
        }
    }
}

impl<'a> TryFromValue<Pipeline<'a>> for Structure<Pipeline<'a>> {
    fn try_from_value(value: Value<Pipeline<'a>>) -> Result<Self> {
        match value {
            Value::Extra(Pipeline::StructIter(i)) => Ok(i.collect()),
            Value::Structure(s) => Ok(s),
            _ => bail!("expected a Structure, got {}", EValue::from(value)),
        }
    }
}

impl<'a> TryFromValue<Element<'a>> for Vec<EValue<'a>> {
    fn try_from_value(value: Value<Element<'a>>) -> Result<Self> {
        let Value::List(v) = value else {
            bail!("expected a List, got {value}")
        };
        Ok(v)
    }
}

impl<'a> TryFromValue<Element<'a>> for Structure<Element<'a>> {
    fn try_from_value(value: Value<Element<'a>>) -> Result<Self> {
        let Value::Structure(s) = value else {
            bail!("expected a Structure, got {value}")
        };
        Ok(s)
    }
}
