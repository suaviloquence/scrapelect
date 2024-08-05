use std::{collections::BTreeMap, fmt, sync::Arc};

use anyhow::Context as _;
use serde::Serialize;

use super::Result;
pub type Structure<T> = BTreeMap<Arc<str>, Value<T>>;
pub type ListIter<'a> = Box<dyn Iterator<Item = PValue<'a>> + 'a>;
pub type StructIter<'a> = Box<dyn Iterator<Item = (Arc<str>, PValue<'a>)> + 'a>;

pub trait TryFromValue<T>: Sized {
    fn try_from_value(value: Value<T>) -> Result<Self>;
    fn try_from_option(value: Option<Value<T>>) -> Result<Self> {
        Self::try_from_value(value.context("Expected a value, found null.")?)
    }
}

#[derive(Debug, Serialize, Clone)]
#[serde(untagged)]
pub enum Value<T = Data> {
    #[serde(serialize_with = "serialize_null_as_option")]
    Null,
    Float(f64),
    Int(i64),
    Bool(bool),
    String(Arc<str>),
    List(Vec<Value<T>>),
    Structure(Structure<T>),
    Extra(T),
}

trait TryFromData: Sized {
    fn try_from_data(value: Value) -> Result<Self>;
}

macro_rules! generate_impls {
    ($($variant:ident ($ty:ty)$(,)?)*) => {
        $(
            impl TryFromData for $ty {
                fn try_from_data(value: Value) -> Result<Self> {
                    let Value::$variant(x) = value else {
                        anyhow::bail!("Expected {}, got {:?}", stringify!($variant), value);
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
    String(Arc<str>),
}

impl<X> Value<X> {
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
            Extra(never) => never.0,
        }
    }

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
                .context("Unsupported data type with default implementation")?,
        )
    }
}

#[allow(unreachable_code)]
#[derive(Debug, Clone)]
pub struct Data(!);

impl Serialize for Data {
    fn serialize<S>(&self, _: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0
    }
}

impl fmt::Display for Data {
    fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0
    }
}

pub type EValue<'a> = Value<ElementCtx<'a>>;

#[non_exhaustive]
#[derive(Debug, Clone)]
pub enum ElementCtx<'a> {
    Element(scraper::ElementRef<'a>),
}

impl fmt::Display for ElementCtx<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Element(elem) => write!(f, "`{}`", elem.html()),
        }
    }
}

pub type PValue<'a> = Value<Pipeline<'a>>;

#[non_exhaustive]
pub enum Pipeline<'a> {
    Element(ElementCtx<'a>),
    ListIter(Box<dyn Iterator<Item = Value<Pipeline<'a>>> + 'a>),
    StructIter(Box<dyn Iterator<Item = (Arc<str>, Value<Pipeline<'a>>)> + 'a>),
}

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

#[inline]
fn serialize_null_as_option<S: serde::Serializer>(se: S) -> core::result::Result<S::Ok, S::Error> {
    None::<()>.serialize(se)
}

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
        Self::Extra(ElementCtx::Element(value))
    }
}

impl<X> Value<X> {
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
}

impl<'a> TryFromValue<ElementCtx<'a>> for scraper::ElementRef<'a> {
    fn try_from_value(value: Value<ElementCtx<'a>>) -> Result<Self> {
        match value {
            Value::Extra(ElementCtx::Element(e)) => Ok(e),
            _ => anyhow::bail!("expected element, got {value:?}"),
        }
    }
}

impl<'a> TryFromValue<Pipeline<'a>> for scraper::ElementRef<'a> {
    fn try_from_value(value: Value<Pipeline<'a>>) -> Result<Self> {
        match value {
            Value::Extra(Pipeline::Element(ElementCtx::Element(e))) => Ok(e),
            _ => anyhow::bail!("expected element"),
        }
    }
}

impl<'a> TryFromValue<Pipeline<'a>> for ListIter<'a> {
    fn try_from_value(value: Value<Pipeline<'a>>) -> Result<Self> {
        match value {
            Value::Extra(Pipeline::ListIter(i)) => Ok(i),
            Value::List(v) => Ok(Box::new(v.into_iter())),
            _ => anyhow::bail!("expected sequence"),
        }
    }
}

impl<'a> TryFromValue<Pipeline<'a>> for Vec<PValue<'a>> {
    fn try_from_value(value: Value<Pipeline<'a>>) -> Result<Self> {
        match value {
            Value::Extra(Pipeline::ListIter(i)) => Ok(i.collect()),
            Value::List(v) => Ok(v),
            _ => anyhow::bail!("expected sequence"),
        }
    }
}

impl<'a> TryFromValue<Pipeline<'a>> for StructIter<'a> {
    fn try_from_value(value: Value<Pipeline<'a>>) -> Result<Self> {
        match value {
            Value::Extra(Pipeline::StructIter(i)) => Ok(i),
            Value::Structure(s) => Ok(Box::new(s.into_iter())),
            _ => anyhow::bail!("expected structure"),
        }
    }
}

impl<'a> TryFromValue<Pipeline<'a>> for Structure<Pipeline<'a>> {
    fn try_from_value(value: Value<Pipeline<'a>>) -> Result<Self> {
        match value {
            Value::Extra(Pipeline::StructIter(i)) => Ok(i.collect()),
            Value::Structure(s) => Ok(s),
            _ => anyhow::bail!("expected structure"),
        }
    }
}

impl<'a> TryFromValue<ElementCtx<'a>> for Vec<EValue<'a>> {
    fn try_from_value(value: Value<ElementCtx<'a>>) -> Result<Self> {
        let Value::List(v) = value else {
            anyhow::bail!("expected List, got {value:?}")
        };
        Ok(v)
    }
}

impl<'a> TryFromValue<ElementCtx<'a>> for Structure<ElementCtx<'a>> {
    fn try_from_value(value: Value<ElementCtx<'a>>) -> Result<Self> {
        let Value::Structure(s) = value else {
            anyhow::bail!("expected Structure, got {value:?}")
        };
        Ok(s)
    }
}
