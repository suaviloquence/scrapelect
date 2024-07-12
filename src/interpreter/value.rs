use std::{collections::BTreeMap, fmt, sync::Arc};

use serde::Serialize;

macro_rules! mk_value {
    (
        $(#[$meta:meta])*
        $vis:vis enum $name:ident<'a> {
            $(
                $(#[$fmeta:meta])*
                $fname:ident $(($ty:ty))?,
            )*
        }
    ) => {
        $(#[$meta])*
        $vis enum $name<'a> {
            $(
                $(#[$fmeta])*
                $fname $(($ty))?,
            )*
        }

        $(
            $(
                impl<'a> TryFromValue<'a> for $ty {
                    fn try_from_value(value: $name<'a>) -> anyhow::Result<Self> {
                        match value {
                            $name::$fname(x) => Ok(x),
                            other => anyhow::bail!("Expected `{}`, found {}", stringify!($fname), other),
                        }
                    }
                }
            )?
        )*
    };
}

pub trait TryFromValue<'a>: Sized {
    fn try_from_value(value: Value<'a>) -> anyhow::Result<Self>;

    fn try_from_option_value(value: Option<Value<'a>>) -> anyhow::Result<Self> {
        match value {
            Some(x) => Self::try_from_value(x),
            None => anyhow::bail!("Expected a value, found `None`"),
        }
    }
}

mk_value! {
    #[derive(Debug, Clone)]
    pub enum Value<'a> {
        Null,
        Float(f64),
        Int(i64),
        String(Arc<str>),
        Element(scraper::ElementRef<'a>),
        List(Vec<Value<'a>>),
        Structure(BTreeMap<Arc<str>, Value<'a>>),
    }
}

#[derive(Debug, Clone, serde::Serialize)]
#[serde(untagged)]
pub enum DataValue {
    #[serde(serialize_with = "serialize_null_as_option")]
    Null,
    Float(f64),
    Int(i64),
    String(Arc<str>),
    List(Vec<DataValue>),
    Structure(BTreeMap<Arc<str>, DataValue>),
}

#[inline]
fn serialize_null_as_option<S: serde::Serializer>(se: S) -> Result<S::Ok, S::Error> {
    None::<()>.serialize(se)
}

impl<'a> TryFrom<Value<'a>> for DataValue {
    type Error = scraper::ElementRef<'a>;

    fn try_from(value: Value<'a>) -> Result<Self, Self::Error> {
        match value {
            Value::Null => Ok(DataValue::Null),
            Value::Float(x) => Ok(DataValue::Float(x)),
            Value::Int(n) => Ok(DataValue::Int(n)),
            Value::String(s) => Ok(DataValue::String(s)),
            Value::Element(e) => Err(e),
            // TODO: just skip?
            Value::List(l) => Ok(DataValue::List(
                l.into_iter()
                    .filter_map(|x| Self::try_from(x).ok())
                    .collect(),
            )),
            Value::Structure(s) => Ok(DataValue::Structure(
                s.into_iter()
                    .filter_map(|(k, v)| Self::try_from(v).ok().map(|v| (k, v)))
                    .collect(),
            )),
        }
    }
}

impl<'a> From<DataValue> for Value<'a> {
    fn from(value: DataValue) -> Self {
        use Value::*;
        match value {
            DataValue::Null => Null,
            DataValue::Float(x) => Float(x),
            DataValue::Int(n) => Int(n),
            DataValue::String(s) => String(s),
            DataValue::List(l) => List(l.into_iter().map(Self::from).collect()),
            DataValue::Structure(s) => {
                Structure(s.into_iter().map(|(k, v)| (k, Self::from(v))).collect())
            }
        }
    }
}

impl<'a> TryFromValue<'a> for Value<'a> {
    #[inline]
    fn try_from_value(value: Value<'a>) -> anyhow::Result<Self> {
        Ok(value)
    }
}

impl<'a> Value<'a> {
    pub fn try_into<T: TryFromValue<'a>>(self) -> anyhow::Result<T> {
        T::try_from_value(self)
    }
}

impl<'a, T: TryFromValue<'a>> TryFromValue<'a> for Option<T> {
    fn try_from_value(value: Value<'a>) -> anyhow::Result<Self> {
        Ok(Some(T::try_from_value(value)?))
    }

    fn try_from_option_value(value: Option<Value<'a>>) -> anyhow::Result<Self> {
        match value {
            Some(x) => Self::try_from_value(x),
            None => Ok(None),
        }
    }
}

impl fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::Int(n) => write!(f, "{n}"),
            Self::Float(x) => write!(f, "{x}"),
            Self::String(s) => write!(f, r#""{s}""#),
            Self::Element(e) => write!(f, "`{}`", e.html()),
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
        }
    }
}

impl fmt::Display for DataValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::Int(n) => write!(f, "{n}"),
            Self::Float(x) => write!(f, "{x}"),
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
        }
    }
}

impl<'a, T> From<Option<T>> for Value<'a>
where
    T: Into<Value<'a>>,
{
    fn from(value: Option<T>) -> Self {
        match value {
            None => Self::Null,
            Some(x) => x.into(),
        }
    }
}

impl<'a, 'b> From<&'b str> for Value<'a> {
    fn from(value: &'b str) -> Self {
        Self::String(Arc::from(value))
    }
}

impl<'a> From<scraper::ElementRef<'a>> for Value<'a> {
    fn from(value: scraper::ElementRef<'a>) -> Self {
        Self::Element(value)
    }
}

#[derive(Debug, Clone)]
pub enum Or<A, B> {
    A(A),
    B(B),
}

impl<'a, A: TryFromValue<'a>, B: TryFromValue<'a>> TryFromValue<'a> for Or<A, B> {
    fn try_from_value(value: Value<'a>) -> anyhow::Result<Self> {
        match value.clone().try_into() {
            Ok(a) => Ok(Self::A(a)),
            Err(a) => match value.try_into() {
                Ok(b) => Ok(Self::B(b)),
                Err(b) => anyhow::bail!("Error: {a} or {b}"),
            },
        }
    }
}
