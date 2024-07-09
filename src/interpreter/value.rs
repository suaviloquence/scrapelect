use std::{borrow::Cow, collections::BTreeMap, fmt};

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
        String(Cow<'a, str>),
        Element(scraper::ElementRef<'a>),
        List(Vec<Value<'a>>),
        Structure(BTreeMap<Cow<'a, str>, Value<'a>>),
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

impl<'a> From<&'a str> for Value<'a> {
    fn from(value: &'a str) -> Self {
        Self::String(Cow::Borrowed(value))
    }
}

impl<'a> From<scraper::ElementRef<'a>> for Value<'a> {
    fn from(value: scraper::ElementRef<'a>) -> Self {
        Self::Element(value)
    }
}
