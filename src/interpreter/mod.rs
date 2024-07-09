use std::{borrow::Cow, cell::OnceCell, collections::BTreeMap, fmt};

use anyhow::Context;

use crate::frontend::{
    ast::{AstRef, Element, FilterList, Leaf, RValue, Statement, StatementList},
    AstArena,
};

mod filter;
pub use filter::Filter;

impl<'ast> Element<'ast> {
    #[must_use]
    pub fn to_selector_str(&self, ast: &AstArena<'ast>) -> String {
        use std::fmt::Write as _;

        let mut buf = String::new();
        let _ = write!(&mut buf, "{}", self.selector_head);

        for selector in ast.flatten(self.selectors) {
            let _ = write!(&mut buf, "{}", selector.sel);
        }

        buf
    }
}

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

#[derive(Debug)]
pub struct ElementContext<'ast, 'doc, 'ctx> {
    variables: BTreeMap<Cow<'ast, str>, Value<'doc>>,
    element: scraper::ElementRef<'doc>,
    text: OnceCell<Cow<'doc, str>>,
    parent: Option<&'ctx ElementContext<'ast, 'doc, 'ctx>>,
}

pub fn interpret<'ast, 'doc>(
    htmls: &'doc mut Vec<scraper::Html>,
    ast: &'ast AstArena<'ast>,
    head: Option<AstRef<'ast, StatementList<'ast>>>,
) -> anyhow::Result<BTreeMap<Cow<'ast, str>, Value<'doc>>> {
    let html = htmls.first().expect("Improper call of `interpret()`");
    let mut ctx = ElementContext {
        element: html.root_element(),
        variables: BTreeMap::new(),
        text: OnceCell::new(),
        parent: None,
    };

    for statement in ast.flatten(head) {
        interpret_statement(ast, &statement.value, &mut ctx)?;
    }

    Ok(ctx.variables)
}

fn interpret_statement<'ast>(
    ast: &'ast AstArena<'ast>,
    statement: &Statement<'ast>,
    ctx: &mut ElementContext<'ast, '_, '_>,
) -> anyhow::Result<()> {
    match statement.id {
        immutable @ ("document" | "text") => {
            anyhow::bail!("can't assign to immutable variable `{immutable}`")
        }
        id => {
            let value = match &statement.value {
                RValue::Leaf(l) => ctx.leaf_to_value(l)?,
                RValue::Element(e) => interpret_element(ctx, ctx.element, ast, e)?,
            };

            let value = apply_filters(ast.flatten(statement.filters).into_iter(), value, ctx, ast)?;
            ctx.variables.insert(Cow::Borrowed(id), value);
        }
    }

    Ok(())
}

fn interpret_element<'ast, 'doc, 'ctx>(
    parent: &'ctx ElementContext<'ast, 'doc, 'ctx>,
    root: scraper::ElementRef<'doc>,
    ast: &AstArena<'ast>,
    element: &Element<'ast>,
) -> anyhow::Result<Value<'doc>> {
    let selector_str = element.to_selector_str(ast);

    let selector = scraper::Selector::parse(&selector_str).map_err(|e| {
        anyhow::anyhow!(
            "Selector parse failed: {e}.  This is a program error. Selector is `{selector_str}`",
        )
    })?;

    // TODO: multiple/optional
    let element_ref = root
        .select(&selector)
        .next()
        .with_context(|| format!("Expected exactly one `{selector_str}`"))?;

    let mut ctx = ElementContext {
        element: element_ref,
        variables: BTreeMap::new(),
        text: OnceCell::new(),
        parent: Some(parent),
    };

    for statement in ast.flatten(element.statements) {
        let statement = &statement.value;
        interpret_statement(ast, statement, &mut ctx)?;
    }

    Ok(Value::Structure(
        ctx.variables
            .into_iter()
            .map(|(k, v)| (Cow::Owned(k.into_owned()), v))
            .collect(),
    ))
}

impl<'ast, 'doc, 'ctx> ElementContext<'ast, 'doc, 'ctx> {
    pub fn get_var(&self, id: &'ast str) -> anyhow::Result<Value<'doc>> {
        match id {
            "element" => Ok(Value::Element(self.element)),
            "text" => Ok(Value::String(Cow::clone(self.text.get_or_init(|| {
                self.element
                    .children()
                    .filter_map(|x| x.value().as_text().map(|x| &**x))
                    .collect()
            })))),
            var => match self.variables.get(var) {
                Some(var) => Ok(var.clone()),
                None => self
                    .parent
                    .with_context(|| format!("Unknown variable `{var}`"))?
                    .get_var(id),
            },
        }
    }

    pub fn leaf_to_value(&self, value: &Leaf<'ast>) -> anyhow::Result<Value<'doc>> {
        match value {
            Leaf::Float(x) => Ok(Value::Float(*x)),
            Leaf::Int(n) => Ok(Value::Int(*n)),
            Leaf::String(s) => Ok(Value::String(Cow::Owned(s.clone().into_owned()))),
            Leaf::Var(id) => self.get_var(id),
        }
    }
}

fn apply_filters<'ast, 'doc, 'ctx>(
    filters: impl Iterator<Item = &'ast FilterList<'ast>>,
    value: Value<'doc>,
    ctx: &mut ElementContext<'ast, 'doc, 'ctx>,
    ast: &AstArena<'ast>,
) -> anyhow::Result<Value<'doc>> {
    let mut value = value;
    for f in filters {
        let args: BTreeMap<_, _> = ast
            .flatten(f.args)
            .into_iter()
            .map(|a| {
                let value = match &a.value {
                    Leaf::Var(id) => ctx.get_var(id)?,
                    Leaf::Int(n) => Value::Int(*n),
                    Leaf::String(Cow::Owned(s)) => Value::String(Cow::Owned(s.clone())),
                    Leaf::String(Cow::Borrowed(s)) => Value::String(Cow::Owned(String::from(*s))),
                    Leaf::Float(f) => Value::Float(*f),
                };

                Ok((a.id, value))
            })
            .collect::<anyhow::Result<_>>()?;

        value = filter::dispatch_filter(f.id, value, args, ctx)?;
    }
    Ok(value)
}
