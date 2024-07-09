use std::{borrow::Cow, cell::OnceCell, collections::BTreeMap};

use anyhow::Context;

use crate::frontend::{
    ast::{AstRef, Element, FilterList, Leaf, RValue, SelectorOpts, Statement, StatementList},
    AstArena,
};

mod filter;
mod value;

pub use filter::Filter;
pub use value::{TryFromValue, Value};

/// Whether we are matching a list, singular item, or optional item
/// as specified by the user
#[derive(Debug)]
enum ExecutionMode<T> {
    One(T),
    Optional(Option<T>),
    Collection(Vec<T>),
}

impl<T> ExecutionMode<T> {
    fn try_map<F, U, E>(self, mut f: F) -> Result<ExecutionMode<U>, E>
    where
        F: FnMut(T) -> Result<U, E>,
    {
        use ExecutionMode::{Collection, One, Optional};

        Ok(match self {
            One(t) => One(f(t)?),
            Optional(Some(t)) => Optional(Some(f(t)?)),
            Optional(None) => Optional(None),
            Collection(vec) => Collection(vec.into_iter().map(f).collect::<Result<_, _>>()?),
        })
    }
}

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

    let mut selection = root.select(&selector);

    let element_refs = match element.ops {
        // TODO: take the first, or fail if there are > 1?
        SelectorOpts::One => ExecutionMode::One(
            selection
                .next()
                .with_context(|| format!("Expected exactly one `{selector_str}`"))?,
        ),
        SelectorOpts::Optional => ExecutionMode::Optional(selection.next()),
        SelectorOpts::Collection => ExecutionMode::Collection(selection.collect()),
    };

    let values = element_refs.try_map(|element_ref| {
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

        Ok::<_, anyhow::Error>(Value::Structure(
            ctx.variables
                .into_iter()
                .map(|(k, v)| (Cow::Owned(k.into_owned()), v))
                .collect(),
        ))
    })?;

    Ok(match values {
        ExecutionMode::One(x) | ExecutionMode::Optional(Some(x)) => x,
        ExecutionMode::Optional(None) => Value::Null,
        ExecutionMode::Collection(l) => Value::List(l),
    })
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
