use std::{borrow::Cow, cell::OnceCell, collections::BTreeMap, sync::Arc};

use anyhow::Context;

use crate::frontend::{
    ast::{AstRef, Element, FilterList, Leaf, RValue, SelectorOpts, Statement, StatementList},
    AstArena,
};

mod filter;
mod value;

pub use filter::Filter;
pub use value::{DataValue, TryFromValue, Value};

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

#[derive(Debug, Default)]
pub struct Variables<'a, 'b>(pub BTreeMap<Cow<'a, str>, Value<'b>>);

#[derive(Debug, Default)]
pub struct DataVariables<'a>(pub BTreeMap<Cow<'a, str>, DataValue>);

impl<'a, 'b> From<Variables<'a, 'b>> for DataVariables<'a> {
    fn from(value: Variables<'a, 'b>) -> Self {
        Self(
            value
                .0
                .into_iter()
                .filter_map(|(k, v)| DataValue::try_from(v).ok().map(|v| (k, v)))
                .collect(),
        )
    }
}

impl<'a, 'b> From<DataVariables<'a>> for Variables<'a, 'b> {
    fn from(value: DataVariables<'a>) -> Self {
        Self(value.0.into_iter().map(|(k, v)| (k, v.into())).collect())
    }
}

#[derive(Debug)]
pub struct ElementContext<'ast, 'ctx> {
    variables: Variables<'ast, 'ctx>,
    element: scraper::ElementRef<'ctx>,
    text: OnceCell<Arc<str>>,
    parent: Option<&'ctx ElementContext<'ast, 'ctx>>,
}

pub fn interpret<'ast>(
    html: scraper::Html,
    ast: &'ast AstArena<'ast>,
    head: Option<AstRef<'ast, StatementList<'ast>>>,
) -> anyhow::Result<DataVariables<'ast>> {
    let mut ctx = ElementContext {
        element: html.root_element(),
        variables: Variables::default(),
        text: OnceCell::new(),
        parent: None,
    };

    for statement in ast.flatten(head) {
        interpret_statement(ast, &statement.value, &mut ctx)?;
    }

    Ok(ctx.variables.into())
}

fn interpret_statement<'ast, 'stmt, 'ctx: 'stmt>(
    ast: &'ast AstArena<'ast>,
    statement: &Statement<'ast>,
    ctx: &'stmt mut ElementContext<'ast, 'ctx>,
) -> anyhow::Result<()> {
    match statement.id {
        immutable @ ("document" | "text") => {
            anyhow::bail!("can't assign to immutable variable `{immutable}`")
        }
        id => {
            let value = match &statement.value {
                RValue::Leaf(l) => ctx.leaf_to_value(l)?,
                RValue::Element(e) => interpret_element(ctx, ctx.element, ast, e)?.into(),
            };

            let value = apply_filters(ast.flatten(statement.filters).into_iter(), value, ctx, ast)?;
            ctx.variables.0.insert(Cow::Borrowed(id), value);
        }
    }

    Ok(())
}

fn interpret_element<'ast, 'ctx>(
    parent: &'ctx ElementContext<'ast, 'ctx>,
    root: scraper::ElementRef<'ctx>,
    ast: &'ast AstArena<'ast>,
    element: &Element<'ast>,
) -> anyhow::Result<DataValue> {
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
            variables: Variables::default(),
            text: OnceCell::new(),
            parent: Some(parent),
        };

        for statement in ast.flatten(element.statements) {
            let statement = &statement.value;
            interpret_statement(ast, statement, &mut ctx)?;
        }

        Ok::<_, anyhow::Error>(DataValue::Structure(
            ctx.variables
                .0
                .into_iter()
                .filter_map(|(k, v)| DataValue::try_from(v).ok().map(|v| (Arc::from(&*k), v)))
                .collect(),
        ))
    })?;

    Ok(match values {
        ExecutionMode::One(x) | ExecutionMode::Optional(Some(x)) => x,
        ExecutionMode::Optional(None) => DataValue::Null,
        ExecutionMode::Collection(l) => DataValue::List(l),
    })
}

impl<'ast, 'ctx> ElementContext<'ast, 'ctx> {
    pub fn get_var(&self, id: &str) -> anyhow::Result<Value<'ctx>> {
        match id {
            "element" => Ok(Value::Element(self.element)),
            "text" => Ok(Value::String(Arc::clone(self.text.get_or_init(|| {
                self.element
                    .children()
                    .filter_map(|x| x.value().as_text().map(|x| &**x))
                    .collect::<String>()
                    .into()
            })))),
            var => match self.variables.0.get(var) {
                Some(var) => Ok(var.clone()),
                None => self
                    .parent
                    .with_context(|| format!("Unknown variable `{var}`"))?
                    .get_var(id),
            },
        }
    }

    pub fn set_var(&mut self, name: Cow<'ast, str>, value: Value<'ctx>) -> anyhow::Result<()> {
        match &*name {
            immutable @ ("element" | "text") => {
                anyhow::bail!("Can't assign to immutable variable `{immutable}`")
            }
            _ => self.variables.0.insert(name, value),
        };

        Ok(())
    }

    pub fn leaf_to_value(&self, value: &Leaf<'ast>) -> anyhow::Result<Value<'ctx>> {
        match value {
            Leaf::Float(x) => Ok(Value::Float(*x)),
            Leaf::Int(n) => Ok(Value::Int(*n)),
            Leaf::String(s) => Ok(Value::String(Arc::from(&**s))),
            Leaf::Var(id) => self.get_var(id),
        }
    }
}

fn apply_filters<'ast, 'ctx>(
    filters: impl Iterator<Item = &'ast FilterList<'ast>>,
    value: Value<'ctx>,
    ctx: &mut ElementContext<'ast, 'ctx>,
    ast: &AstArena<'ast>,
) -> anyhow::Result<Value<'ctx>> {
    let mut value = value;
    for f in filters {
        let args: BTreeMap<_, _> = ast
            .flatten(f.args)
            .into_iter()
            .map(|a| {
                let value = match &a.value {
                    Leaf::Var(id) => ctx.get_var(id)?,
                    Leaf::Int(n) => Value::Int(*n),
                    Leaf::String(c) => Value::String(Arc::from(&**c)),
                    Leaf::Float(f) => Value::Float(*f),
                };

                Ok((a.id, value))
            })
            .collect::<anyhow::Result<_>>()?;

        value = filter::dispatch_filter(f.id, value, args, ctx)?;
    }
    Ok(value)
}

#[cfg(test)]
pub fn interpret_string_harness(
    program: &'static str,
    html: &'static str,
) -> anyhow::Result<DataVariables<'static>> {
    let (ast, head) = crate::frontend::Parser::new(program).parse()?;
    let html = scraper::Html::parse_document(html);
    interpret(html, Box::leak(Box::new(ast)), head)
}

#[cfg(test)]
mod tests {
    use super::{interpret, DataValue::*};

    fn integration_test(filename: &str) -> anyhow::Result<()> {
        let input = std::fs::read_to_string(format!("examples/inputs/{filename}.html"))?;
        let script = std::fs::read_to_string(format!("examples/scrps/{filename}.scrp"))?;
        let output: serde_json::Value = serde_json::from_reader(std::fs::File::open(format!(
            "examples/outputs/{filename}.json"
        ))?)?;

        let (ast, head) = crate::frontend::Parser::new(&script)
            .parse()
            .expect("parse error");
        let html = scraper::Html::parse_document(&input);

        let result = interpret(html, &ast, head)?;
        let result = serde_json::to_value(result.0)?;
        assert_eq!(output, result);

        Ok(())
    }

    macro_rules! integration_test {
        {
            $($name: ident,)*
        } => {
            $(
                #[test]
                fn $name() -> anyhow::Result<()> {
                    integration_test(stringify!($name))
                }
            )*
        };
    }

    #[test]
    fn test_basic() {
        let output = super::interpret_string_harness(
            r#"
            h3: h3 {
                txt: $text;

                a: a[] {
                    child: $text;
                    parent: $txt;
                };
                div: div? {};
            };
            "#,
            r#"
                <html>
                    <h3>Hello,<a>Hello, child</a><span></span>parent!</h3>
                </html>
            "#,
        )
        .expect("parsing and interpreting should succeed");

        let Some(Structure(d)) = output.0.get("h3") else {
            panic!("got {output:?}, expected h3: {{ .. }}")
        };

        let Some(List(a)) = d.get("a") else {
            panic!("got {output:?}, expected a: [ .. ]");
        };

        let Some(Structure(a)) = a.get(0) else {
            panic!("got {output:?}");
        };

        assert!(
            match a.get("parent") {
                Some(String(x)) => &**x == "Hello,parent!",
                _ => false,
            },
            "got {output:?}"
        );

        assert!(
            match a.get("child") {
                Some(String(x)) => &**x == "Hello, child",
                _ => false,
            },
            "got {output:?}"
        );

        assert!(
            match d.get("txt") {
                Some(String(x)) => &**x == "Hello,parent!",
                _ => false,
            },
            "got {output:?}"
        );
    }

    integration_test! {
        abc,
        attr,
    }
}
