use std::{borrow::Cow, cell::OnceCell, collections::BTreeMap, sync::Arc};

use anyhow::Context;
use execution_mode::ExecutionMode;
use reqwest::Url;

use value::{EValue, ListIter};

use crate::frontend::{
    ast::{
        self, AstRef, Element, FilterList, Inline, Leaf, Qualifier, RValue, Statement,
        StatementList,
    },
    AstArena,
};

mod execution_mode;
mod filter;
mod value;

pub use filter::Filter;
pub use value::Value;

type Error = anyhow::Error;
type Result<T> = core::result::Result<T, Error>;

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
pub struct Variables<'a, 'b>(pub BTreeMap<Cow<'a, str>, EValue<'b>>);

#[derive(Debug, Default)]
pub struct DataVariables<'a>(pub BTreeMap<Cow<'a, str>, Value>);

impl<'a, 'b> From<Variables<'a, 'b>> for DataVariables<'a> {
    fn from(value: Variables<'a, 'b>) -> Self {
        Self(
            value
                .0
                .into_iter()
                .filter_map(|(k, v)| v.into_data().map(|v| (k, v)))
                .collect(),
        )
    }
}

impl<'a, 'b> From<DataVariables<'a>> for Variables<'a, 'b> {
    fn from(value: DataVariables<'a>) -> Self {
        Self(
            value
                .0
                .into_iter()
                .map(|(k, v)| (k, Value::from_data(v)))
                .collect(),
        )
    }
}

impl<'ast> From<DataVariables<'ast>> for Value {
    fn from(value: DataVariables<'ast>) -> Self {
        Self::Structure(
            value
                .0
                .into_iter()
                .map(|(k, v)| (Arc::from(&*k), v))
                .collect(),
        )
    }
}

#[derive(Debug)]
pub struct ElementContext<'ast, 'ctx> {
    variables: Variables<'ast, 'ctx>,
    element: scraper::ElementRef<'ctx>,
    text: OnceCell<Arc<str>>,
    parent: Option<&'ctx ElementContext<'ast, 'ctx>>,
    url: Url,
}

#[derive(Debug)]
pub struct Interpreter<'ast> {
    client: reqwest::Client,
    ast: &'ast AstArena<'ast>,
}

impl<'ast> Interpreter<'ast> {
    #[must_use]
    #[inline]
    pub fn new(ast: &'ast AstArena<'ast>) -> Self {
        Self::with_client(
            ast,
            reqwest::Client::builder()
                .user_agent(concat!(
                    env!("CARGO_PKG_NAME"),
                    " v",
                    env!("CARGO_PKG_VERSION")
                ))
                .build()
                .expect("Default client is invalid"),
        )
    }

    #[must_use]
    #[inline]
    pub const fn with_client(ast: &'ast AstArena<'ast>, client: reqwest::Client) -> Self {
        Self { ast, client }
    }

    #[inline]
    pub async fn interpret(
        &self,
        root_url: Url,
        head: Option<AstRef<'ast, StatementList<'ast>>>,
    ) -> Result<DataVariables<'ast>> {
        let html = self.get_html(&root_url).await?;
        self.interpret_block(html.root_element(), head, None, root_url)
            .await
    }

    async fn get_html(&self, url: &Url) -> Result<scraper::Html> {
        let text = match url.scheme() {
            "http" | "https" => self
                .client
                .get(url.clone())
                .send()
                .await
                .context("Error sending HTTP request")?
                .text()
                .await
                .context("Error getting HTTP body text")?,
            "file" => tokio::fs::read_to_string(url.path())
                .await
                .with_context(|| format!("Error reading from file `{}`", url.path()))?,
            other => anyhow::bail!("unknown URL scheme `{other}`"),
        };

        Ok(scraper::Html::parse_document(&text))
    }

    async fn interpret_block(
        &self,
        element: scraper::ElementRef<'_>,
        statements: Option<AstRef<'ast, StatementList<'ast>>>,
        parent: Option<&ElementContext<'ast, '_>>,
        url: Url,
    ) -> Result<DataVariables<'ast>> {
        let mut ctx = ElementContext {
            element,
            parent,
            variables: Variables::default(),
            text: OnceCell::new(),
            url,
        };

        for statement in self.ast.flatten(statements) {
            self.interpret_statement(&statement.value, &mut ctx).await?;
        }

        Ok(ctx.variables.into())
    }

    async fn interpret_statement(
        &self,
        statement: &Statement<'ast>,
        ctx: &mut ElementContext<'ast, '_>,
    ) -> Result<()> {
        let value = match &statement.value {
            RValue::Leaf(l) => ctx.leaf_to_value(l)?,
            RValue::Element(e) => Value::from_data(self.interpret_element(e, ctx).await?),
        };

        let value =
            self.apply_filters(value, self.ast.flatten(statement.filters).into_iter(), ctx)?;
        ctx.set_var(Cow::Borrowed(statement.id), value)?;

        Ok(())
    }

    async fn interpret_element(
        &self,
        element: &Element<'ast>,
        ctx: &mut ElementContext<'ast, '_>,
    ) -> anyhow::Result<Value> {
        let html;

        let (root_element, url) = if let Some(url) = &element.url {
            let url: Arc<str> = self.eval_inline(url, ctx)?.try_unwrap()?;
            let url: Url = match url.parse() {
                Ok(url) => url,
                Err(url::ParseError::RelativeUrlWithoutBase) => ctx
                    .url
                    .join(&url)
                    .with_context(|| format!("`{url} is not a valid relative URL"))?,
                Err(e) => anyhow::bail!("`{url}` is not a valid URL: {e}"),
            };
            html = self.get_html(&url).await?;
            (html.root_element(), url)
        } else {
            (ctx.element, ctx.url.clone())
        };

        let selector_str = element.to_selector_str(self.ast);

        let selector = scraper::Selector::parse(&selector_str).map_err(|e| {
            anyhow::anyhow!(
                "Selector parse failed: {e}.  This is a program error. Selector is `{selector_str}`",
            )
        })?;

        let selection = root_element.select(&selector);

        let element_refs = ExecutionMode::hinted_from_iter(element.qualifier, selection)?;

        let values = futures::future::try_join_all(element_refs.into_iter().map(|element_ref| {
            self.interpret_block(element_ref, element.statements, Some(ctx), url.clone())
        }))
        .await?;

        Ok(
            ExecutionMode::hinted_from_iter(
                element.qualifier,
                values.into_iter().map(Value::from),
            )?
            .into_value(),
        )
    }

    fn apply_filters<'ctx>(
        &self,
        value: EValue<'ctx>,
        mut filters: impl Iterator<Item = &'ast FilterList<'ast>>,
        ctx: &mut ElementContext<'ast, 'ctx>,
    ) -> Result<EValue<'ctx>> {
        filters
            .try_fold(value.into(), |value, filter| match &filter.filter {
                ast::Filter::Call(call) => {
                    let args = self
                        .ast
                        .flatten(call.args)
                        .into_iter()
                        .map(|arg| Ok((arg.id, ctx.leaf_to_value(&arg.value)?)))
                        .collect::<Result<BTreeMap<_, _>>>()?;

                    match filter.qualifier {
                        Qualifier::One => filter::dispatch_filter(call.id, value, args, ctx),
                        Qualifier::Optional if matches!(value, Value::Null) => Ok(Value::Null),
                        Qualifier::Optional => filter::dispatch_filter(call.id, value, args, ctx),
                        Qualifier::Collection => value
                            .try_unwrap::<ListIter>()?
                            .map(|value| filter::dispatch_filter(call.id, value, args.clone(), ctx))
                            .collect::<Result<Vec<_>>>()
                            .map(Value::List),
                    }
                }
                ast::Filter::Select(_select) => {
                    todo!()
                }
            })
            .map(EValue::from)
    }

    fn eval_inline<'ctx>(
        &self,
        inline: &Inline<'ast>,
        ctx: &mut ElementContext<'ast, 'ctx>,
    ) -> Result<EValue<'ctx>> {
        self.apply_filters(
            ctx.leaf_to_value(&inline.value)?,
            self.ast.flatten(inline.filters).into_iter(),
            ctx,
        )
    }
}

impl<'ast, 'ctx> ElementContext<'ast, 'ctx> {
    pub fn get_var(&self, id: &str) -> anyhow::Result<EValue<'ctx>> {
        match id {
            "element" => Ok(self.element.into()),
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

    pub fn set_var(&mut self, name: Cow<'ast, str>, value: EValue<'ctx>) -> anyhow::Result<()> {
        match &*name {
            immutable @ ("element" | "text") => {
                anyhow::bail!("Can't assign to immutable variable `{immutable}`")
            }
            _ => self.variables.0.insert(name, value),
        };

        Ok(())
    }

    pub fn leaf_to_value(&self, value: &Leaf<'ast>) -> anyhow::Result<EValue<'ctx>> {
        match value {
            Leaf::Float(x) => Ok(Value::Float(*x)),
            Leaf::Int(n) => Ok(Value::Int(*n)),
            Leaf::String(s) => Ok(Value::String(Arc::from(&**s))),
            Leaf::Var(id) => self.get_var(id),
        }
    }
}

#[cfg(test)]
pub async fn interpret_string_harness(
    program: &'static str,
    html: &'static str,
) -> Result<DataVariables<'static>> {
    let (ast, head) = crate::frontend::Parser::new(program).parse()?;
    let html = scraper::Html::parse_document(html);
    let interpreter = Interpreter::new(Box::leak(Box::new(ast)));
    interpreter
        // TODO: url hack
        .interpret_block(
            html.root_element(),
            head,
            None,
            "file:///tmp/inmemory.html".parse().expect("URL parse"),
        )
        .await
}

#[cfg(test)]
mod tests {
    use super::Value::*;

    async fn integration_test(filename: &str) -> anyhow::Result<()> {
        let input = std::fs::read_to_string(format!("examples/inputs/{filename}.html"))?;
        let script = std::fs::read_to_string(format!("examples/scrps/{filename}.scrp"))?;

        let (ast, head) = crate::frontend::Parser::new(&script)
            .parse()
            .expect("parse error");

        let html = scraper::Html::parse_document(&input);

        let result = super::Interpreter::new(&ast)
            .interpret_block(
                html.root_element(),
                head,
                None,
                format!(
                    "file://{}/examples/inputs/{}",
                    std::env::current_dir().expect("get current dir").display(),
                    filename,
                )
                .parse()
                .expect("parse URL failed"),
            )
            .await?
            .0;

        let mut settings = insta::Settings::clone_current();
        settings.set_snapshot_path("../../examples/outputs");
        settings.set_prepend_module_to_snapshot(false);

        settings.bind(|| {
            insta::assert_json_snapshot!(filename, result);
        });

        Ok(())
    }

    macro_rules! integration_test {
        {
            $($name: ident,)*
        } => {
            $(
                #[tokio::test]
                async fn $name() -> anyhow::Result<()> {
                    integration_test(stringify!($name)).await
                }
            )*
        };
    }

    #[tokio::test]
    async fn test_basic() {
        let output = super::interpret_string_harness(
            r#"
            h3: h3 {
                txt: $text;

                a: a {
                    child: $text;
                    parent: $txt;
                }*;
                div: div {}?;
            };
            "#,
            r#"
                <html>
                    <h3>Hello,<a>Hello, child</a><span></span>parent!</h3>
                </html>
            "#,
        )
        .await
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
        qualifiers,
        relative,
        recurser,
    }
}
