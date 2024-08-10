use std::{borrow::Cow, collections::BTreeMap, sync::Arc};

use execution_mode::ExecutionMode;
use reqwest::Url;
use scrapelect_filter_types::{
    bail, other, Bindings, EValue, ElementContext, ListIter, PValue, Value,
};

use crate::frontend::{
    ast::{
        self, AstRef, Element, FilterList, Inline, Leaf, Qualifier, RValue, Statement,
        StatementList,
    },
    AstArena,
};

mod execution_mode;
pub mod filter;

pub use scrapelect_filter_types::{Error, MessageExt, Result, WrapExt};

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
    ) -> Result<Bindings<'ast>> {
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
                .with_msg(|| format!("request to `{url}` failed"))?
                .text()
                .await
                .with_msg(|| format!("retrieving body from `{url} failed"))?,
            "file" => tokio::fs::read_to_string(url.path())
                .await
                .with_msg(|| format!("reading from file `{}` failed", url.path()))?,
            other => bail!("unknown URL scheme `{other}`"),
        };

        Ok(scraper::Html::parse_document(&text))
    }

    async fn interpret_block(
        &self,
        element: scraper::ElementRef<'_>,
        statements: Option<AstRef<'ast, StatementList<'ast>>>,
        parent: Option<&ElementContext<'ast, '_>>,
        url: Url,
    ) -> Result<Bindings<'ast>> {
        let mut ctx = ElementContext::new(element, parent, url);

        for statement in self.ast.flatten(statements) {
            self.interpret_statement(&statement.value, &mut ctx).await?;
        }

        Ok(ctx.bindings.into_data())
    }

    async fn interpret_statement(
        &self,
        statement: &Statement<'ast>,
        ctx: &mut ElementContext<'ast, '_>,
    ) -> Result<()> {
        let inner = || async move {
            let value = match &statement.value {
                RValue::Leaf(l) => leaf_to_value(ctx, l)?,
                RValue::Element(e) => Value::from_data(self.interpret_element(e, ctx).await?),
            };

            let value =
                self.apply_filters(value, self.ast.flatten(statement.filters).into_iter(), ctx)?;
            ctx.set(Cow::Borrowed(statement.id), value)?;

            Ok(())
        };

        inner().await.wrap_with(|| {
            format!(
                "note: occurred while evaluating binding `{}`.",
                statement.id
            )
        })
    }

    async fn interpret_element(
        &self,
        element: &Element<'ast>,
        ctx: &mut ElementContext<'ast, '_>,
    ) -> Result<Value> {
        let selector_str = element.to_selector_str(self.ast);
        let selector_str = &selector_str;
        let inner = || async move {
            let html;

            let (root_element, url) = if let Some(url) = &element.url {
                let url: Arc<str> = self.eval_inline(url, ctx)?.try_unwrap()?;
                let url: Url = match url.parse() {
                    Ok(url) => url,
                    Err(url::ParseError::RelativeUrlWithoutBase) => ctx
                        .url
                        .join(&url)
                        .with_msg(|| format!("`{url} is not a valid relative URL"))?,
                    Err(e) => bail!(@e, "`{url}` is not a valid URL"),
                };
                html = self.get_html(&url).await?;
                (html.root_element(), url)
            } else {
                (ctx.element, ctx.url.clone())
            };

            let selector = scraper::Selector::parse(selector_str).map_err(|e| {
                other!(
                    "failed to parse selector `{selector_str}`.
                This is a bug in `scrapelect`, please report it.
                `selectors` error: {e}"
                )
            })?;

            let selection = root_element.select(&selector);

            let element_refs = ExecutionMode::hinted_from_iter(element.qualifier, selection)?;

            let values =
                futures::future::try_join_all(element_refs.into_iter().map(|element_ref| {
                    self.interpret_block(element_ref, element.statements, Some(ctx), url.clone())
                }))
                .await?;

            Ok(ExecutionMode::hinted_from_iter(
                element.qualifier,
                values.into_iter().map(Bindings::into_value),
            )?
            .into_value())
        };

        inner()
            .await
            .wrap_with(|| format!("note: occurred while evaluating element block `{selector_str}`"))
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
                        .map(|arg| Ok((arg.id, self.eval_inline(&arg.value, ctx)?)))
                        .collect::<Result<BTreeMap<_, _>>>()?;
                    qualify(filter.qualifier, value, |value| {
                        filter::dispatch_filter(call.id, value, args.clone(), ctx)
                    })
                }
                ast::Filter::Select(select) => qualify(filter.qualifier, value, |value| {
                    let ls: ListIter = value.try_unwrap()?;

                    let mut inner_scope =
                        ElementContext::new(ctx.element, Some(ctx), ctx.url.clone());

                    Ok(Value::List(
                        ls.map(|value| {
                            let value = EValue::from(value);
                            inner_scope.set(select.name.into(), value.clone())?;

                            let keep: bool = self
                                .eval_inline(&select.value, &mut inner_scope)?
                                .try_unwrap()?;

                            Ok(keep.then(|| value.into()))
                        })
                        .filter_map(Result::transpose)
                        .collect::<Result<_>>()?,
                    ))
                }),
            })
            .map(EValue::from)
    }

    fn eval_inline<'ctx>(
        &self,
        inline: &Inline<'ast>,
        ctx: &mut ElementContext<'ast, 'ctx>,
    ) -> Result<EValue<'ctx>> {
        self.apply_filters(
            leaf_to_value(ctx, &inline.value)?,
            self.ast.flatten(inline.filters).into_iter(),
            ctx,
        )
    }
}

fn qualify<'doc, F>(
    qualifier: Qualifier,
    value: PValue<'doc>,
    mut action: F,
) -> Result<PValue<'doc>>
where
    F: FnMut(PValue<'doc>) -> Result<PValue<'doc>>,
{
    match qualifier {
        Qualifier::One => action(value),
        Qualifier::Optional if matches!(value, Value::Null) => Ok(Value::Null),
        Qualifier::Optional => action(value),
        Qualifier::Collection => value
            .try_unwrap::<ListIter>()?
            .map(action)
            .collect::<Result<Vec<_>>>()
            .map(Value::List),
    }
}

pub fn leaf_to_value<'ast, 'ctx>(
    ctx: &ElementContext<'ast, 'ctx>,
    value: &Leaf<'ast>,
) -> Result<EValue<'ctx>> {
    match value {
        Leaf::Float(x) => Ok(Value::Float(*x)),
        Leaf::Int(n) => Ok(Value::Int(*n)),
        Leaf::String(s) => Ok(Value::String(Arc::from(&**s))),
        Leaf::Var(id) => ctx.get(id),
    }
}

#[cfg(test)]
pub async fn interpret_string_harness(
    program: &'static str,
    html: &'static str,
) -> anyhow::Result<Bindings<'static>> {
    use anyhow::Context;

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
        .context("Error running interpreter")
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
                text: $element | text();

                a: a {
                    child: $element | text();
                    parent: $text;
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
            match d.get("text") {
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
        filter_select,
    }
}
