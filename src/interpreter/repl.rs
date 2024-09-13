//! The `scrapelect` read-evaluate-print-loop interpreter.
//!
//! To run the loop, see [`Repl::rep`] and [`Repl::repl`].  This module is primarily
//! for use in the `scrapelect` binary, but

use std::{
    borrow::Cow,
    io::{self, BufRead, StdinLock, StdoutLock, Write},
    iter::FusedIterator,
    sync::Arc,
};

use anyhow::Context as _;
use scrapelect_filter_types::{
    other, Bindings, EValue, Element, ElementContext, ElementContextView, Value,
};
use scraper::{ElementRef, Html};
use url::Url;

use crate::frontend::{
    ast::{self, Selector},
    Parser, Token,
};

use super::Interpreter;

pub enum Nested<T, I> {
    One(Option<T>),
    Nested(I),
}

impl<T, I: Iterator<Item = T>> Iterator for Nested<T, I> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::One(o) => o.take(),
            Self::Nested(n) => n.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::One(o) => o.iter().size_hint(),
            Self::Nested(n) => n.size_hint(),
        }
    }
}

impl<T, I: Iterator<Item = T> + FusedIterator> FusedIterator for Nested<T, I> {}

#[derive(Debug)]
struct Context<'a> {
    bindings: Bindings<'static, Element<'a>>,
    element: ElementRef<'a>,
    url: Url,
    selector: Option<String>,
}

#[non_exhaustive]
#[derive(Debug)]
pub struct Repl<R = StdinLock<'static>, W = StdoutLock<'static>> {
    client: reqwest::Client,
    input: R,
    output: W,
}

impl Repl {
    /// Creates a new empty [`Repl`] instance using stdin and stdout.
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self {
            client: reqwest::Client::builder()
                .user_agent(concat!(
                    env!("CARGO_PKG_NAME"),
                    " v",
                    env!("CARGO_PKG_VERSION")
                ))
                .build()
                .expect("Default client is invalid"),
            input: io::stdin().lock(),
            output: io::stdout().lock(),
        }
    }

    /// Creates a new [`Repl`] with the given url loaded and opened.
    pub async fn open(url: Url) -> anyhow::Result<Self> {
        let mut this = Self::new();
        let interpreter = Interpreter::with_client(this.client.clone());
        let document = interpreter.get_html(&url).await?;

        todo!();

        Ok(this)
    }
}

macro_rules! output {
    ($self: expr, $($tt:tt)*) => {
        write!(&mut $self.output, $($tt)*)
    };
}

macro_rules! outputln {
    ($self: expr$(, $($tt:tt)*)?) => {
        writeln!(&mut $self.output, $($($tt)*)?)
    };
}

impl<R: BufRead, W: Write> Repl<R, W> {
    /// Read a line from the input into `buf`.  Returns `Ok(true)` if
    /// a string was added and `Ok(false)` if EOF was reached.
    ///
    /// # Errors
    ///
    /// Returns an `Err` when an error occured while reading the line.
    fn get_line(&mut self, buf: &mut String, ctx: Option<&Context<'_>>) -> anyhow::Result<bool> {
        if let Some(ctx) = ctx {
            if let Some(selector) = &ctx.selector {
                output!(self, "{selector}")?;
            } else {
                output!(self, "{}", ctx.url)?;
            }
        }
        output!(self, "> ")?;
        self.output.flush()?;

        buf.clear();
        while !buf.ends_with('\n') {
            let len = self.input.read_line(buf)?;
            if len == 0 {
                return Ok(false);
            }
        }

        // remove newline
        buf.pop();

        Ok(true)
    }

    async fn handle_command(
        &mut self,
        interpreter: &Interpreter,
        command: &str,
    ) -> anyhow::Result<bool> {
        let mut parser = Parser::new(command);

        match parser.try_eat(Token::Id)?.value {
            "exit" | "quit" => return Ok(false),
            "open" => {
                let url = Parser::parse_string_literal(parser.try_eat(Token::String)?.value);
                let url = url
                    .parse()
                    .map_err(|e| anyhow::anyhow!("Invalid URL `{url}`: {e}"))?;

                let document = interpreter.get_html(&url).await?;

                self.stack.push(ContextNode::new(
                    Arc::new(ElementArc::new(
                        Arc::new(ElementParent::Document { document, url }),
                        |doc| {
                            let ElementParent::Document { document, .. } = &**doc else {
                                unreachable!(
                                    "expected a document parent at {}:{}:{}",
                                    file!(),
                                    line!(),
                                    column!()
                                )
                            };

                            document.root_element()
                        },
                    )),
                    |_| Bindings::new(),
                ));
            }
            "leave" => match self.stack.pop() {
                Some(mut node) => {
                    let bindings =
                        node.with_bindings_mut(|b| std::mem::take(b).into_data().into_value());

                    if let ElementParent::Element {
                        name: Some(name), ..
                    } = &**node.borrow_element().borrow_parent()
                    {
                        if let Some(last) = self.stack.last_mut() {
                            outputln!(self, "{name}: {bindings}")?;
                            last.with_bindings_mut(|b| {
                                b.0.insert(Cow::Owned(name.clone()), Value::from_data(bindings))
                            });
                        }
                    }
                }
                None => return Ok(false),
            },
            "enter" => {
                let name = parser.try_eat(Token::Id)?.value;

                let selector = parser.parse_selector()?;
                self.select(&selector, Some(name.to_owned()))?;
            }
            "help" => {
                outputln!(self,
                    "Available commands:\n\n\
                    - /help: display this help message\n\
                    - /open <url: String>: open a new web page at `url` and select the root element.\n\
                    - /enter <name> <selector>: enter a multiline element context block `name: selector {{...}}`\n\
                    - /current: print the current URL and selector\n\
                    - /leave: leave the current element context\n\
                    - /quit | /exit: exit the REPL\n\
                    "
                )?;
            }
            "current" => {
                for item in &self.stack {
                    match &**item.borrow_element().borrow_parent() {
                        ElementParent::Document { url, .. } => output!(self, "\non {url}:")?,
                        ElementParent::Element { selector, .. } => output!(self, " {selector}")?,
                    }
                }
                outputln!(self)?;
            }
            "eval" => {
                let inline = parser.parse_value()?;
                let mut ctx = Context::new_as_error(&self.stack)?;

                let value = interpreter.eval_inline(&inline, &mut ctx)?;
                outputln!(self, "{value}")?;
                let ledger = into_data(ctx);
                self.pop_off(ledger)?;
            }
            unknown => anyhow::bail!(
                "Unknown command `/{unknown}`.\n\
                Run `/help` for a list of commands."
            ),
        }

        Ok(true)
    }

    pub async fn repl(mut self) -> anyhow::Result<()> {
        let interpreter = Interpreter::with_client(self.client.clone());

        loop {
            let result = self.rep(&interpreter).await;

            match result {
                Ok(true) => (),
                Ok(false) => break,
                Err(e) => eprintln!("Error: {e}"),
            }
        }

        outputln!(self, "Exiting...")?;
        Ok(())
    }

    pub async fn rep(&mut self, interpreter: &Interpreter) -> anyhow::Result<bool> {
        let mut buf = String::new();
        if !self.get_line(&mut buf)? {
            return Ok(false);
        }

        let input = buf.trim();

        if let Some(command) = input.strip_prefix("/") {
            self.handle_command(interpreter, command).await
        } else {
            let mut ctx = Context::new_as_error(&self.stack)?;
            let mut parser = Parser::new(input);
            let statement = parser.parse_statement()?;

            interpreter
                .interpret_statement(&statement, &mut ctx)
                .await?;

            let ledger = into_data(ctx);
            self.pop_off(ledger)?;

            Ok(true)
        }
    }

    pub fn select(
        &mut self,
        selector: &ast::Selector<'_>,
        name: Option<String>,
    ) -> anyhow::Result<()> {
        let ctx = self.stack.last().context(
            "You do not have a document open.\n\
            Call `/open <url: String>` to load a document from a URL.",
        )?;

        let new_element = ElementArc::try_new(
            Arc::new(ElementParent::Element {
                parent: ctx.borrow_element().clone(),
                selector: selector.to_string(),
                name,
            }),
            |e| {
                let ElementParent::Element { parent, .. } = &**e else {
                    unreachable!(
                        "expected an `Element` variant at {}:{}:{}",
                        file!(),
                        line!(),
                        column!()
                    );
                };
                parent
                    .borrow_element()
                    .select(&selector.to_scraper())
                    .next()
                    .with_context(|| format!("no element found with selector `{selector}`"))
            },
        )?;

        self.stack
            .push(ContextNode::new(Arc::new(new_element), |_| Bindings::new()));
        Ok(())
    }

    fn pop_off(&mut self, ledger: Bindings<'static, Element<'static>>) -> anyhow::Result<()> {
        let last_mut = self
            .stack
            .last_mut()
            .context("Expected to be in an element block")?;

        for (name, value) in ledger.0 {
            outputln!(self, "{name}: {value}")?;
            last_mut.with_bindings_mut(|b| b.0.insert(name, value));
        }

        Ok(())
    }
}

// TODO: make this not drop elements with lifetimes.
fn into_data(ctx: Context<'_>) -> Bindings<'static, Element<'static>> {
    for (k, v) in &ctx.ledger.0 {
        if let Value::Extra(e) = v {
            eprintln!(
                "Binding {k}: {e} contains a temporary element reference, which is currently not supported in the REPL.\n\
                It will be 'forgotten' and not available in the subsequent lines."
            );
        }
    }

    Bindings::from_data(ctx.ledger.into_data())
}
