use std::{borrow::Cow, env};

use anyhow::Context;
use frontend::Parser;
use interpreter::Value;

pub mod frontend;
pub mod interpreter;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let mut args = env::args();
    // skip name
    let name = args.next().context("usage: csscrape <filename> <url>")?;

    let filename = args
        .next()
        .with_context(|| format!("usage: {name} <filename> <url>"))?;

    let url = args
        .next()
        .with_context(|| format!("usage: {name} <filename = {filename}> <url>"))?;

    let pgm = std::fs::read_to_string(&filename)
        .with_context(|| format!("error reading file {filename}"))?;

    let parser = Parser::new(&pgm);

    let client = reqwest::Client::builder()
        .user_agent(env::var("USER_AGENT").unwrap_or_else(|_| {
            format!("{}v{}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"))
        }))
        .build()
        .context("error building HTTP client")?;
    let cloned = client.clone();

    let parse_fut = async move {
        match parser.parse() {
            Ok(x) => Ok(x),
            Err(e) => anyhow::bail!("Parse error: {e}"),
        }
    };

    let fetch_fut = async move {
        let client = cloned;
        let url = url;
        let req = client
            .get(&url)
            .send()
            .await
            .with_context(|| format!("Error fetching `{url}`"))?;

        let text = req
            .text()
            .await
            .with_context(|| format!("Error getting body text"))?;

        let html = scraper::Html::parse_document(&text);

        Ok::<_, anyhow::Error>(html)
    };

    let ((ast, head), html) = tokio::try_join!(parse_fut, fetch_fut)?;

    let mut htmls = vec![html];

    let results = interpreter::interpret(&mut htmls, &ast, head)?;

    let results = Value::Structure(
        results
            .into_iter()
            .map(|(k, v)| (Cow::Owned(k.into_owned()), v))
            .collect(),
    );

    println!("{results}");

    Ok(())
}
