#![forbid(unsafe_code)]
use std::env;

use anyhow::Context;
use scrapelect::{frontend::Parser, interpreter::Interpreter};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let mut args = env::args();
    // skip name
    let name = args.next().context("usage: scrapelect <filename> <url>")?;

    let filename = args
        .next()
        .with_context(|| format!("usage: {name} <filename> <url>"))?;

    let url = args
        .next()
        .with_context(|| format!("usage: {name} <filename = {filename}> <url>"))?;

    let pgm = std::fs::read_to_string(&filename)
        .with_context(|| format!("error reading file {filename}"))?;

    let parser = Parser::new(&pgm);

    let ast = parser
        .parse()
        .with_context(|| format!("parse error in {filename}:"))?;

    let interpreter = Interpreter::new(&ast);

    let results = interpreter
        .interpret(
            url.parse()
                .with_context(|| format!("Couldn't parse `{url}` into a URL"))?,
        )
        .await?;

    println!("{}", serde_json::to_string_pretty(&results)?);

    Ok(())
}
