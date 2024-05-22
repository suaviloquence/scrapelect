use std::env;

use anyhow::Context;
use frontend::{ast::AstArena, Parser};

mod frontend;

fn main() -> anyhow::Result<()> {
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

    let (arena, r) = match parser.parse() {
        Ok(t) => t,
        // hack until we remove lifetime from error
        Err(e) => return Err(None.context(e.to_string())?),
    };

    let elements = arena.flatten(r);
    println!("{elements:?}");

    Ok(())
}
