#![forbid(unsafe_code)]
use std::path::PathBuf;

use anyhow::Context;
use clap::Parser as _;
use scrapelect::{frontend::Parser, interpreter::Interpreter};
use url::Url;

#[derive(Debug, clap::Parser)]
#[command(version, args_conflicts_with_subcommands = true)]
struct Interface {
    #[command(subcommand)]
    mode: Option<Mode>,
    // There is some magic going on where `Option<RunArgs>` makes it required
    // when the subcommand is not provided.
    #[command(flatten)]
    run: Option<RunArgs>,
}

#[derive(Debug, clap::Args)]
struct RunArgs {
    /// The `.scrp` file describing how to convert the web page into structured data
    file: PathBuf,
    /// The URL of the web page to start scraping at.
    url: Url,
}

#[derive(Debug, clap::Subcommand)]
enum Mode {
    Run(RunArgs),
    Repl,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args = Interface::parse();

    match (args.mode, args.run) {
        (Some(Mode::Run(run_args)), None) | (None, Some(run_args)) => {
            let pgm = std::fs::read_to_string(&run_args.file)
                .with_context(|| format!("error reading file {}", run_args.file.display()))?;

            let parser = Parser::new(&pgm);

            let ast = parser
                .parse()
                .with_context(|| format!("parse error in {}:", run_args.file.display()))?;

            let interpreter = Interpreter::new();

            let results = interpreter.interpret(&ast, run_args.url).await?;

            println!("{}", serde_json::to_string_pretty(&results)?);
        }
        // TODO: investigate if the (None, None) branch is reachable (I think it isn't)
        (Some(Mode::Repl), None) | (None, None) => {
            todo!()
        }
        (Some(_), Some(_)) => {
            unreachable!(
                "This should be impossible to reach with clap's `args_conflicts_with_subcommands`.
                If you see this error message, please file a GitHub issue with the arguments
                you provided to `scrapelect`."
            )
        }
    }

    Ok(())
}
