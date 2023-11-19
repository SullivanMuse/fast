mod context;
mod cst;

use crate::context::Context;

use std::path::PathBuf;

use clap::{Parser, Subcommand};

#[derive(Debug, Parser)]
#[clap(name = "fast", version = "0.0.1", author = "J. Sullivan Muse")]
struct Args {
    #[command(subcommand)]
    sub: Sub,
}

/// fast run FILE
/// fast cmd 5*3
/// fast repl
#[derive(Debug, Subcommand)]
enum Sub {
    Run { input: PathBuf },
    Cmd { segments: Vec<String> },
    Repl,
}

fn main() {
    let args = Args::parse();
    let mut context = Context::new();
    loop {
        let source = context.repl();
        dbg!(source);
        context.debug(source, 5);
    }
}
