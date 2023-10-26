mod env;
mod eval;
mod expr;
mod load_module;
mod parse;
mod span;

use clap::{Parser, Subcommand};
use std::fs::read_to_string;
use std::io::Write;
use std::path::PathBuf;

use crate::{
    eval::{Intrinsics, Value},
    parse::expr,
};

fn repl() {
    fn input(prompt: &str) -> String {
        print!("{}", prompt);
        std::io::stdout().flush().unwrap();
        let mut s = String::new();
        std::io::stdin().read_line(&mut s).unwrap();
        s
    }

    loop {
        let dec = |x: &Value| Value::Int(x.get_i64() - 1);
        let inc = |x: &Value| Value::Int(x.get_i64() + 1);
        let intrinsics: Intrinsics<'_> = vec![("dec", dec), ("inc", inc)];
        let s = input("(fast) ");
        let span = s.as_str().into();
        match expr(span) {
            Ok((_, e)) => {
                let value = e.eval_with_intrinsics(&intrinsics);
                println!("{value:?}");
            }
            _ => {}
        }
    }
}

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
    let s = match args.sub {
        Sub::Run { input } => read_to_string(input).unwrap(),
        Sub::Cmd { segments } => segments.join(" "),
        Sub::Repl => {
            repl();
            return;
        }
    };
    let dec = |x: &Value| Value::Int(x.get_i64() - 1);
    let inc = |x: &Value| Value::Int(x.get_i64() + 1);
    let intrinsics: Intrinsics<'_> = vec![("dec", dec), ("inc", inc)];
    let span = s.as_str().into();
    match expr(span) {
        Ok((_, e)) => {
            let value = e.eval_with_intrinsics(&intrinsics);
            println!("{value:?}");
        }
        _ => {}
    }
}
