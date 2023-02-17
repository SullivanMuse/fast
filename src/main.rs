mod env;
mod eval;
mod expr;
mod parse;
mod span;

use crate::{
    eval::{Intrinsics, Value},
    parse::expr,
};

fn main() {
    fn input() -> String {
        let mut s = String::new();
        std::io::stdin().read_line(&mut s).unwrap();
        s
    }

    loop {
        let dec = |x: &Value| Value::Int(x.get_i64() - 1);
        let inc = |x: &Value| Value::Int(x.get_i64() + 1);
        let intrinsics: Intrinsics<'_> = vec![("dec", dec), ("inc", inc)];
        let s = input();
        let span = s.as_str().into();
        match expr(span) {
            Ok((_, e)) => {
                let value = e.eval_with_intrinsics(&intrinsics);
                println!("{value:?}");
            }
            _ => (),
        }
    }
}
