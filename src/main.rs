mod env;
mod eval;
mod expr;
mod parse;
mod span;

use crate::eval::Value;
use crate::eval::ValuePtr;
use parse::expr;

fn main() {
    fn input() -> String {
        let mut s = String::new();
        std::io::stdin().read_line(&mut s).unwrap();
        s
    }

    fn dec(x: ValuePtr) -> ValuePtr {
        match *x.borrow() {
            Value::Int(x) => Value::Int(x - 1).into_ptr(),
            _ => panic!("TypeError"),
        }
    }

    let intrinsics: &[(&str, fn(ValuePtr) -> ValuePtr)] = &[("dec", dec)];

    loop {
        let s = input();
        let span = s.as_str().into();
        let expr = expr(span);
        match expr {
            Err(_) => (),
            Ok((_, e)) => {
                let value = e.eval_with_intrinsics(intrinsics);
                println!("{value:?}");
            }
        }
    }
}
