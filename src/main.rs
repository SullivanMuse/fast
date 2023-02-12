mod env;
mod eval;
mod expr;
mod parse;
mod span;

use parse::expr;

fn main() {
    fn input() -> String {
        let mut s = String::new();
        std::io::stdin().read_line(&mut s).unwrap();
        s
    }

    loop {
        let s = input();
        let span = s.as_str().into();
        let expr = expr(span);
        match expr {
            Err(_) => (),
            Ok((_, e)) => {
                let value = e.eval_new();
                println!("{:?}", value);
            }
        }
    }
}
