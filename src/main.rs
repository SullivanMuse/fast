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
        println!("{:?}", expr(span));
    }
}
