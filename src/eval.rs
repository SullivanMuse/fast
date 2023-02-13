use crate::{
    env::{Env as Environment, EnvVec},
    expr::{App, Case, Ellipsis, Expr, Input},
};
use std::{cell::RefCell, rc::Rc};

type Env<'a> = EnvVec<String, ValuePtr<'a>>;

#[derive(Clone, Debug)]
pub(crate) struct Closure<'a> {
    pub(crate) env: Env<'a>,
    pub(crate) params: Vec<Input<'a>>,
    pub(crate) body: &'a Expr<'a>,
}

#[derive(Clone, Debug)]
pub(crate) enum Value<'a> {
    Uninit,
    Int(i64),
    Tag(&'a str),
    Tuple(Vec<ValuePtr<'a>>),
    Closure(Closure<'a>),
}

impl<'a> Value<'a> {
    fn into_ptr(self) -> ValuePtr<'a> {
        Rc::new(RefCell::new(self))
    }
}

type ValuePtr<'a> = Rc<RefCell<Value<'a>>>;

fn expand_list<'a>(exprs: &'a Vec<Expr<'a>>, env: &mut Env<'a>) -> Vec<ValuePtr<'a>> {
    let mut xs = Vec::new();
    for elem in exprs {
        match elem {
            Expr::Expand(Ellipsis { span: _, id }) => {
                let key: &str = id.expect("Must have value to unpack.").as_inner();
                match *env.get(key).borrow_mut() {
                    Value::Tuple(ref inner) => xs.extend(inner.iter().cloned()),
                    _ => panic!("Expand expression must evaluate to a tuple."),
                }
            }

            elem => xs.push(elem.eval(env)),
        }
    }
    xs
}

impl<'a> Expr<'a> {
    pub(crate) fn eval_new(&'a self) -> ValuePtr<'a> {
        let mut env = Env::new();
        self.eval(&mut env)
    }

    fn eval(&'a self, env: &mut Env<'a>) -> ValuePtr<'a> {
        match self {
            Self::Int(span) => Value::Int(span.as_inner().parse::<i64>().unwrap()).into_ptr(),

            Self::Id(span) => env.get(span.as_inner()).clone(),

            Expr::Tag(_, span) => Value::Tag(span.as_inner()).into_ptr(),

            Expr::Expand(_) => panic!("Expand expressions must be inside tuples."),

            Expr::Tuple(_, inner) => Value::Tuple(expand_list(inner, env)).into_ptr(),

            Expr::App(ref app) => match *app.inner.eval(env).borrow() {
                Value::Closure(ref closure) => {
                    let args = expand_list(&app.args, env);
                    assert!(
                        closure.params.len() == args.len(),
                        "Params must match args."
                    );
                    let mut env = env.clone();
                    for (param, arg) in closure.params.iter().zip(args) {
                        env.insert(param.as_inner().to_string(), arg);
                    }
                    closure.body.eval(&mut env)
                }
                _ => panic!("Callee must evaluate to a closure."),
            },

            Expr::Case(_) => todo!(),

            Expr::Paren(_, inner) => inner.eval(env),

            Expr::Do { .. } => todo!(),

            Expr::Fn(_, param, inner) => {
                let env = env.clone();
                let params = vec![*param];
                let body = &inner;
                Value::Closure(Closure { env, params, body }).into_ptr()
            }
        }
    }
}
