use crate::expr::{Ellipsis, Expr, Input};
use std::collections::HashMap;

#[derive(Clone, Debug)]
struct Env<'a>(HashMap<&'a str, Value<'a>>);

impl<'a> Env<'a> {
    fn get(&mut self, span: Input<'a>) -> &Value<'a> {
        if !self.0.contains_key(span.as_inner()) {
            self.0.insert(span.as_inner(), Value::Uninit);
        }
        self.0.get(span.as_inner()).unwrap()
    }

    fn get_mut(&mut self, span: Input<'a>) -> &mut Value<'a> {
        if !self.0.contains_key(span.as_inner()) {
            self.0.insert(span.as_inner(), Value::Uninit);
        }
        self.0.get_mut(span.as_inner()).unwrap()
    }
}

#[derive(Clone, Debug)]
enum Value<'a> {
    Uninit,
    Int(i64),
    Tag(&'a str),
    Tuple(Vec<Value<'a>>),
    Closure {
        env: Env<'a>,
        params: Vec<Input<'a>>,
        body: &'a Expr<'a>,
    },
}

fn expand_list<'a>(exprs: &Vec<Expr<'a>>, env: &mut Env<'a>) -> Vec<Value<'a>> {
    let mut xs = vec![];
    for elem in exprs {
        match elem {
            Expr::Expand(Ellipsis { span, id }) => match env.get(id.unwrap()).clone() {
                Value::Tuple(mut inner) => xs.append(&mut inner),
                _ => panic!("Expand expression must evaluate to a tuple."),
            },
            elem => xs.push(elem.eval(env)),
        }
    }
    xs
}

impl<'a> Expr<'a> {
    fn eval(&self, env: &mut Env<'a>) -> Value<'a> {
        match self {
            Self::Int(span) => Value::Int(span.as_inner().parse::<i64>().unwrap()),
            Self::Id(span) => env.get(*span).clone(),
            Expr::Tag(_, span) => Value::Tag(span.as_inner()),
            Expr::Expand(Ellipsis { span, id }) => {
                panic!("Cannot have expand except inside tuple.")
            }
            Expr::Tuple(_, inner) => Value::Tuple(expand_list(inner, env)),
            Expr::App {
                span,
                inner,
                arg_span,
                args,
            } => match inner.eval(env) {
                Value::Closure {
                    env: _,
                    params,
                    body,
                } => {
                    let args = expand_list(args, env);
                    assert!(params.len() == args.len(), "Params must match args.");
                    // Bind all of the arguments
                    todo!()
                }
                _ => panic!("Callee must evaluate to a closure."),
            },
            Expr::Case {
                span,
                subject,
                arms,
            } => todo!(),
            Expr::Paren(_, _) => todo!(),
            Expr::Do {
                span,
                statements,
                ret,
            } => todo!(),
        }
    }
}
