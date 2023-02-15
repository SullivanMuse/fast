use crate::{
    env::{Env as Environment, EnvVec},
    expr::{Ellipsis, Expr, Input, Pattern, Statement},
};
use std::{cell::RefCell, rc::Rc};

type Env<'a> = EnvVec<String, ValuePtr<'a>>;

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Closure<'a> {
    pub(crate) env: Env<'a>,
    pub(crate) params: Vec<Input<'a>>,
    pub(crate) body: &'a Expr<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Value<'a> {
    Uninit,
    Int(i64),
    Tag(&'a str),
    Tuple(Vec<ValuePtr<'a>>),
    Closure(Closure<'a>),
}

impl<'a> Value<'a> {
    fn unit() -> ValuePtr<'a> {
        Self::Tuple(Vec::new()).into_ptr()
    }

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

            Self::Tag(_, span) => Value::Tag(span.as_inner()).into_ptr(),

            Self::Expand(_) => panic!("Expand expressions must be inside tuples."),

            Self::Tuple(_, inner) => Value::Tuple(expand_list(inner, env)).into_ptr(),

            Self::App(ref app) => match *app.inner.eval(env).borrow() {
                Value::Closure(ref closure) => {
                    // Expand arguments to closure
                    let args = expand_list(&app.args, env);

                    // Make sure args match closure
                    assert!(
                        closure.params.len() == args.len(),
                        "Params must match args."
                    );

                    // Copy the closure's environment
                    let mut env = closure.env.clone();
                    for (param, arg) in closure.params.iter().zip(args) {
                        env.insert(param.as_inner().to_string(), arg);
                    }
                    closure.body.eval(&mut env)
                }
                _ => panic!("Callee must evaluate to a closure."),
            },

            Self::Case(_) => todo!(),

            Self::Paren(_, inner) => inner.eval(env),

            Self::Do(ref inner) => {
                let mut env = Env::new();
                for statement in inner.statements.iter() {
                    match statement {
                        Statement::Expr(expr) => {
                            expr.eval(&mut env);
                        }
                        Statement::Assign(assign) => {
                            let value = assign.expr.eval(&mut env);
                            match assign.pattern {
                                Pattern::Id(span) => env.insert(span.as_inner().to_string(), value),
                                _ => todo!(),
                            }
                        }
                    }
                }
                inner
                    .ret
                    .as_ref()
                    .map(|e| e.eval(&mut env))
                    .unwrap_or_else(Value::unit)
            }

            Self::Fn(_, param, inner) => {
                let env = env.clone();
                let params = vec![*param];
                let body = &inner;
                Value::Closure(Closure { env, params, body }).into_ptr()
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::expr;

    #[test]
    fn test_eval_int() {
        if let Ok((_, x)) = expr("1234".into()) {
            assert_eq!(x.eval_new(), Value::Int(1234).into_ptr());
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_eval_unit() {
        if let Ok((_, x)) = expr("()".into()) {
            assert_eq!(x.eval_new(), Value::Tuple(vec![]).into_ptr());
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_eval_tuple() {
        if let Ok((_, x)) = expr("(1, 2, 3)".into()) {
            assert_eq!(
                x.eval_new(),
                Value::Tuple(vec![
                    Value::Int(1).into_ptr(),
                    Value::Int(2).into_ptr(),
                    Value::Int(3).into_ptr()
                ])
                .into_ptr()
            );
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_eval_tag() {
        if let Ok((_, x)) = expr(":tag".into()) {
            assert_eq!(x.eval_new(), Value::Tag("tag").into_ptr());
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_eval_fun_call1() {
        if let Ok((_, x)) = expr("(x -> x)(3)".into()) {
            assert_eq!(x.eval_new(), Value::Int(3).into_ptr());
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_eval_fun_call2() {
        if let Ok((_, x)) = expr("(x y z -> x)(1)(2)(3)".into()) {
            assert_eq!(x.eval_new(), Value::Int(1).into_ptr());
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_eval_fun_call3() {
        if let Ok((_, x)) = expr("(x y z -> z)(1)(2)(3)".into()) {
            assert_eq!(x.eval_new(), Value::Int(3).into_ptr());
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_eval_id() {
        if let Ok((_, x)) = expr("{id = x -> x; id(1)}".into()) {
            assert_eq!(x.eval_new(), Value::Int(1).into_ptr());
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_eval_do() {
        if let Ok((_, x)) = expr("{x = 1; x}".into()) {
            assert_eq!(x.eval_new(), Value::Int(1).into_ptr());
        } else {
            assert!(false);
        }
    }
}
