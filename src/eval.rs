use crate::{
    env::{Env as Environment, EnvVec},
    expr::{Ellipsis, Expr, Input, Pattern, Statement},
};
use std::{cell::RefCell, collections::HashSet, rc::Rc};

type Env<'a> = EnvVec<String, ValuePtr<'a>>;

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Closure<'a> {
    pub(crate) env: RefCell<Env<'a>>,
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
    Intrinsic(fn(ValuePtr<'a>) -> ValuePtr<'a>),
}

impl<'a> Value<'a> {
    fn unit() -> ValuePtr<'a> {
        Self::Tuple(Vec::new()).into_ptr()
    }

    pub(crate) fn into_ptr(self) -> ValuePtr<'a> {
        Rc::new(RefCell::new(self))
    }
}

pub(crate) type ValuePtr<'a> = Rc<RefCell<Value<'a>>>;

fn expand_list<'a>(exprs: &'a Vec<Expr<'a>>, env: &mut Env<'a>) -> Vec<ValuePtr<'a>> {
    let mut xs = Vec::new();
    for elem in exprs {
        match elem {
            Expr::Expand(Ellipsis { span: _, id }) => {
                let key: &str = id.expect("Must have value to unpack.").as_inner();
                match *env[key].borrow_mut() {
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

    pub(crate) fn eval_with_intrinsics(&'a self, fs: &[(&'a str, fn(ValuePtr<'a>) -> ValuePtr<'a>)]) -> ValuePtr<'a> {
        let mut env = Env::new();
        for (k, v) in fs {
            env.insert(k.to_string(), Value::Intrinsic(*v).into_ptr());
        }
        self.eval(&mut env)
    }

    fn eval(&'a self, env: &mut Env<'a>) -> ValuePtr<'a> {
        match self {
            Self::Int(span) => Value::Int(span.as_inner().parse::<i64>().unwrap()).into_ptr(),

            Self::Id(span) => env[span.as_inner()].clone(),

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
                    let mut closure_env = closure.env.borrow_mut();
                    closure_env.push();
                    for (param, arg) in closure.params.iter().zip(args) {
                        closure_env.insert(param.as_inner().to_string(), arg);
                    }
                    let value = closure.body.eval(&mut closure_env);
                    closure_env.pop();
                    value
                }
                Value::Intrinsic(f) => {
                    let args = expand_list(&app.args, env);
                    assert!(args.len() == 1, "Intrinsics take one parameter.");
                    f(args[0].clone())
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
                                Pattern::Id(span) => {
                                    let key = span.as_inner();
                                    match env.get(key).map(Clone::clone) {
                                        None => {
                                            env.insert(key.to_string(), value);
                                        }
                                        Some(inner) => {
                                            let is_init = match *inner.borrow() {
                                                Value::Uninit => true,
                                                _ => false,
                                            };

                                            if is_init {
                                                inner.swap(&value);
                                            } else {
                                                env.insert(key.to_string(), value);
                                            }
                                        }
                                    }
                                }
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
                // Initialize uninitialized captures with Uninit
                let set = {
                    let mut set = HashSet::new();
                    inner.free(&mut set);
                    set.remove(param.as_inner());
                    set
                };
                for key in set {
                    if !env.contains(key) {
                        env.insert(key.to_string(), Value::Uninit.into_ptr());
                    }
                }

                let env = RefCell::new(env.clone());
                let params = vec![*param];
                let body = &inner;
                Value::Closure(Closure { env, params, body }).into_ptr()
            }
        }
    }

    fn free(&self, set: &mut HashSet<&'a str>) {
        match self {
            Expr::Id(span) => {
                set.insert(span.as_inner());
            }
            Expr::Expand(ellipsis) => {
                ellipsis.id.map(|id| set.insert(id.as_inner()));
            }
            Expr::Tuple(_, inner) => inner.iter().for_each(|e| e.free(set)),
            Expr::App(app) => {
                app.inner.free(set);
                app.args.iter().for_each(|e| e.free(set));
            }
            Expr::Case(case) => {
                case.subject.free(set);
                for arm in &case.arms {
                    arm.expr.free(set);
                    arm.pattern.remove_bound(set);
                }
            }
            Expr::Paren(_, inner) => inner.free(set),
            Expr::Do(do_struct) => {
                for statement in &do_struct.statements {
                    match statement {
                        Statement::Expr(e) => e.free(set),
                        Statement::Assign(assign) => {
                            assign.expr.free(set);
                            assign.pattern.remove_bound(set);
                        }
                    }
                }
                do_struct.ret.as_ref().map(|e| e.free(set));
            }
            Expr::Fn(_, param, body) => {
                body.free(set);
                set.remove(param.as_inner());
            }
            _ => {}
        }
    }
}

impl<'a> Pattern<'a> {
    fn remove_bound(&self, set: &mut HashSet<&'a str>) {
        match self {
            Pattern::Id(span) => {
                set.remove(span.as_inner());
            }
            Pattern::Collect(ellipsis) => match ellipsis.id {
                None => {}
                Some(id) => {
                    set.remove(id.as_inner());
                }
            },
            Pattern::Tuple(_, inner) => inner.iter().for_each(|p| p.remove_bound(set)),
            Pattern::App(pattern_app) => {
                pattern_app.f.remove_bound(set);
                pattern_app.xs.iter().for_each(|p| p.remove_bound(set));
            }
            Pattern::Paren(_, inner) => inner.remove_bound(set),
            _ => {}
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::expr;

    macro_rules! evals_to {
        ($s: expr, $v: expr) => {
            if let Ok((_, x)) = expr($s.into()) {
                assert_eq!(x.eval_new(), $v.into_ptr());
            } else {
                assert!(false);
            }
        };
    }

    #[test]
    fn test_eval_int() {
        evals_to!("1234", Value::Int(1234));
    }

    #[test]
    fn test_eval_unit() {
        evals_to!("()", Value::Tuple(vec![]));
    }

    #[test]
    fn test_eval_tuple() {
        evals_to!(
            "(1, 2, 3)",
            Value::Tuple(vec![
                Value::Int(1).into_ptr(),
                Value::Int(2).into_ptr(),
                Value::Int(3).into_ptr()
            ])
        );
    }

    #[test]
    fn test_eval_tag() {
        evals_to!(":tag", Value::Tag("tag"));
    }

    #[test]
    fn test_eval_fun_call1() {
        evals_to!("(x -> x)(3)", Value::Int(3));
    }

    #[test]
    fn test_eval_fun_call2() {
        evals_to!("(x y z -> x)(1)(2)(3)", Value::Int(1));
    }

    #[test]
    fn test_eval_fun_call3() {
        evals_to!("(x y z -> z)(1)(2)(3)", Value::Int(3));
    }

    #[test]
    fn test_eval_id() {
        evals_to!("{id = x -> x; id(1)}", Value::Int(1));
    }

    #[test]
    fn test_eval_do() {
        evals_to!("{x = 1; x}", Value::Int(1));
    }

    #[test]
    fn test_late_binding() {
        evals_to!("{f = x -> g(x); g = x -> 5; f(1)}", Value::Int(5));
    }
}
