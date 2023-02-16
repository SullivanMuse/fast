use crate::{
    env::{Env as Environment, EnvVec},
    expr::{Ellipsis, Expr, Input, Pattern, Statement},
};
use std::{cell::RefCell, collections::HashSet, rc::Rc};
use unwrap::unwrap;

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

    pub(crate) fn eval_with_intrinsics(
        &'a self,
        fs: &[(&'a str, fn(ValuePtr<'a>) -> ValuePtr<'a>)],
    ) -> ValuePtr<'a> {
        let mut env = Env::new();
        for (k, v) in fs {
            env.insert(k.to_string(), Value::Intrinsic(*v).into_ptr());
        }
        self.eval(&mut env)
    }

    fn eval(&'a self, env: &mut Env<'a>) -> ValuePtr<'a> {
        match self {
            Self::Int(span) => {
                let value = unwrap!(
                    span.as_inner().parse::<i64>(),
                    "interpreter: {:?} failed to parse to i64",
                    self
                );
                Value::Int(value).into_ptr()
            }

            Self::Id(span) => env[span.as_inner()].clone(),

            Self::Tag(_, span) => Value::Tag(span.as_inner()).into_ptr(),

            Self::Expand(_) => panic!(
                "interpreter: expand expressions must be inside tuples: {self:?}"
            ),

            Self::Tuple(_, inner) => Value::Tuple(expand_list(inner, env)).into_ptr(),

            Self::App(ref app) => match *app.inner.eval(env).borrow() {
                Value::Closure(ref closure) => {
                    // Expand arguments to closure
                    let args = expand_list(&app.args, env);

                    // Make sure args match closure
                    assert!(
                        closure.params.len() == args.len(),
                        "interpreter: params ({:?}) do not match args ({:?})",
                        &closure.params,
                        &app.args,
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
                    assert!(
                        args.len() == 1,
                        "interpreter: intrinsics take one parameter: {self:?}"
                    );
                    f(args[0].clone())
                }
                ref x => panic!(
                    "interpreter: callee must evaluate to a closure: {self:?}, but got {x:?} instead"
                ),
            },

            Self::Case(case) => {
                let subject_value = case.subject.eval(env);
                for arm in &case.arms {
                    env.push();
                    if arm.pattern.bind(&subject_value, env) {
                        return arm.expr.eval(env);
                    }
                    env.pop();
                }
                panic!(
                    "interpreter: none of the case arms was found to match: {case:?}"
                );
            }

            Self::Paren(_, inner) => inner.eval(env),

            Self::Do(ref inner) => {
                env.push();
                for statement in inner.statements.iter() {
                    match statement {
                        Statement::Expr(expr) => {
                            expr.eval(env);
                        }
                        Statement::Assign(assign) => {
                            let value = assign.expr.eval(env);
                            if !assign.pattern.bind(&value, env) {
                                panic!(
                                    "interpreter: irrefutable pattern failed to bind: {:?}",
                                    assign.pattern
                                );
                            }
                        }
                    }
                }
                let out = inner
                    .ret
                    .as_ref()
                    .map(|e| e.eval(env))
                    .unwrap_or_else(Value::unit);
                env.pop();
                out
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
                if let Some(e) = do_struct.ret.as_ref() {
                    e.free(set)
                }
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

    fn bind(&self, value: &ValuePtr<'a>, env: &mut Env<'a>) -> bool {
        match self {
            // id patterns bind unconditionally to the value
            Pattern::Id(id) => {
                let key = id.as_inner();
                match env.get(key).map(Clone::clone) {
                    Some(inner) => match inner.replace(Value::Uninit) {
                        Value::Uninit => inner.swap(value),
                        _ => env.insert(key.to_string(), value.clone()),
                    },
                    _ => env.insert(key.to_string(), value.clone()),
                }
                true
            }

            // ignore conditions bind unconditionally without modifying the environment
            Pattern::Ignore(_) => true,

            // int patterns bind if the value is equal to the specified int
            Pattern::Int(x) => {
                let x = unwrap!(
                    x.as_inner().parse::<i64>(),
                    "interpreter: failed to parse {:?} as i64",
                    self
                );
                matches!(*value.borrow(), Value::Int(y) if x == y)
            }

            // tag pattern binds if the value is equal to the specified tag
            Pattern::Tag(_, span) => {
                matches!(*value.borrow(), Value::Tag(tag) if span.as_inner() == tag)
            }

            // Bare collects are not allowed
            Pattern::Collect(_) => {
                panic!("interpreter: bare collect patterns are not allowed: {self:?}")
            }

            // May include up to one collect pattern
            Pattern::Tuple(_, patterns) => {
                // Ensure that the value is a tuple
                let value = value.borrow();
                let values = if let Value::Tuple(ref values) = *value {
                    values
                } else {
                    return false;
                };

                let collect_count = patterns
                    .iter()
                    .filter(|pat| matches!(pat, Pattern::Collect(_)))
                    .count();
                assert!(
                    collect_count <= 1,
                    "interpreter: must be a maximum of one collect pattern within a tuple pattern: {self:?}"
                );

                if collect_count == 0 {
                    if patterns.len() == values.len() {
                        patterns
                            .iter()
                            .zip(values.iter())
                            .map(|(pat, ex)| pat.bind(ex, env))
                            .all(|x| x)
                    } else {
                        false
                    }
                } else {
                    let collect_index = unwrap!(
                        patterns
                            .iter()
                            .position(|pat| matches!(pat, Pattern::Collect(_))),
                        "interpreter: should be a collect pattern here: {:?}",
                        self
                    );
                    let first = patterns[..collect_index]
                        .iter()
                        .zip(values[..collect_index].iter())
                        .map(|(pat, ex)| pat.bind(ex, env))
                        .all(|x| x);
                    let collect_values_count = (patterns.len() - 1) - values.len();
                    // collect values
                    let collected =
                        values[collect_index..collect_index + collect_values_count].to_vec();
                    if let Pattern::Collect(ellipsis) = &patterns[collect_index] {
                        if let Some(id) = ellipsis.id {
                            env.insert(
                                id.as_inner().to_string(),
                                Value::Tuple(collected).into_ptr(),
                            );
                        }
                    } else {
                        panic!("interpreter: there should be a collect pattern here: {self:?}");
                    }
                    let second = patterns[collect_index + 1..]
                        .iter()
                        .zip(values[collect_index + collect_values_count..].iter())
                        .map(|(pat, ex)| pat.bind(ex, env))
                        .all(|x| x);
                    first && second
                }
            }

            Pattern::App(_) => todo!(),

            // Obviously we just bind the inner pattern
            Pattern::Paren(_, inner) => inner.bind(value, env),
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

    #[test]
    fn test_case() {
        evals_to!(
            "{
            x = 5;
            case x
                of 1 = x
                of 5 = 8
            end
        }",
            Value::Int(8)
        );
    }
}
