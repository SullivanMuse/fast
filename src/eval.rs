use crate::{
    env::{Env as Environment, EnvVec},
    expr::{Ellipsis, Expr, Input, Pattern, Statement},
};
use std::{cell::RefCell, collections::HashSet, rc::Rc};
use unwrap::unwrap;

type Env<'a> = EnvVec<String, ValuePtr<'a>>;

pub(crate) type Intrinsic<'a> = fn(&Value<'a>) -> Value<'a>;

pub(crate) type Intrinsics<'a> = Vec<(&'static str, Intrinsic<'a>)>;

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Closure<'a> {
    pub(crate) env: RefCell<Env<'a>>,
    pub(crate) params: Vec<Input<'a>>,
    pub(crate) body: Expr<'a>,
}

#[derive(Clone)]
pub(crate) enum Value<'a> {
    Uninit,
    Int(i64),
    Tag(&'a str),
    Tuple(Vec<ValuePtr<'a>>),
    Closure(Closure<'a>),
    Intrinsic(Intrinsic<'a>),
}

impl<'a> std::fmt::Debug for Value<'a> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Uninit => fmt.debug_tuple("Value::Uninit").finish(),
            Value::Int(x) => fmt.debug_tuple("Value::Int").field(x).finish(),
            Value::Tag(tag) => fmt.debug_tuple("Value::Tag").field(tag).finish(),
            Value::Tuple(inner) => fmt.debug_tuple("Value::Tuple").field(inner).finish(),
            Value::Closure(closure) => fmt.debug_tuple("Value::Closure").field(closure).finish(),
            Value::Intrinsic(_) => fmt.debug_tuple("Value::Intrinsic").finish(),
        }
    }
}

impl<'a> core::cmp::PartialEq for Value<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Uninit, Value::Uninit) => true,
            (Value::Int(x), Value::Int(y)) if x == y => true,
            (Value::Tag(x), Value::Tag(y)) if x == y => true,
            (Value::Tuple(x), Value::Tuple(y)) if x == y => true,
            (Value::Closure(x), Value::Closure(y)) if x == y => true,
            (Value::Intrinsic(x), Value::Intrinsic(y)) if std::ptr::eq(x, y) => true,
            _ => false,
        }
    }
}

impl<'a> Value<'a> {
    pub(crate) fn get_i64(&self) -> i64 {
        match self {
            Value::Int(x) => *x,
            _ => panic!("interpreter: expected i64: {:?}", self),
        }
    }
}

impl<'a> Value<'a> {
    const UNIT: Self = Self::Tuple(Vec::new());

    pub(crate) fn into_ptr(self) -> ValuePtr<'a> {
        Rc::new(RefCell::new(self))
    }
}

pub(crate) type ValuePtr<'a> = Rc<RefCell<Value<'a>>>;

fn expand_list<'a>(exprs: &Vec<Expr<'a>>, env: &mut Env<'a>) -> Vec<ValuePtr<'a>> {
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

            elem => xs.push(elem.eval(env).into_ptr()),
        }
    }
    xs
}

impl<'a> Expr<'a> {
    #[cfg(test)]
    fn eval_new(&'a self) -> Value<'a> {
        let mut env = Env::new();
        self.eval(&mut env)
    }

    pub(crate) fn eval_with_intrinsics(&self, fs: &Intrinsics<'a>) -> Value<'a> {
        let mut env = Env::new();
        for (k, v) in fs {
            env.insert(k.to_string(), Value::Intrinsic(*v).into_ptr());
        }
        self.eval(&mut env)
    }

    fn eval(&self, env: &mut Env<'a>) -> Value<'a> {
        match self {
            Self::Int(span) => Value::Int(span.value_i64()),

            Self::Id(span) => env[span.as_inner()].borrow().clone(),

            Self::Tag(_, span) => Value::Tag(span.as_inner()),

            Self::Expand(_) => panic!(
                "interpreter: expand expressions must be inside tuples: {self:?}"
            ),

            Self::Tuple(_, inner) => Value::Tuple(expand_list(inner, env)),

            Self::App(ref app) => match app.inner.eval(env) {
                Value::Closure(closure) => {
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
                    f(&Value::Tuple(args))
                }

                x => panic!(
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

            Self::Do(inner) => {
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
                    .unwrap_or(Value::UNIT);
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
                let body = (**inner).clone();
                Value::Closure(Closure { env, params, body })
            }
        }
    }

    fn free(&self, set: &mut HashSet<&'a str>) {
        match self {
            Self::Id(span) => {
                set.insert(span.as_inner());
            }
            Self::Expand(ellipsis) => {
                ellipsis.id.map(|id| set.insert(id.as_inner()));
            }
            Self::Tuple(_, inner) => inner.iter().for_each(|e| e.free(set)),
            Self::App(app) => {
                app.inner.free(set);
                app.args.iter().for_each(|e| e.free(set));
            }
            Self::Case(case) => {
                case.subject.free(set);
                for arm in &case.arms {
                    arm.expr.free(set);
                    arm.pattern.remove_bound(set);
                }
            }
            Self::Paren(_, inner) => inner.free(set),
            Self::Do(do_struct) => {
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
            Self::Fn(_, param, body) => {
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
            Self::Id(span) => {
                set.remove(span.as_inner());
            }
            Self::Collect(ellipsis) => match ellipsis.id {
                None => {}
                Some(id) => {
                    set.remove(id.as_inner());
                }
            },
            Self::Tuple(_, inner) => inner.iter().for_each(|p| p.remove_bound(set)),
            Self::App(pattern_app) => {
                pattern_app.f.remove_bound(set);
                pattern_app.xs.iter().for_each(|p| p.remove_bound(set));
            }
            Self::Paren(_, inner) => inner.remove_bound(set),
            _ => {}
        }
    }

    fn bind(&self, value: &Value<'a>, env: &mut Env<'a>) -> bool {
        match self {
            // id patterns bind unconditionally to the value
            Self::Id(id) => {
                let key = id.as_inner();
                match env.get(key).map(Clone::clone) {
                    Some(inner) => match inner.replace(Value::Uninit) {
                        Value::Uninit => {
                            inner.replace(value.clone());
                        }
                        _ => env.insert(key.to_string(), value.clone().into_ptr()),
                    },
                    _ => env.insert(key.to_string(), value.clone().into_ptr()),
                }
                true
            }

            // ignore conditions bind unconditionally without modifying the environment
            Self::Ignore(_) => true,

            // int patterns bind if the value is equal to the specified int
            Self::Int(span) => matches!(value, Value::Int(y) if span.value_i64() == *y),

            // tag pattern binds if the value is equal to the specified tag
            Self::Tag(_, span) => matches!(value, Value::Tag(tag) if span.as_inner() == *tag),

            // Bare collects are not allowed
            Self::Collect(_) => {
                panic!("interpreter: bare collect patterns are not allowed: {self:?}")
            }

            // May include up to one collect pattern
            Self::Tuple(_, patterns) => {
                // Ensure that the value is a tuple
                let values = if let Value::Tuple(values) = value {
                    values
                } else {
                    return false;
                };

                let collect_count = patterns
                    .iter()
                    .filter(|pat| matches!(pat, Self::Collect(_)))
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
                            .map(|(pat, ex)| pat.bind(&ex.borrow(), env))
                            .all(|x| x)
                    } else {
                        false
                    }
                } else {
                    let collect_index = unwrap!(
                        patterns
                            .iter()
                            .position(|pat| matches!(pat, Self::Collect(_))),
                        "interpreter: should be a collect pattern here: {:?}",
                        self
                    );
                    let first = patterns[..collect_index]
                        .iter()
                        .zip(values[..collect_index].iter())
                        .map(|(pat, ex)| pat.bind(&ex.borrow(), env))
                        .all(|x| x);
                    let collect_values_count = (patterns.len() - 1) - values.len();
                    // collect values
                    let collected =
                        values[collect_index..collect_index + collect_values_count].to_vec();
                    if let Self::Collect(ellipsis) = &patterns[collect_index] {
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
                        .map(|(pat, ex)| pat.bind(&ex.borrow(), env))
                        .all(|x| x);
                    first && second
                }
            }

            Self::App(_) => todo!(),

            // Obviously we just bind the inner pattern
            Self::Paren(_, inner) => inner.bind(value, env),
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
                assert_eq!(x.eval_new(), $v);
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
    fn test_case1() {
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

    #[test]
    fn test_case2() {
        evals_to!("case (2, 3) of (1, y) = y of (x, 3) = x end", Value::Int(2));
    }

    #[test]
    fn test_case3() {
        evals_to!("case :y of :x = 1 of :y = 2 end", Value::Int(2));
    }

    #[test]
    fn test_case4() {
        evals_to!(
            "case (:x, :y) of (:y, :x) = 1 of (:x, :y) = 2 end",
            Value::Int(2)
        );
    }
}
