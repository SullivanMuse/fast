use crate::{
    env::{Env as Environment, EnvVec},
    expr::{App, Ellipsis, Expr, Input},
};

type Env<'a> = EnvVec<String, Value<'a>>;

#[derive(Clone, Debug)]
pub(crate) enum Value<'a> {
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

fn expand_list<'a>(exprs: &'a Vec<Expr<'a>>, env: &mut Env<'a>) -> Vec<Value<'a>> {
    let mut xs = Vec::new();
    for elem in exprs {
        match elem {
            Expr::Expand(Ellipsis { span: _, id }) => {
                let key: &str = id.expect("Must have value to unpack.").as_inner();
                match env.get(key).clone() {
                    Value::Tuple(mut inner) => xs.append(&mut inner),
                    _ => panic!("Expand expression must evaluate to a tuple."),
                }
            }
            elem => xs.push(elem.eval(env)),
        }
    }
    xs
}

impl<'a> Expr<'a> {
    pub(crate) fn eval_new(&'a self) -> Value<'a> {
        let mut env = Env::new();
        self.eval(&mut env)
    }

    fn eval(&'a self, env: &mut Env<'a>) -> Value<'a> {
        match self {
            Self::Int(span) => Value::Int(span.as_inner().parse::<i64>().unwrap()),

            Self::Id(span) => env.get(span.as_inner()).clone(),

            Expr::Tag(_, span) => Value::Tag(span.as_inner()),

            Expr::Expand(_) => panic!("Expand expressions must be inside tuples."),

            Expr::Tuple(_, inner) => Value::Tuple(expand_list(inner, env)),

            Expr::App(App {
                span: _,
                inner,
                arg_span: _,
                args,
            }) => match inner.eval(env) {
                Value::Closure {
                    env: _,
                    params,
                    body,
                } => {
                    let args = expand_list(args, env);
                    assert!(params.len() == args.len(), "Params must match args.");
                    let mut env = env.clone();
                    for (param, arg) in params.into_iter().zip(args) {
                        env.insert(param.as_inner().to_string(), arg);
                    }
                    body.eval(&mut env)
                }
                _ => panic!("Callee must evaluate to a closure."),
            },

            Expr::Case {
                span,
                subject,
                arms,
            } => todo!(),

            Expr::Paren(_, inner) => inner.eval(env),

            Expr::Do { .. } => todo!(),

            Expr::Fn(_, param, inner) => {
                let env = env.clone();
                let params = vec![*param];
                let body = &inner;
                Value::Closure { env, params, body }
            }
        }
    }
}
