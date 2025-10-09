use crate::syntax::Expr;
use std::{
    collections::HashSet,
    sync::atomic::{AtomicUsize, Ordering},
};

static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn gensym(prefix: &str) -> String {
    let id = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{}{}", prefix, id)
}

fn alpha_convention(expr: &Expr) -> Expr {
    match expr {
        Expr::Var(x) => Expr::Var(gensym(x)),
        Expr::Bool(b) => Expr::Bool(*b),
        Expr::Application { func, arg } => Expr::Application {
            func: Box::new(alpha_convention(func)),
            arg: Box::new(alpha_convention(arg)),
        },
        Expr::Lambda { param, body } => {
            let new_param = gensym(param);
            let new_body = substitute(&*body, param, &Expr::Var(new_param.clone()));
            Expr::Lambda {
                param: new_param,
                body: Box::new(alpha_convention(&new_body)),
            }
        }
    }
}
fn frees(expr: &Expr) -> HashSet<String> {
    match expr {
        Expr::Var(x) => HashSet::from([x.clone()]),
        Expr::Bool(_) => HashSet::new(),
        Expr::Application { func, arg } => {
            let mut frees1 = frees(func);
            let frees2 = frees(arg);
            frees1.extend(frees2);
            frees1
        }
        Expr::Lambda { param, body } => {
            let mut frees1 = frees(body);
            frees1.remove(param);
            frees1
        }
    }
}
pub fn substitute(body: &Expr, x: &str, s: &Expr) -> Expr {
    match &body {
        Expr::Var(var) => {
            if var == x {
                s.clone()
            } else {
                body.clone()
            }
        }
        Expr::Bool(_) => (*body).clone(),
        Expr::Application { func, arg } => {
            let func1 = substitute(&**func, x, s);
            let arg1 = substitute(&**arg, x, s);
            Expr::Application {
                func: Box::new(func1),
                arg: Box::new(arg1),
            }
        }
        Expr::Lambda {
            param,
            body: inner_body,
        } => {
            if param == x {
                // x is shadowed, do not substitute inside
                Expr::Lambda {
                    param: param.clone(),
                    body: inner_body.clone(),
                }
            } else if frees(s).contains(param) {
                // param conflicts, do alpha-renaming
                let new_param = gensym(param);
                let renamed_body = substitute(&*inner_body, param, &Expr::Var(new_param.clone()));
                let new_lambda = Expr::Lambda {
                    param: new_param.clone(),
                    body: Box::new(renamed_body),
                };
                substitute(&new_lambda, x, s)
            } else {
                Expr::Lambda {
                    param: param.clone(),
                    body: Box::new(substitute(&*inner_body, x, s)),
                }
            }
        }
    }
}

pub fn step(expr: Expr) -> Option<Expr> {
    match expr {
        Expr::Application { func, arg } if (*func).is_lambda() && (*arg).is_value() => {
            if let Expr::Lambda { param, body } = *func {
                Some(substitute(&*body, &param, &*arg))
            } else {
                unreachable!()
            }
        }
        Expr::Application { func, arg } if (*func).is_value() => {
            step(*arg).map(|next| Expr::Application {
                func,
                arg: Box::new(next),
            })
        }
        Expr::Application { func, arg } if (*arg).is_value() => {
            step(*func).map(|next| Expr::Application {
                func: Box::new(next),
                arg,
            })
        }
        _ => None,
    }
}

pub fn eval(mut expr: Expr) -> Expr {
    while let Some(next) = step(expr.clone()) {
        println!("{}", next);
        expr = next;
    }
    expr
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    #[test]
    fn test_eval_simple_bool() {
        let expr = Expr::Bool(true);
        assert_eq!(eval(expr.clone()), expr);
    }

    #[test]
    fn test_eval_var() {
        let expr = Expr::Var("x".to_string());
        assert_eq!(eval(expr.clone()), expr);
    }

    #[test]
    fn test_eval_lambda_is_value() {
        let expr = Expr::Lambda {
            param: "x".to_string(),
            body: Box::new(Expr::Var("x".to_string())),
        };
        assert_eq!(eval(expr.clone()), expr);
    }

    #[test]
    fn test_eval_application_simple() {
        // (λx => x) true ==> true
        let expr = Expr::Application {
            func: Box::new(Expr::Lambda {
                param: "x".to_string(),
                body: Box::new(Expr::Var("x".to_string())),
            }),
            arg: Box::new(Expr::Bool(true)),
        };
        assert_eq!(eval(expr), Expr::Bool(true));
    }

    #[test]
    fn test_eval_nested_application() {
        // ((λx => λy => x) true) false ==> λy => true
        let expr = Expr::Application {
            func: Box::new(Expr::Application {
                func: Box::new(Expr::Lambda {
                    param: "x".to_string(),
                    body: Box::new(Expr::Lambda {
                        param: "y".to_string(),
                        body: Box::new(Expr::Var("x".to_string())),
                    }),
                }),
                arg: Box::new(Expr::Bool(true)),
            }),
            arg: Box::new(Expr::Bool(false)),
        };
        let expected = Expr::Bool(true);
        assert_eq!(eval(expr), expected);
    }

    #[test]
    fn test_eval_application_with_non_value_arg() {
        // (λx => x) ((λy => y) true) ==> true
        let expr = Expr::Application {
            func: Box::new(Expr::Lambda {
                param: "x".to_string(),
                body: Box::new(Expr::Var("x".to_string())),
            }),
            arg: Box::new(Expr::Application {
                func: Box::new(Expr::Lambda {
                    param: "y".to_string(),
                    body: Box::new(Expr::Var("y".to_string())),
                }),
                arg: Box::new(Expr::Bool(true)),
            }),
        };
        assert_eq!(eval(expr), Expr::Bool(true));
    }

    #[test]
    fn test_eval_lambda_shadowing() {
        // (λx => λx => x) true ==> λx => x
        let expr = Expr::Application {
            func: Box::new(Expr::Lambda {
                param: "x".to_string(),
                body: Box::new(Expr::Lambda {
                    param: "x".to_string(),
                    body: Box::new(Expr::Var("x".to_string())),
                }),
            }),
            arg: Box::new(Expr::Bool(true)),
        };
        let expected = Expr::Lambda {
            param: "x".to_string(),
            body: Box::new(Expr::Var("x".to_string())),
        };
        assert_eq!(eval(expr), expected);
    }
    #[test]
    fn test_substitute1() {
        let expr = Parser::from_str("λx => x")
            .unwrap()
            .parse_expr()
            .unwrap()
            .expr;
        let expr1 = substitute(&expr, "x", &Expr::Var(format!("y")));
        println!("{}", expr1);
        assert_eq!(
            expr1,
            Expr::Lambda {
                param: "x".to_string(),
                body: Box::new(Expr::Var("x".to_string()))
            }
        );
    }
}
