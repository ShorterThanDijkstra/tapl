use core::fmt;
use std::collections::HashMap;

use crate::syntax::{DeBruijnExpr, Program, Type};

#[derive(Clone, Debug, PartialEq)]
pub enum EvalError {
    UnboundVariable(String),
}
impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalError::UnboundVariable(name) => write!(f, "Eval error: unbound variable: {}", name),
        }
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct Env(HashMap<String, DeBruijnExpr>);
impl Env {
    pub fn new() -> Self {
        Env(HashMap::new())
    }
    pub fn get(&self, key: &str) -> Option<&DeBruijnExpr> {
        self.0.get(key)
    }
    pub fn insert(&mut self, key: String, value: DeBruijnExpr) {
        self.0.insert(key, value);
    }
}

fn up1(expr: DeBruijnExpr) -> DeBruijnExpr {
    up(expr, 0, 1)
}

fn down1(expr: DeBruijnExpr) -> DeBruijnExpr {
    down(expr, 1)
}
// [index -> value]expr
fn substitute(index: usize, value: DeBruijnExpr, expr: DeBruijnExpr) -> DeBruijnExpr {
    match expr {
        DeBruijnExpr::Var { index: index1, .. } => {
            if index == index1 {
                value
            } else {
                expr
            }
        }
        DeBruijnExpr::UnboundVar { name, ctx } => DeBruijnExpr::UnboundVar { name, ctx },
        DeBruijnExpr::Bool { .. } => expr,
        DeBruijnExpr::If {
            pred,
            conseq,
            alter,
        } => {
            let pred1 = substitute(index, value.clone(), *pred);
            let conseq1 = substitute(index, value.clone(), *conseq);
            let alter1 = substitute(index, value, *alter);
            DeBruijnExpr::If {
                pred: Box::new(pred1),
                conseq: Box::new(conseq1),
                alter: Box::new(alter1),
            }
        }
        DeBruijnExpr::Application { func, arg } => {
            let func1 = substitute(index, value.clone(), *func);
            let arg1 = substitute(index, value, *arg);
            DeBruijnExpr::Application {
                func: Box::new(func1),
                arg: Box::new(arg1),
            }
        }
        DeBruijnExpr::Lambda {
            param,
            param_ty,
            body,
        } => {
            let body1 = substitute(index + 1, up1(value), *body);
            DeBruijnExpr::Lambda {
                param,
                param_ty,
                body: Box::new(body1),
            }
        }
    }
}
pub fn down(expr: DeBruijnExpr, len: usize) -> DeBruijnExpr {
    match expr {
        DeBruijnExpr::Var { index, ctx } => {
            let new_index = if index < len { 0 } else { index - len };
            DeBruijnExpr::Var {
                index: new_index,
                ctx,
            }
        }
        DeBruijnExpr::UnboundVar { .. } => expr,
        DeBruijnExpr::Bool { .. } => expr,
        DeBruijnExpr::If {
            pred,
            conseq,
            alter,
        } => {
            let pred1 = down(*pred, len);
            let conseq1 = down(*conseq, len);
            let alter1 = down(*alter, len);
            DeBruijnExpr::If {
                pred: Box::new(pred1),
                conseq: Box::new(conseq1),
                alter: Box::new(alter1),
            }
        }
        DeBruijnExpr::Application { func, arg } => {
            let func1 = down(*func, len);
            let arg1 = down(*arg, len);
            DeBruijnExpr::Application {
                func: Box::new(func1),
                arg: Box::new(arg1),
            }
        }
        DeBruijnExpr::Lambda {
            param,
            param_ty,
            body,
        } => {
            let body1 = down(*body, len);
            DeBruijnExpr::Lambda {
                param,
                param_ty,
                body: Box::new(body1),
            }
        }
    }
}
pub fn up(expr: DeBruijnExpr, cutoff: usize, len: usize) -> DeBruijnExpr {
    match expr {
        DeBruijnExpr::Var { index, ctx } => {
            let new_index = if index < cutoff { index } else { index + len };
            DeBruijnExpr::Var {
                index: new_index,
                ctx,
            }
        }
        DeBruijnExpr::UnboundVar { .. } => expr,
        DeBruijnExpr::Bool { .. } => expr,
        DeBruijnExpr::If {
            pred,
            conseq,
            alter,
        } => DeBruijnExpr::If {
            pred: Box::new(up(*pred, cutoff, len)),
            conseq: Box::new(up(*conseq, cutoff, len)),
            alter: Box::new(up(*alter, cutoff, len)),
        },
        DeBruijnExpr::Application { func, arg } => DeBruijnExpr::Application {
            func: Box::new(up(*func, cutoff, len)),
            arg: Box::new(up(*arg, cutoff, len)),
        },
        DeBruijnExpr::Lambda {
            param,
            body,
            param_ty,
        } => DeBruijnExpr::Lambda {
            param,
            param_ty,
            body: Box::new(up(*body, cutoff + 1, len)),
        },
    }
}

pub fn step_expr(expr: DeBruijnExpr, env: &Env) -> Result<Option<DeBruijnExpr>, EvalError> {
    let expr = match expr {
        DeBruijnExpr::Application { func, arg } => {
            let func = match *func {
                DeBruijnExpr::UnboundVar { name, ctx } => match env.get(&name) {
                    Some(e) => e.clone(),
                    None => DeBruijnExpr::UnboundVar { name, ctx },
                },
                other => other,
            };
            let arg = match *arg {
                DeBruijnExpr::UnboundVar { name, ctx } => match env.get(&name) {
                    Some(e) => e.clone(),
                    None => DeBruijnExpr::UnboundVar { name, ctx },
                },
                other => other,
            };
            DeBruijnExpr::Application {
                func: Box::new(func),
                arg: Box::new(arg),
            }
        }
        _ => expr,
    };
    match expr {
        DeBruijnExpr::Application { func, arg } if (*func).is_lambda() && (*arg).is_value() => {
            if let DeBruijnExpr::Lambda { body, .. } = *func {
                Ok(Some(down1(substitute(0, up1(*arg), *body))))
            } else {
                unreachable!()
            }
        }
        DeBruijnExpr::Application { func, arg } if (*func).is_value() => {
            let next = step_expr(*arg, env)?;
            Ok(next.map(|next| DeBruijnExpr::Application {
                func,
                arg: Box::new(next),
            }))
        }
        DeBruijnExpr::Application { func, arg } if (*arg).is_value() => {
            let next = step_expr(*func, env)?;
            Ok(next.map(|next| DeBruijnExpr::Application {
                func: Box::new(next),
                arg,
            }))
        }
        DeBruijnExpr::If { pred, conseq, .. } if pred.is_true() => Ok(Some(*conseq)),
        DeBruijnExpr::If { pred, alter, .. } if pred.is_false() => Ok(Some(*alter)),
        DeBruijnExpr::If {
            pred,
            conseq,
            alter,
        } => {
            let next = step_expr(*pred, env)?;
            Ok(next.map(|next| DeBruijnExpr::If {
                pred: Box::new(next),
                conseq,
                alter,
            }))
        }
        DeBruijnExpr::UnboundVar { name, .. } => {
            if let Some(value) = env.get(&name) {
                Ok(Some(value.clone()))
            } else {
                Err(EvalError::UnboundVariable(name))
            }
        }
        _ => Ok(None),
    }
}

// context makes trouble
fn remove_ctx(expr: DeBruijnExpr) -> DeBruijnExpr {
    match expr {
        DeBruijnExpr::Var { index, .. } => DeBruijnExpr::Var { index, ctx: vec![] },
        DeBruijnExpr::UnboundVar { name, .. } => DeBruijnExpr::UnboundVar { name, ctx: vec![] },
        DeBruijnExpr::Bool { b, .. } => DeBruijnExpr::Bool { b, ctx: vec![] },
        DeBruijnExpr::If {
            pred,
            conseq,
            alter,
        } => DeBruijnExpr::If {
            pred: Box::new(remove_ctx(*pred)),
            conseq: Box::new(remove_ctx(*conseq)),
            alter: Box::new(remove_ctx(*alter)),
        },
        DeBruijnExpr::Application { func, arg } => DeBruijnExpr::Application {
            func: Box::new(remove_ctx(*func)),
            arg: Box::new(remove_ctx(*arg)),
        },
        DeBruijnExpr::Lambda {
            param,
            body,
            param_ty,
        } => DeBruijnExpr::Lambda {
            param,
            param_ty,
            body: Box::new(remove_ctx(*body)),
        },
    }
}

pub fn eval_expr(mut expr: DeBruijnExpr, env: &Env) -> Result<DeBruijnExpr, EvalError> {
    expr = remove_ctx(expr);
    while let Some(next) = step_expr(expr.clone(), env)? {
        expr = next;
    }
    assert!(expr.is_value());
    Ok(expr)
}
pub fn eval_program(program: Program) -> Result<DeBruijnExpr, EvalError> {
    let mut env = Env::new();
    for def in program.defs {
        let debruijn = DeBruijnExpr::from_expr(def.expr);
        let value = eval_expr(debruijn, &env)?;
        env.insert(def.name, value);
    }
    eval_expr(DeBruijnExpr::from_expr(program.main), &env)
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    fn parse_expr(str: &str) -> DeBruijnExpr {
        let from_str = Parser::from_str(str);
        match from_str {
            Ok(mut parser) => match parser.parse_expr() {
                Ok(coord_expr) => DeBruijnExpr::from_expr(coord_expr.expr),
                Err(e) => panic!("{}", e),
            },
            Err(e) => panic!("{}", e),
        }
    }
    #[test]
    fn test_atom1() {
        parse_expr("x");
    }

    #[test]
    fn test_atom2() {
        let expr = parse_expr("True");
        assert_eq!(
            expr,
            DeBruijnExpr::Bool {
                b: true,
                ctx: vec![]
            }
        );
    }

    #[test]
    fn test_eval_simple1() {
        let expr = parse_expr("(λx :Bool=> x) True");
        let result = eval_expr(expr.clone(), &Env::new()).unwrap();
        assert_eq!(
            result,
            DeBruijnExpr::Bool {
                b: true,
                ctx: vec![]
            }
        );
    }

    #[test]
    fn test_eval_simple2() {
        let expr = parse_expr("if False then True else False");
        let result = eval_expr(expr.clone(), &Env::new()).unwrap();
        assert_eq!(
            result,
            DeBruijnExpr::Bool {
                b: false,
                ctx: vec![]
            }
        );
    }

    #[test]
    fn test_eval_simple3() {
        let expr = parse_expr("λx :Bool => λy:Bool => x");
        let result = eval_expr(expr.clone(), &Env::new()).unwrap();
        assert_eq!(result, remove_ctx(expr));
    }
    #[test]
    fn test_eval_app1() {
        let expr = parse_expr("((λx : Bool => λy : Bool => x) True) False");
        let result = eval_expr(expr.clone(), &Env::new()).unwrap();
        assert_eq!(
            result,
            DeBruijnExpr::Bool {
                b: true,
                ctx: vec![]
            }
        );
    }
    #[test]
    fn test_eval_app2() {
        let expr = parse_expr("(λx : Bool => x) ((λy : Bool => y) True)");
        let result = eval_expr(expr.clone(), &Env::new()).unwrap();
        assert_eq!(
            result,
            DeBruijnExpr::Bool {
                b: true,
                ctx: vec![]
            }
        )
    }

    #[test]
    fn test_eval_if_lambda() {
        let expr = parse_expr("if True then (λx : _ => x x) else (λx : _ => λy : _ => y)");
        let result = eval_expr(expr.clone(), &Env::new()).unwrap();
        assert_eq!(
            result,
            DeBruijnExpr::Lambda {
                param: "x".to_string(),

                param_ty: Type::Var("_".to_string()),
                body: Box::new(DeBruijnExpr::Application {
                    func: Box::new(DeBruijnExpr::Var {
                        index: 0,
                        ctx: vec![]
                    }),
                    arg: Box::new(DeBruijnExpr::Var {
                        index: 0,
                        ctx: vec![]
                    }),
                }),
            }
        )
    }

    #[test]
    pub fn test_eval_if_app() {
        let expr = parse_expr("if (λx : _ => x) True then (λy : _ => y) else (λz : _ => False)");
        let result = eval_expr(expr.clone(), &Env::new()).unwrap();
        assert_eq!(
            result,
            DeBruijnExpr::Lambda {
                param: "y".to_string(),
                param_ty: Type::Var("_".to_string()),
                body: Box::new(DeBruijnExpr::Var {
                    index: 0,
                    ctx: vec![]
                }),
            }
        )
    }
}
