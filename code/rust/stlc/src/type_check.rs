use crate::syntax::{Expr, Program, Type};
use core::fmt;
use std::collections::HashMap;

pub struct Context(HashMap<String, Type>);
impl Context {
    pub fn new() -> Self {
        Context(HashMap::new())
    }
    pub fn clone(&self) -> Self {
        Context(self.0.clone())
    }

    pub fn add(&mut self, name: String, ty: Type) {
        self.0.insert(name, ty);
    }
    pub fn get(&self, name: &str) -> Option<&Type> {
        self.0.get(name)
    }
}
#[derive(Debug, PartialEq)]
pub enum TypeError {
    ParameterTypeMismatch {
        expected: Type,
        found: Type,
    },
    ExpectedFunctionType {
        found: Type,
    },
    ConditionalGuardNotBool {
        found: Type,
    },
    BranchesTypeMismatch {
        then_ty: Type,
        else_ty: Type,
    },
    UnknownTypeforVariable(String),
    DefinitionTypeMismatch {
        expected: Type,
        found: Type,
        name: String,
    },
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeError::DefinitionTypeMismatch {
                expected,
                found,
                name,
            } => {
                write!(
                    f,
                    "Definition '{}' type mismatch: expected {}, found {}",
                    name, expected, found
                )
            }
            TypeError::ParameterTypeMismatch { expected, found } => {
                write!(
                    f,
                    "Parameter type mismatch: expected {}, found {}",
                    expected, found
                )
            }
            TypeError::ExpectedFunctionType { found } => {
                write!(f, "Expected function type, found {}", found)
            }
            TypeError::ConditionalGuardNotBool { found } => {
                write!(f, "Conditional guard must be of type Bool, found {}", found)
            }
            TypeError::BranchesTypeMismatch { then_ty, else_ty } => {
                write!(
                    f,
                    "Branches of conditional must have the same type: then branch is {}, else branch is {}",
                    then_ty, else_ty
                )
            }
            TypeError::UnknownTypeforVariable(name) => {
                write!(f, "Unknown type for variable: {}", name)
            }
        }
    }
}
pub fn check_expr(expr: &Expr, ctx: &Context) -> Result<Type, TypeError> {
    match expr {
        Expr::Var(name) => match ctx.get(&name) {
            Some(ty) => Ok(ty.clone()),
            None => Err(TypeError::UnknownTypeforVariable(name.clone())),
        },
        Expr::Bool(_) => Ok(Type::Bool),
        Expr::If {
            pred,
            conseq,
            alter,
        } => {
            let pred_ty = check_expr(&*pred, ctx)?;
            if pred_ty != Type::Bool {
                return Err(TypeError::ConditionalGuardNotBool {
                    found: pred_ty.clone(),
                });
            }
            let conseq_ty = check_expr(&*conseq, ctx)?;
            let alter_ty = check_expr(&*alter, ctx)?;
            if conseq_ty != alter_ty {
                return Err(TypeError::BranchesTypeMismatch {
                    then_ty: conseq_ty,
                    else_ty: alter_ty,
                });
            }
            Ok(conseq_ty)
        }
        Expr::Application { func, arg } => {
            let func_ty = check_expr(&*func, ctx)?;
            match func_ty {
                Type::Function { input, output } => {
                    let arg_ty = check_expr(&*arg, ctx)?;
                    if *input != arg_ty {
                        return Err(TypeError::ParameterTypeMismatch {
                            expected: arg_ty,
                            found: *input,
                        });
                    }
                    Ok(*output)
                }
                _ => Err(TypeError::ExpectedFunctionType {
                    found: func_ty.clone(),
                }),
            }
        }
        Expr::Lambda {
            param,
            param_ty,
            body,
        } => {
            let mut new_ctx = (*ctx).clone();
            new_ctx.add(param.to_string(), (*param_ty).clone());
            let body_ty = check_expr(&*body, &new_ctx)?;
            Ok(Type::Function {
                input: Box::new(param_ty.clone()),
                output: Box::new(body_ty),
            })
        }
    }
}

pub fn check_program(program: &Program) -> Vec<TypeError> {
    let mut ctx = Context::new();
    let mut errs = vec![];
    for def in &program.defs {
        let res = check_expr(&def.expr, &ctx);
        match res {
            Ok(ty) => {
                if let Some(def_ty) = &def.ty {
                    if *def_ty != ty {
                        errs.push(TypeError::DefinitionTypeMismatch {
                            expected: def_ty.clone(),
                            found: ty,
                            name: def.name.clone(),
                        });
                    }
                    ctx.add(def.name.clone(), def_ty.clone());
                }
            }
            Err(e) => {
                errs.push(e);
                if let Some(def_ty) = &def.ty {
                    ctx.add(def.name.clone(), def_ty.clone());
                }
            }
        }
    }
    match check_expr(&program.main, &ctx) {
        Ok(_) => {}
        Err(e) => errs.push(e),
    };
    errs.into_iter().rev().collect()
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::{Def, Expr, Program, Type};

    #[test]
    fn test_check_expr_var() {
        let mut ctx = Context::new();
        ctx.add("x".to_string(), Type::Bool);
        let expr = Expr::Var("x".to_string());
        let ty = check_expr(&expr, &ctx).unwrap();
        assert_eq!(ty, Type::Bool);
    }

    #[test]
    fn test_check_expr_if() {
        let expr = Expr::If {
            pred: Box::new(Expr::Bool(true)),
            conseq: Box::new(Expr::Bool(false)),
            alter: Box::new(Expr::Bool(true)),
        };
        let ty = check_expr(&expr, &Context::new()).unwrap();
        assert_eq!(ty, Type::Bool);
    }

    #[test]
    fn test_check_expr_if_type_mismatch() {
        let expr = Expr::If {
            pred: Box::new(Expr::Bool(true)),
            conseq: Box::new(Expr::Bool(false)),
            alter: Box::new(Expr::Var("x".to_string())),
        };
        let mut ctx = Context::new();
        ctx.add(
            "x".to_string(),
            Type::Function {
                input: Box::new(Type::Bool),
                output: Box::new(Type::Bool),
            },
        );
        let err = check_expr(&expr, &ctx).unwrap_err();
        match err {
            TypeError::BranchesTypeMismatch { then_ty, else_ty } => {
                assert_eq!(then_ty, Type::Bool);
                assert_eq!(
                    else_ty,
                    Type::Function {
                        input: Box::new(Type::Bool),
                        output: Box::new(Type::Bool)
                    }
                );
            }
            _ => panic!("Expected BranchesTypeMismatch error"),
        }
    }

    #[test]
    fn test_check_expr_application() {
        let expr = Expr::Application {
            func: Box::new(Expr::Lambda {
                param: "x".to_string(),
                param_ty: Type::Bool,
                body: Box::new(Expr::Var("x".to_string())),
            }),
            arg: Box::new(Expr::Bool(true)),
        };
        let ty = check_expr(&expr, &Context::new()).unwrap();
        assert_eq!(ty, Type::Bool);
    }
}
