use crate::syntax::{Expr, Type };
use core::fmt;
use std::collections::HashMap;

pub struct Context(HashMap<String, Type>);
impl Context {
    pub fn new() -> Self {
        Context(HashMap::new())
    }
    pub fn add(&mut self, name: String, ty: Type) {
        self.0.insert(name, ty);
    }
    pub fn get(&self, name: &str) -> Option<&Type> {
        self.0.get(name)
    }
}
pub enum TypeError {
    ParameterTypeMismatch { expected: Type, found: Type },
    ExpectedFunctionType { found: Type },
    ConditionalGuardNotBool { found: Type },
    BranchesTypeMismatch { then_ty: Type, else_ty: Type },
    UnknownTypeforVariable(String),
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
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
pub fn check_expr(expr: &Expr, mut ctx: &Context) -> Result<Type, TypeError> {
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
        Expr::Lambda { param,param_ty, body } => {
            todo!()
        }
    }
}
