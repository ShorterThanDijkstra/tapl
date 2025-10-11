use std::fmt;
/*
 * Program := Dec Dec*
 * Dec := TypeDec '\n' FuncDec '\n'
 * TypeDec := Identifier ':' Type
 * FuncDec := Identifier '=' Expr
 * Type := Atom | '(' Type '->' Type ')'
 * Expr := Var | Bool | 'if' Expr 'then' Expr 'else' Expr | 'λ' Identifier '=>' Expr | (Expr) | Expr Expr
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub decs: Vec<Dec>,
    pub main: Dec,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Dec {
    pub name: String,
    pub ty_dec: TypeDec,
    pub func_dec: FuncDec,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDec {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDec {
    pub name: String,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Prim(String),
    Function { input: Box<Type>, output: Box<Type> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(String),
    Bool(bool),
    If {
        pred: Box<Expr>,
        conseq: Box<Expr>,
        alter: Box<Expr>,
    },
    Application {
        func: Box<Expr>,
        arg: Box<Expr>,
    },
    Lambda {
        param: String,
        body: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct CoordProgram {
    pub program: Program,
    pub row_start: usize,
    pub col_start: usize,
    pub row_end: usize,
    pub col_end: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CoordDec {
    pub dec: Dec,
    pub row_start: usize,
    pub col_start: usize,
    pub row_end: usize,
    pub col_end: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CoordTypeDec {
    pub type_dec: TypeDec,
    pub row_start: usize,
    pub col_start: usize,
    pub row_end: usize,
    pub col_end: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CoordFuncDec {
    pub func_dec: FuncDec,
    pub row_start: usize,
    pub col_start: usize,
    pub row_end: usize,
    pub col_end: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CoordType {
    pub ty: Type,
    pub row_start: usize,
    pub col_start: usize,
    pub row_end: usize,
    pub col_end: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CoordExpr {
    pub expr: Expr,
    pub row_start: usize,
    pub col_start: usize,
    pub row_end: usize,
    pub col_end: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeBruijnExpr {
    Var {
        index: usize,
        ctx: Vec<String>,
    },
    Bool {
        b: bool,
        ctx: Vec<String>,
    },
    If {
        pred: Box<DeBruijnExpr>,
        conseq: Box<DeBruijnExpr>,
        alter: Box<DeBruijnExpr>,
    },
    Application {
        func: Box<DeBruijnExpr>,
        arg: Box<DeBruijnExpr>,
    },
    Lambda {
        param: String,
        body: Box<DeBruijnExpr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum SyntaxError {
    UnboundIdentifier(String),
    VariableLookupFailure(usize),
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for dec in &self.decs {
            writeln!(f, "{dec}")?;
        }
        writeln!(f, "main: {}", self.main)
    }
}

impl fmt::Display for Dec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.ty_dec)?;
        writeln!(f, "{}", self.func_dec)
    }
}

impl fmt::Display for TypeDec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{} : {}", self.name, self.ty)
    }
}

impl fmt::Display for FuncDec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", self.name, self.body)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Prim(s) => write!(f, "{s}"),
            Type::Function { input, output } => write!(f, "({input} -> {output})"),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Var(s) => write!(f, "{s}"),
            Expr::Bool(true) => write!(f, "True"),
            Expr::Bool(false) => write!(f, "False"),
            Expr::If {
                pred,
                conseq,
                alter,
            } => write!(f, "if {pred} then {conseq} else {alter}"),
            Expr::Application { func, arg } => write!(f, "({func} {arg})"),
            Expr::Lambda { param, body } => write!(f, "(λ{param} => {body})"),
        }
    }
}

impl fmt::Display for CoordProgram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} [{}:{}-{}:{}]",
            self.program, self.row_start, self.col_start, self.row_end, self.col_end
        )
    }
}

impl fmt::Display for CoordDec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} [{}:{}-{}:{}]",
            self.dec, self.row_start, self.col_start, self.row_end, self.col_end
        )
    }
}

impl fmt::Display for CoordTypeDec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} [{}:{}-{}:{}]",
            self.type_dec, self.row_start, self.col_start, self.row_end, self.col_end
        )
    }
}

impl fmt::Display for CoordFuncDec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} [{}:{}-{}:{}]",
            self.func_dec, self.row_start, self.col_start, self.row_end, self.col_end
        )
    }
}

impl fmt::Display for CoordType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} [{}:{}-{}:{}]",
            self.ty, self.row_start, self.col_start, self.row_end, self.col_end
        )
    }
}

impl fmt::Display for CoordExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} [{}:{}-{}:{}]",
            self.expr, self.row_start, self.col_start, self.row_end, self.col_end
        )
    }
}

impl fmt::Display for DeBruijnExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DeBruijnExpr::Var { index, .. } => write!(f, "{index}"),
            DeBruijnExpr::Bool { b, .. } => {
                if *b {
                    write!(f, "True")
                } else {
                    write!(f, "False")
                }
            }
            DeBruijnExpr::If {
                pred,
                conseq,
                alter,
                ..
            } => {
                write!(f, "if {} then {} else {}", pred, conseq, alter)
            }
            DeBruijnExpr::Application { func, arg } => write!(f, "({} {})", func, arg),
            DeBruijnExpr::Lambda { body, .. } => write!(f, "(λ => {})", body),
        }
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SyntaxError::UnboundIdentifier(name) => {
                write!(f, "syntax error: unbound identifier: {}", name)
            }
            SyntaxError::VariableLookupFailure(index) => {
                write!(f, "syntax error: variable look up failure at {}", index)
            }
        }
    }
}

impl Expr {
    pub fn is_value(&self) -> bool {
        match self {
            Expr::Bool(_) => true,
            Expr::Lambda { .. } => true,
            _ => false,
        }
    }
    pub fn is_lambda(&self) -> bool {
        matches!(self, Expr::Lambda { .. })
    }

    pub fn is_true(&self) -> bool {
        matches!(self, Expr::Bool(true))
    }

    pub fn is_false(&self) -> bool {
        matches!(self, Expr::Bool(false))
    }
}
impl DeBruijnExpr {
    pub fn is_value(&self) -> bool {
        match self {
            DeBruijnExpr::Lambda { .. } => true,
            DeBruijnExpr::Bool { .. } => true,
            _ => false,
        }
    }
    pub fn is_lambda(&self) -> bool {
        matches!(self, DeBruijnExpr::Lambda { .. })
    }
    pub fn is_true(&self) -> bool {
        match self {
            DeBruijnExpr::Bool { b, .. } => *b,
            _ => false,
        }
    }
    pub fn is_false(&self) -> bool {
        match self {
            DeBruijnExpr::Bool { b, .. } => !*b,
            _ => false,
        }
    }
    pub fn from_expr(expr: Expr) -> Result<DeBruijnExpr, SyntaxError> {
        Self::from_expr_help(expr, vec![])
    }
    fn to_expr(&self) -> Result<Expr, SyntaxError> {
        match self {
            DeBruijnExpr::Var { index, ctx } => {
                let len = ctx.len();
                let index1 = len - *index - 1;
                match ctx.get(index1) {
                    Some(name) => Ok(Expr::Var(name.clone())),
                    None => Err(SyntaxError::VariableLookupFailure(index1)),
                }
            }
            DeBruijnExpr::Bool { b, .. } => Ok(Expr::Bool(*b)),
            DeBruijnExpr::If {
                pred,
                conseq,
                alter,
            } => {
                let pred1 = pred.to_expr()?;
                let conseq1 = conseq.to_expr()?;
                let alter1 = alter.to_expr()?;
                Ok(Expr::If {
                    pred: Box::new(pred1),
                    conseq: Box::new(conseq1),
                    alter: Box::new(alter1),
                })
            }
            DeBruijnExpr::Application { func, arg } => {
                let func1 = func.to_expr()?;
                let arg1 = arg.to_expr()?;
                Ok(Expr::Application {
                    func: Box::new(func1),
                    arg: Box::new(arg1),
                })
            }
            DeBruijnExpr::Lambda { param, body } => {
                let body1 = body.to_expr()?;
                Ok(Expr::Lambda {
                    param: (*param).clone(),
                    body: Box::new(body1),
                })
            }
        }
    }
    fn from_expr_help(expr: Expr, mut vars: Vec<String>) -> Result<DeBruijnExpr, SyntaxError> {
        match expr {
            Expr::Var(name) => {
                let len = vars.len();
                match vars.clone().iter().rposition(|x| x == &name) {
                    Some(i) => {
                        let index = len - 1 - i;
                        Ok(DeBruijnExpr::Var { index, ctx: vars })
                    }
                    None => Err(SyntaxError::UnboundIdentifier(name)),
                }
            }
            Expr::Bool(b) => Ok(DeBruijnExpr::Bool { b, ctx: vars }),
            Expr::If {
                pred,
                conseq,
                alter,
            } => {
                let pred1 = Self::from_expr_help(*pred, vars.clone())?;
                let conseq1 = Self::from_expr_help(*conseq, vars.clone())?;
                let alter = Self::from_expr_help(*alter, vars.clone())?;
                Ok(DeBruijnExpr::If {
                    pred: Box::new(pred1),
                    conseq: Box::new(conseq1),
                    alter: Box::new(alter),
                })
            }
            Expr::Application { func, arg } => {
                let func1 = Self::from_expr_help(*func, vars.clone())?;
                let arg1 = Self::from_expr_help(*arg, vars.clone())?;
                Ok(DeBruijnExpr::Application {
                    func: Box::new(func1),
                    arg: Box::new(arg1),
                })
            }
            Expr::Lambda { param, body } => {
                vars.push(param.clone());
                let body1 = Self::from_expr_help(*body, vars.clone())?;
                Ok(DeBruijnExpr::Lambda {
                    param,
                    body: Box::new(body1),
                })
            }
        }
    }
}
#[cfg(test)]
mod context_debruijn_tests {
    use super::*;

    fn test_from_expr(expr: Expr, expected: DeBruijnExpr) {
        let result = DeBruijnExpr::from_expr(expr.clone()).unwrap();
        assert_eq!(result, expected);
        // Also test to_expr round-trip
        let expr_back = result.to_expr().unwrap();
        assert_eq!(expr_back, expr);
    }

    #[test]
    fn test_simple_identity_lambda() {
        // λs => λz => z
        let expr = Expr::Lambda {
            param: "s".to_string(),
            body: Box::new(Expr::Lambda {
                param: "z".to_string(),
                body: Box::new(Expr::Var("z".to_string())),
            }),
        };
        let expected = DeBruijnExpr::Lambda {
            param: "s".to_string(),
            body: Box::new(DeBruijnExpr::Lambda {
                param: "z".to_string(),
                body: Box::new(DeBruijnExpr::Var {
                    index: 0,
                    ctx: vec!["s".to_string(), "z".to_string()],
                }),
            }),
        };
        test_from_expr(expr, expected);
    }

    #[test]
    fn test_simple_application_lambda() {
        // λs => λz => s (s z)
        let expr = Expr::Lambda {
            param: "s".to_string(),
            body: Box::new(Expr::Lambda {
                param: "z".to_string(),
                body: Box::new(Expr::Application {
                    func: Box::new(Expr::Var("s".to_string())),
                    arg: Box::new(Expr::Application {
                        func: Box::new(Expr::Var("s".to_string())),
                        arg: Box::new(Expr::Var("z".to_string())),
                    }),
                }),
            }),
        };
        let expected = DeBruijnExpr::Lambda {
            param: "s".to_string(),
            body: Box::new(DeBruijnExpr::Lambda {
                param: "z".to_string(),
                body: Box::new(DeBruijnExpr::Application {
                    func: Box::new(DeBruijnExpr::Var {
                        index: 1,
                        ctx: vec!["s".to_string(), "z".to_string()],
                    }),
                    arg: Box::new(DeBruijnExpr::Application {
                        func: Box::new(DeBruijnExpr::Var {
                            index: 1,
                            ctx: vec!["s".to_string(), "z".to_string()],
                        }),
                        arg: Box::new(DeBruijnExpr::Var {
                            index: 0,
                            ctx: vec!["s".to_string(), "z".to_string()],
                        }),
                    }),
                }),
            }),
        };
        test_from_expr(expr, expected);
    }

    #[test]
    fn test_church_add_lambda() {
        // λm => λn => λs => λz => m s (n z s)
        let expr = Expr::Lambda {
            param: "m".to_string(),
            body: Box::new(Expr::Lambda {
                param: "n".to_string(),
                body: Box::new(Expr::Lambda {
                    param: "s".to_string(),
                    body: Box::new(Expr::Lambda {
                        param: "z".to_string(),
                        body: Box::new(Expr::Application {
                            func: Box::new(Expr::Application {
                                func: Box::new(Expr::Var("m".to_string())),
                                arg: Box::new(Expr::Var("s".to_string())),
                            }),
                            arg: Box::new(Expr::Application {
                                func: Box::new(Expr::Application {
                                    func: Box::new(Expr::Var("n".to_string())),
                                    arg: Box::new(Expr::Var("z".to_string())),
                                }),
                                arg: Box::new(Expr::Var("s".to_string())),
                            }),
                        }),
                    }),
                }),
            }),
        };
        let expected = DeBruijnExpr::Lambda {
            param: "m".to_string(),
            body: Box::new(DeBruijnExpr::Lambda {
                param: "n".to_string(),
                body: Box::new(DeBruijnExpr::Lambda {
                    param: "s".to_string(),
                    body: Box::new(DeBruijnExpr::Lambda {
                        param: "z".to_string(),
                        body: Box::new(DeBruijnExpr::Application {
                            func: Box::new(DeBruijnExpr::Application {
                                func: Box::new(DeBruijnExpr::Var {
                                    index: 3,
                                    ctx: vec![
                                        "m".to_string(),
                                        "n".to_string(),
                                        "s".to_string(),
                                        "z".to_string(),
                                    ],
                                }),
                                arg: Box::new(DeBruijnExpr::Var {
                                    index: 1,
                                    ctx: vec![
                                        "m".to_string(),
                                        "n".to_string(),
                                        "s".to_string(),
                                        "z".to_string(),
                                    ],
                                }),
                            }),
                            arg: Box::new(DeBruijnExpr::Application {
                                func: Box::new(DeBruijnExpr::Application {
                                    func: Box::new(DeBruijnExpr::Var {
                                        index: 2,
                                        ctx: vec![
                                            "m".to_string(),
                                            "n".to_string(),
                                            "s".to_string(),
                                            "z".to_string(),
                                        ],
                                    }),
                                    arg: Box::new(DeBruijnExpr::Var {
                                        index: 0,
                                        ctx: vec![
                                            "m".to_string(),
                                            "n".to_string(),
                                            "s".to_string(),
                                            "z".to_string(),
                                        ],
                                    }),
                                }),
                                arg: Box::new(DeBruijnExpr::Var {
                                    index: 1,
                                    ctx: vec![
                                        "m".to_string(),
                                        "n".to_string(),
                                        "s".to_string(),
                                        "z".to_string(),
                                    ],
                                }),
                            }),
                        }),
                    }),
                }),
            }),
        };
        test_from_expr(expr, expected);
    }

    #[test]
    fn test_y_combinator_lambda() {
        // λf => (λx => f (λy => (x x) y)) (λx => f (λy => (x x) y))
        let expr = Expr::Lambda {
            param: "f".to_string(),
            body: Box::new(Expr::Application {
                func: Box::new(Expr::Lambda {
                    param: "x".to_string(),
                    body: Box::new(Expr::Application {
                        func: Box::new(Expr::Var("f".to_string())),
                        arg: Box::new(Expr::Lambda {
                            param: "y".to_string(),
                            body: Box::new(Expr::Application {
                                func: Box::new(Expr::Application {
                                    func: Box::new(Expr::Var("x".to_string())),
                                    arg: Box::new(Expr::Var("x".to_string())),
                                }),
                                arg: Box::new(Expr::Var("y".to_string())),
                            }),
                        }),
                    }),
                }),
                arg: Box::new(Expr::Lambda {
                    param: "x".to_string(),
                    body: Box::new(Expr::Application {
                        func: Box::new(Expr::Var("f".to_string())),
                        arg: Box::new(Expr::Lambda {
                            param: "y".to_string(),
                            body: Box::new(Expr::Application {
                                func: Box::new(Expr::Application {
                                    func: Box::new(Expr::Var("x".to_string())),
                                    arg: Box::new(Expr::Var("x".to_string())),
                                }),
                                arg: Box::new(Expr::Var("y".to_string())),
                            }),
                        }),
                    }),
                }),
            }),
        };

        let result = DeBruijnExpr::from_expr(expr.clone()).unwrap();
        let expr_back = result.to_expr().unwrap();
        assert_eq!(expr_back, expr);
    }

    #[test]
    fn test_shadowing_lambda() {
        // (λx => (λx => x)) (λx => x)
        let expr = Expr::Application {
            func: Box::new(Expr::Lambda {
                param: "x".to_string(),
                body: Box::new(Expr::Lambda {
                    param: "x".to_string(),
                    body: Box::new(Expr::Var("x".to_string())),
                }),
            }),
            arg: Box::new(Expr::Lambda {
                param: "x".to_string(),
                body: Box::new(Expr::Var("x".to_string())),
            }),
        };

        let result = DeBruijnExpr::from_expr(expr.clone()).unwrap();
        let expr_back = result.to_expr().unwrap();
        assert_eq!(expr_back, expr);
    }
}
