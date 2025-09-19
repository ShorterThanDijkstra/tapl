use std::cmp::{max, min};

use crate::lexer::{CoordToken, Lexer, LexerError, Token};
use crate::syntax::{
    CoordDec, CoordExpr, CoordFuncDec, CoordProgram, CoordType, CoordTypeDec, Dec, Expr, FuncDec,
    Program, Type, TypeDec,
};
#[derive(Debug, PartialEq)]
pub enum ParseError {
    LexerError(LexerError),
    UnexpectedToken { expected: String, found: String },
    UnexpectedEof { expected: String },
    InvalidSyntax { message: String },
}

impl From<LexerError> for ParseError {
    fn from(error: LexerError) -> Self {
        ParseError::LexerError(error)
    }
}

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<CoordToken>,
    position: usize,
}

enum ExprWraper {
    Epsilon,
    Atom(Box<CoordExpr>), // for x, lambda x: Ty => e, (e)
    Application {
        func: Box<CoordExpr>,
        arg: Box<CoordExpr>,
    }, // for application
}

impl Parser {
    pub fn new(input: &str) -> Result<Self, ParseError> {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize()?;
        Ok(Self {
            tokens,
            position: 0,
        })
    }

    fn peek(&self) -> Option<&CoordToken> {
        self.tokens.get(self.position)
    }

    fn advance(&mut self) -> Option<&CoordToken> {
        let coord_token = self.tokens.get(self.position);
        if coord_token.is_some() && !coord_token.unwrap().is_token(Token::EOF) {
            self.position += 1;
        }
        coord_token
    }

    fn expect_token(&mut self, expected: Token) -> Result<&CoordToken, ParseError> {
        let coord_token = self.advance().ok_or(ParseError::UnexpectedToken {
            expected: expected.to_string(),
            found: "EOF".to_string(),
        })?;
        if coord_token.is_token(expected.clone()) {
            Ok(coord_token)
        } else {
            Err(ParseError::UnexpectedToken {
                expected: expected.to_string(),
                found: coord_token.to_string(),
            })
        }
    }

    fn expect_identifier(&mut self) -> Result<&CoordToken, ParseError> {
        let coord_token = self.advance().ok_or(ParseError::UnexpectedToken {
            expected: Token::Identifier("?".to_string()).to_string(),
            found: Token::EOF.to_string(),
        })?;
        match coord_token.token {
            Token::Identifier(_) => Ok(&coord_token),
            _ => Err(ParseError::UnexpectedToken {
                expected: Token::Identifier("?".to_string()).to_string(),
                found: coord_token.to_string(),
            }),
        }
    }

    // Program := Dec {Dec}
    pub fn parse_program(&mut self) -> Result<CoordProgram, ParseError> {
        let mut coord_decs = Vec::new();
        let mut main = None;
        while let Some(token) = self.peek() {
            if token.is_token(Token::EOF) {
                break;
            }
            let coord_dec = self.parse_dec()?;
            if !Self::is_main_dec(&coord_dec) {
                coord_decs.push(coord_dec);
            } else if main.is_none() {
                main = Some(coord_dec);
            } else {
                return Err(ParseError::InvalidSyntax {
                    message: "Program must contain at exactly one main declaration".to_string(),
                });
            }
        }

        if main.is_none() {
            return Err(ParseError::InvalidSyntax {
                message: "Program must contain at exactly one main declaration".to_string(),
            });
        }

        let program = Program {
            decs: coord_decs
                .iter()
                .map(|coord_dec| coord_dec.dec.clone())
                .collect(),
            main: main.clone().unwrap().dec,
        };
        let mut row_start = main.clone().unwrap().row_start;
        let mut col_start = main.clone().unwrap().col_start;
        let mut row_end = main.clone().unwrap().row_end;
        let mut col_end = main.clone().unwrap().col_end;
        for dec in coord_decs {
            if row_start <= dec.row_start {
                row_start = dec.row_start;
                col_start = min(col_start, dec.col_start);
            }
            if row_end >= dec.row_end {
                row_end = dec.row_end;
                col_end = max(col_end, dec.col_end);
            }
        }
        let coord_prog = CoordProgram {
            program,
            row_start,
            row_end,
            col_start,
            col_end,
        };
        Ok(coord_prog)
    }

    fn is_main_dec(coord_dec: &CoordDec) -> bool {
        let dec = coord_dec.dec.clone();
        return dec.name == "main" && dec.ty_dec.ty == Type::Atom("Unit".to_string());
    }

    // Dec := TypeDec "\n" FuncDec
    fn parse_dec(&mut self) -> Result<CoordDec, ParseError> {
        let coord_ty_dec = self.parse_type_dec()?;
        let coord_func_dec = self.parse_func_dec()?;

        if coord_ty_dec.col_start != 0 {
            return Err(ParseError::InvalidSyntax {
                message: format!(
                    "Type declaration must start at the beginning of a line, found at row {}, column {}",
                    coord_ty_dec.row_start, coord_ty_dec.col_start,
                ),
            });
        }

        if coord_func_dec.col_start != 0 {
            return Err(ParseError::InvalidSyntax {
                message: format!(
                    "Function declaration must start at the beginning of a line, found at row {}, column {}",
                    coord_func_dec.row_start, coord_func_dec.col_start,
                ),
            });
        }
        if coord_ty_dec.type_dec.name != coord_func_dec.func_dec.name {
            return Err(ParseError::InvalidSyntax {
                message: format!(
                    "Type declaration name '{}' does not match function declaration name '{}'",
                    coord_ty_dec.type_dec.name, coord_func_dec.func_dec.name
                ),
            });
        }
        let dec = Dec {
            name: coord_ty_dec.type_dec.name.clone(),
            ty_dec: coord_ty_dec.type_dec,
            func_dec: coord_func_dec.func_dec,
        };
        let row_start = coord_ty_dec.row_start;
        let col_start = coord_ty_dec.col_start;
        let row_end = coord_func_dec.row_end;
        let col_end = coord_func_dec.col_end;
        let coord_dec = CoordDec {
            dec,
            row_start,
            col_start,
            row_end,
            col_end,
        };
        Ok(coord_dec)
    }

    // TypeDec := Identifier : Type
    fn parse_type_dec(&mut self) -> Result<CoordTypeDec, ParseError> {
        if let CoordToken {
            token: Token::Identifier(name),
            row,
            col,
            ..
        } = self.expect_identifier()?
        {
            let name = name.clone();
            let row_start = *row;
            let col_start = *col;
            self.expect_token(Token::Colon)?;
            let coord_ty = self.parse_type()?;
            let type_dec = TypeDec {
                name,
                ty: coord_ty.ty,
            };
            let row_end = coord_ty.row_end;
            let col_end = coord_ty.col_end;
            let coord_ty_dec = CoordTypeDec {
                type_dec,
                row_start,
                col_start,
                row_end,
                col_end,
            };
            Ok(coord_ty_dec)
        } else {
            return Err(ParseError::InvalidSyntax {
                message: "Type declaration expects identifier".to_string(),
            });
        }
    }

    // FuncDec := Identifier = Expr
    fn parse_func_dec(&mut self) -> Result<CoordFuncDec, ParseError> {
        if let CoordToken {
            token: Token::Identifier(name),
            row,
            col,
            ..
        } = self.expect_identifier()?
        {
            let name = name.clone();
            let row_start = *row;
            let col_start = *col;
            self.expect_token(Token::Equals)?;
            let body = self.parse_expr()?;
            let func_dec = FuncDec {
                name,
                body: body.expr,
            };
            let row_end = body.row_end;
            let col_end = body.col_end;
            let coord_func_dec = CoordFuncDec {
                func_dec,
                row_start,
                col_start,
                row_end,
                col_end,
            };
            Ok(coord_func_dec)
        } else {
            return Err(ParseError::InvalidSyntax {
                message: "Type declaration expects identifier".to_string(),
            });
        }
    }

    // Type := Identifier | (Type) | Type -> Type
    // Right-associative: A -> B -> C is A -> (B -> C)
    fn parse_type(&mut self) -> Result<CoordType, ParseError> {
        let peek = self.peek();
        match peek {
            None => Err(ParseError::InvalidSyntax {
                message: "expects a token".to_string(),
            }),
            Some(CoordToken {
                token: Token::Identifier(_),
                ..
            }) => self.parse_type_ident(),
            Some(CoordToken {
                token: Token::LeftParen,
                ..
            }) => self.parse_type_paren(),
            _ => self.parse_type_arrow(),
        }
    }
    // Type := ( Type ) [-> Type]
    fn parse_type_paren(&mut self) -> Result<CoordType, ParseError> {
        let CoordToken { row, col, .. } = self.expect_token(Token::LeftParen)?;
        let row_start = *row;
        let col_start = *col;
        let ty = self.parse_type()?;
        let CoordToken { row, col, size, .. } = self.expect_token(Token::RightParen)?;
        let row_end = *row;
        let col_end = *col + *size - 1;
        let next = self.peek();
        match next {
            None => Err(ParseError::InvalidSyntax {
                message: "expects a token".to_string(),
            }),
            Some(CoordToken {
                token: Token::RightArrow,
                ..
            }) => {
                let CoordType {
                    ty: right,
                    row_end: row_end1,
                    col_end: col_end1,
                    ..
                } = self.parse_type_arrow()?;
                let ty = Type::Function {
                    input: Box::new(ty.ty),
                    output: Box::new(right),
                };
                let coord_ty = CoordType {
                    ty,
                    row_start,
                    col_start,
                    row_end: row_end1,
                    col_end: col_end1,
                };
                Ok(coord_ty)
            }
            Some(_) => {
                let coord_ty = CoordType {
                    ty: ty.ty,
                    row_start,
                    col_start,
                    row_end,
                    col_end,
                };
                Ok(coord_ty)
            }
        }
    }

    // Type := Identifier [-> Type]
    fn parse_type_ident(&mut self) -> Result<CoordType, ParseError> {
        let peek = self.peek();
        match peek {
            None => Err(ParseError::InvalidSyntax {
                message: "expects a token".to_string(),
            }),
            Some(CoordToken {
                token: Token::Identifier(name),
                row,
                col,
                size,
            }) => {
                let name = name.clone();
                let row_start = *row;
                let col_start = *col;
                let size = *size;
                self.advance();
                let ty = Type::Atom(name);
                let next = self.peek();
                match next {
                    None => Err(ParseError::InvalidSyntax {
                        message: "expects a token".to_string(),
                    }),
                    Some(CoordToken {
                        token: Token::RightArrow,
                        ..
                    }) => {
                        let CoordType {
                            ty: right,
                            row_end,
                            col_end,
                            ..
                        } = self.parse_type_arrow()?;
                        let ty = Type::Function {
                            input: Box::new(ty),
                            output: Box::new(right),
                        };
                        let coord_ty = CoordType {
                            ty,
                            row_start,
                            col_start,
                            row_end: row_end,
                            col_end: col_end,
                        };
                        Ok(coord_ty)
                    }
                    Some(_) => {
                        let coord_ty = CoordType {
                            ty,
                            row_start,
                            col_start,
                            row_end: row_start,
                            col_end: col_start + size - 1,
                        };

                        Ok(coord_ty)
                    }
                }
            }
            Some(t) => Err(ParseError::UnexpectedToken {
                expected: "Identifier".to_string(),
                found: t.to_string(),
            }),
        }
    }

    // Type := -> Type
    fn parse_type_arrow(&mut self) -> Result<CoordType, ParseError> {
        let peek = self.peek();
        match peek {
            None => Err(ParseError::InvalidSyntax {
                message: "expects a token".to_string(),
            }),
            Some(CoordToken {
                token: Token::RightArrow,
                ..
            }) => {
                self.advance();
                let ty = self.parse_type()?;
                Ok(ty)
            }
            Some(t) => Err(ParseError::UnexpectedToken {
                expected: "Identifier".to_string(),
                found: t.to_string(),
            }),
        }
    }

    // Expr := Var | λ Identifier => Expr | (Expr) | Expr Expr
    fn parse_expr(&mut self) -> Result<CoordExpr, ParseError> {
        let expr = self.parse_expr_wrapper()?;
        match expr {
            ExprWraper::Epsilon => Err(ParseError::InvalidSyntax {
                message: "expects an expression".to_string(),
            }),
            ExprWraper::Atom(e) => Ok(*e),
            ExprWraper::Application { func, arg } => {
                let row_start = func.row_start;
                let col_start = func.col_start;
                let row_end = arg.row_end;
                let col_end = arg.col_end;
                let expr = Expr::Application {
                    func: Box::new(func.expr),
                    arg: Box::new(arg.expr),
                };
                let coord_expr = CoordExpr {
                    expr,
                    row_start,
                    col_start,
                    row_end,
                    col_end,
                };
                Ok(coord_expr)
            }
        }
    }

    // Expr := Expr' {Expr'}
    fn parse_expr_wrapper(&mut self) -> Result<ExprWraper, ParseError> {
        let peek = self.peek();
        match peek {
            None => Err(ParseError::InvalidSyntax {
                message: "expects an expression".to_string(),
            }),
            Some(CoordToken {
                token: Token::Identifier(_),
                ..
            }) => Ok(self.parse_expr_var()?),
            Some(CoordToken {
                token: Token::Lambda,
                ..
            }) => Ok(self.parse_expr_lambda()?),
            Some(CoordToken {
                token: Token::LeftParen,
                ..
            }) => Ok(self.parse_expr_paren()?),
            _ => Ok(ExprWraper::Epsilon),
        }
    }

    // Expr := Var {Expr'}
    fn parse_expr_var(&mut self) -> Result<ExprWraper, ParseError> {
        let peek = self.peek();
        match peek {
            None => Err(ParseError::InvalidSyntax {
                message: "expects an expression".to_string(),
            }),
            Some(CoordToken {
                token: Token::Identifier(name),
                row,
                col,
                size,
            }) => {
                let row_start = *row;
                let col_start = *col;
                let size = *size;
                let expr = CoordExpr {
                    expr: Expr::Var(name.clone()),
                    row_start,
                    col_start,
                    row_end: row_start,
                    col_end: col_start + size - 1,
                };

                self.advance();
                let next = self.parse_expr_wrapper()?;
                match next {
                    ExprWraper::Epsilon => Ok(ExprWraper::Atom(Box::new(expr))),
                    ExprWraper::Atom(atom) => {
                        let ew = ExprWraper::Application {
                            func: Box::new(expr),
                            arg: atom,
                        };
                        Ok(ew)
                    }
                    ExprWraper::Application { func, arg } => {
                        let app1 = Expr::Application {
                            func: Box::new(expr.expr),
                            arg: Box::new(func.expr),
                        };
                        let coord_app1 = CoordExpr {
                            expr: app1,
                            row_start: expr.row_start,
                            col_start: expr.col_start,
                            row_end: func.row_end,
                            col_end: func.col_end,
                        };

                        let ew = ExprWraper::Application {
                            func: Box::new(coord_app1),
                            arg: arg,
                        };
                        Ok(ew)
                    }
                }
            }
            Some(t) => Err(ParseError::UnexpectedToken {
                expected: "Identifier".to_string(),
                found: t.to_string(),
            }),
        }
    }

    // Expr := (λ Identifier => Expr) {Expr'}
    fn parse_expr_lambda(&mut self) -> Result<ExprWraper, ParseError> {
        let peek = self.peek();
        match peek {
            None => Err(ParseError::InvalidSyntax {
                message: "expects Lambda".to_string(),
            }),
            Some(CoordToken {
                token: Token::Lambda,
                row,
                col,
                ..
            }) => {
                let row_start = *row;
                let col_start = *col;
                self.advance();
                let ident = self.expect_identifier()?;
                match ident {
                    CoordToken {
                        token: Token::Identifier(name),
                        ..
                    } => {
                        let name = name.clone();
                        self.expect_token(Token::RightArrowDouble)?;
                        let body = self.parse_expr()?;
                        let lambda = Expr::Lambda {
                            param: name,
                            body: Box::new(body.expr),
                        };
                        let coord_lambda = CoordExpr {
                            expr: lambda,
                            row_start,
                            col_start,
                            row_end: body.row_end,
                            col_end: body.col_end,
                        };
                        Ok(ExprWraper::Atom(Box::new(coord_lambda)))
                    }
                    _ => Err(ParseError::UnexpectedToken {
                        expected: "Identifier".to_string(),
                        found: ident.to_string(),
                    }),
                }
            }
            Some(t) => Err(ParseError::UnexpectedToken {
                expected: Token::Identifier("?".to_string()).to_string(),
                found: t.to_string(),
            }),
        }
    }

    // Expr := Expr Expr
    fn parse_expr_app(&mut self) -> Result<ExprWraper, ParseError> {
        let func = self.parse_expr()?;
        let arg = self.parse_expr()?;
        let next = self.parse_expr_wrapper()?;
        match next {
            ExprWraper::Epsilon => {
                let ew = ExprWraper::Application {
                    func: Box::new(func),
                    arg: Box::new(arg),
                };
                Ok(ew)
            }
            ExprWraper::Atom(atom) => {
                let app = Expr::Application {
                    func: Box::new(func.expr),
                    arg: Box::new(arg.expr),
                };
                let coord_app = CoordExpr {
                    expr: app,
                    row_start: func.row_start,
                    col_start: func.col_start,
                    row_end: arg.row_end,
                    col_end: arg.col_end,
                };
                let ew = ExprWraper::Application {
                    func: Box::new(coord_app),
                    arg: atom,
                };
                Ok(ew)
            }
            ExprWraper::Application {
                func: func1,
                arg: arg1,
            } => {
                let app = Expr::Application {
                    func: Box::new(func.expr),
                    arg: Box::new(arg.expr),
                };
                let coor_app = CoordExpr {
                    expr: app,
                    row_start: func.row_start,
                    col_start: func.col_start,
                    row_end: arg.row_end,
                    col_end: arg.col_end,
                };
                let app1 = Expr::Application {
                    func: Box::new(coor_app.expr),
                    arg: Box::new(func1.expr),
                };
                let coord_app1 = CoordExpr {
                    expr: app1,
                    row_start: coor_app.row_start,
                    col_start: coor_app.col_start,
                    row_end: func1.row_end,
                    col_end: func1.col_end,
                };
                let ew = ExprWraper::Application {
                    func: Box::new(coord_app1),
                    arg: arg1,
                };
                Ok(ew)
            }
        }
    }

    // Expr := (Expr) {Expr'}
    fn parse_expr_paren(&mut self) -> Result<ExprWraper, ParseError> {
        let peek = self.peek();
        match peek {
            None => Err(ParseError::InvalidSyntax {
                message: "expects LeftParen".to_string(),
            }),
            Some(CoordToken {
                token: Token::LeftParen,
                row,
                col,
                ..
            }) => {
                let row_start = *row;
                let col_start = *col;
                self.advance();
                let mut expr = self.parse_expr()?;
                let CoordToken {
                    row: row1,
                    col: col1,
                    size,
                    ..
                } = self.expect_token(Token::RightParen)?;
                let row_end = *row1;
                let col_end = *col1 + *size - 1;
                expr.row_start = row_start;
                expr.col_start = col_start;
                expr.row_end = row_end;
                expr.col_end = col_end;

                let next = self.parse_expr_wrapper()?;
                match next {
                    ExprWraper::Epsilon => Ok(ExprWraper::Atom(Box::new(expr))),

                    ExprWraper::Atom(atom) => {
                        let ew = ExprWraper::Application {
                            func: Box::new(expr),
                            arg: atom,
                        };
                        Ok(ew)
                    }
                    ExprWraper::Application { func, arg } => {
                        let app = Expr::Application {
                            func: Box::new(expr.expr),
                            arg: Box::new(func.expr),
                        };
                        let coord_app = CoordExpr {
                            expr: app,
                            row_start: expr.row_start,
                            col_start: expr.col_start,
                            row_end: func.row_end,
                            col_end: func.col_end,
                        };
                        let ew = ExprWraper::Application {
                            func: Box::new(coord_app),
                            arg: arg,
                        };
                        Ok(ew)
                    }
                }
            }
            Some(t) => Err(ParseError::UnexpectedToken {
                expected: Token::Lambda.to_string(),
                found: t.to_string(),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn test_parse_type(s: &str) {
        let mut parser = Parser::new(s).unwrap();
        let ty = parser.parse_type().unwrap();
        println!("{}", ty.to_string())
    }

    #[test]
    fn test_parse_type1() {
        test_parse_type("Bool");
    }

    #[test]
    fn test_parse_type2() {
        test_parse_type("\n(Bool)");
    }

    #[test]
    fn test_parse_type3() {
        test_parse_type("Bool -> Int");
    }

    #[test]
    fn test_parse_type4() {
        test_parse_type("Bool -> Int -> String");
    }

    #[test]
    fn test_parse_type5() {
        test_parse_type("(Bool -> Int) -> String");
    }

    #[test]
    fn test_parse_type6() {
        test_parse_type("(Bool -> (Int)) -> String");
    }

    fn test_parse_expr(s: &str) {
        let mut parser = Parser::new(s).unwrap();
        let expr = parser.parse_expr().unwrap();
        println!("{}", expr)
    }

    #[test]
    fn test_parse_expr1() {
        test_parse_expr("x");
    }

    #[test]
    fn test_parse_expr2() {
        test_parse_expr("(\\x => x)");
    }

    #[test]
    fn test_parse_expr3() {
        test_parse_expr("(\\x => x)(\\y => y)");
    }

    #[test]
    fn test_parse_expr4() {
        let s = r#"(\x => x)(\y => y)(\z => z)"#;
        test_parse_expr(s);
    }
}
