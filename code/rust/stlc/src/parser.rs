use std::cmp::{max, min};
use std::fs;
use std::path::Path;

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
    pub fn from_file<P: AsRef<Path>>(file_path: P) -> Result<Self, ParseError> {
        let content = fs::read_to_string(file_path).map_err(|e| ParseError::InvalidSyntax {
            message: format!("Failed to read file: {}", e),
        })?;
        Self::from_str(&content)
    }
    
    pub fn from_str(input: &str) -> Result<Self, ParseError> {
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
            println!("{}", coord_dec);
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
            if row_start >= dec.row_start {
                row_start = dec.row_start;
                col_start = min(col_start, dec.col_start);
            }
            if row_end <= dec.row_end {
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

        if coord_ty_dec.col_start != 1 {
            return Err(ParseError::InvalidSyntax {
                message: format!(
                    "Type declaration must start at the beginning of a line, found at row {}, column {}",
                    coord_ty_dec.row_start, coord_ty_dec.col_start,
                ),
            });
        }

        if coord_func_dec.col_start != 1 {
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

    // ExprWrap := Var | λ Identifier => Expr | (Expr) | Epsilon
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
                let peek = self.peek();
                if peek.is_some() && peek.unwrap().col == 1 {
                    Ok(ExprWraper::Atom(Box::new(expr)))
                } else {
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
            }
            Some(t) => Err(ParseError::UnexpectedToken {
                expected: "Identifier".to_string(),
                found: t.to_string(),
            }),
        }
    }

    // Expr := λ Identifier => Expr
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

                let peek = self.peek();
                if peek.is_some() && peek.unwrap().col == 1 {
                    return Ok(ExprWraper::Atom(Box::new(expr)));
                }
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
    use crate::syntax::*;

    // Helper function to create a parser and test type parsing
    fn test_parse_type(s: &str) -> Result<CoordType, ParseError> {
        let mut parser = Parser::from_str(s)?;
        parser.parse_type()
    }

    // Helper function to create a parser and test expression parsing
    fn test_parse_expr(s: &str) -> Result<CoordExpr, ParseError> {
        let mut parser = Parser::from_str(s)?;
        parser.parse_expr()
    }

    // Helper function to create a parser and test program parsing
    fn test_parse_program(s: &str) -> Result<CoordProgram, ParseError> {
        let mut parser = Parser::from_str(s)?;
        parser.parse_program()
    }

    // Type parsing tests
    #[test]
    fn test_parse_type_atom() {
        let result = test_parse_type("Bool").unwrap();
        assert_eq!(result.ty, Type::Atom("Bool".to_string()));
        assert_eq!(result.row_start, 1);
        assert_eq!(result.col_start, 1);
    }

    #[test]
    fn test_parse_type_parenthesized() {
        let result = test_parse_type("(Bool)").unwrap();
        assert_eq!(result.ty, Type::Atom("Bool".to_string()));
    }

    #[test]
    fn test_parse_type_function_simple() {
        let result = test_parse_type("Bool -> Int").unwrap();
        match result.ty {
            Type::Function { input, output } => {
                assert_eq!(*input, Type::Atom("Bool".to_string()));
                assert_eq!(*output, Type::Atom("Int".to_string()));
            }
            _ => panic!("Expected function type"),
        }
    }

    #[test]
    fn test_parse_type_function_right_associative() {
        let result = test_parse_type("Bool -> Int -> String").unwrap();
        match result.ty {
            Type::Function { input, output } => {
                assert_eq!(*input, Type::Atom("Bool".to_string()));
                match *output {
                    Type::Function {
                        input: inner_input,
                        output: inner_output,
                    } => {
                        assert_eq!(*inner_input, Type::Atom("Int".to_string()));
                        assert_eq!(*inner_output, Type::Atom("String".to_string()));
                    }
                    _ => panic!("Expected nested function type"),
                }
            }
            _ => panic!("Expected function type"),
        }
    }

    #[test]
    // test type for Bool -> \nInt
    fn test_parse_type_function_newline() {
        let result = test_parse_type("Bool -> \nInt").unwrap();
        match result.ty {
            Type::Function { input, output } => {
                assert_eq!(*input, Type::Atom("Bool".to_string()));
                assert_eq!(*output, Type::Atom("Int".to_string()));
            }
            _ => panic!("Expected function type"),
        }
    }

    #[test]
    fn test_parse_type_function_left_parentheses() {
        let result = test_parse_type("(Bool -> Int) -> String").unwrap();
        match result.ty {
            Type::Function { input, output } => {
                match *input {
                    Type::Function {
                        input: inner_input,
                        output: inner_output,
                    } => {
                        assert_eq!(*inner_input, Type::Atom("Bool".to_string()));
                        assert_eq!(*inner_output, Type::Atom("Int".to_string()));
                    }
                    _ => panic!("Expected nested function type in input"),
                }
                assert_eq!(*output, Type::Atom("String".to_string()));
            }
            _ => panic!("Expected function type"),
        }
    }

    #[test]
    fn test_parse_type_complex_parentheses() {
        let result = test_parse_type("(Bool -> (Int)) -> String").unwrap();
        match result.ty {
            Type::Function { input, output } => {
                match *input {
                    Type::Function {
                        input: inner_input,
                        output: inner_output,
                    } => {
                        assert_eq!(*inner_input, Type::Atom("Bool".to_string()));
                        assert_eq!(*inner_output, Type::Atom("Int".to_string()));
                    }
                    _ => panic!("Expected nested function type in input"),
                }
                assert_eq!(*output, Type::Atom("String".to_string()));
            }
            _ => panic!("Expected function type"),
        }
    }

    #[test]
    fn test_parse_type_error_empty() {
        let result = test_parse_type("");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_type_error_unmatched_paren() {
        let result = test_parse_type("(Bool");
        assert!(result.is_err());
    }

    // Expression parsing tests
    #[test]
    fn test_parse_expr_variable() {
        let result = test_parse_expr("x").unwrap();
        assert_eq!(result.expr, Expr::Var("x".to_string()));
        assert_eq!(result.row_start, 1);
        assert_eq!(result.col_start, 1);
    }

    #[test]
    fn test_parse_expr_lambda_simple() {
        let result = test_parse_expr("\\x => x").unwrap();
        match result.expr {
            Expr::Lambda { param, body } => {
                assert_eq!(param, "x");
                assert_eq!(*body, Expr::Var("x".to_string()));
            }
            _ => panic!("Expected lambda expression"),
        }
    }

    #[test]
    fn test_parse_expr_lambda_unicode() {
        let result = test_parse_expr("λx => x").unwrap();
        match result.expr {
            Expr::Lambda { param, body } => {
                assert_eq!(param, "x");
                assert_eq!(*body, Expr::Var("x".to_string()));
            }
            _ => panic!("Expected lambda expression"),
        }
    }

    #[test]
    fn test_parse_expr_parenthesized() {
        let result = test_parse_expr("(x)").unwrap();
        assert_eq!(result.expr, Expr::Var("x".to_string()));
    }

    #[test]
    fn test_parse_expr_parenthesized_lambda() {
        let result = test_parse_expr("(\\x => x)").unwrap();
        match result.expr {
            Expr::Lambda { param, body } => {
                assert_eq!(param, "x");
                assert_eq!(*body, Expr::Var("x".to_string()));
            }
            _ => panic!("Expected lambda expression"),
        }
    }

    #[test]
    fn test_parse_expr_application_simple() {
        let result = test_parse_expr("f x").unwrap();
        match result.expr {
            Expr::Application { func, arg } => {
                assert_eq!(*func, Expr::Var("f".to_string()));
                assert_eq!(*arg, Expr::Var("x".to_string()));
            }
            _ => panic!("Expected application"),
        }
    }

    #[test]
    fn test_parse_expr_application_left_associative() {
        let result = test_parse_expr("f x y").unwrap();
        match result.expr {
            Expr::Application { func, arg } => {
                match *func {
                    Expr::Application {
                        func: inner_func,
                        arg: inner_arg,
                    } => {
                        assert_eq!(*inner_func, Expr::Var("f".to_string()));
                        assert_eq!(*inner_arg, Expr::Var("x".to_string()));
                    }
                    _ => panic!("Expected nested application in func"),
                }
                assert_eq!(*arg, Expr::Var("y".to_string()));
            }
            _ => panic!("Expected application"),
        }
    }

    #[test]
    fn test_parse_expr_lambda_application() {
        let result = test_parse_expr("(\\x => x)(\\y => y)").unwrap();
        match result.expr {
            Expr::Application { func, arg } => match (*func, *arg) {
                (
                    Expr::Lambda {
                        param: p1,
                        body: b1,
                    },
                    Expr::Lambda {
                        param: p2,
                        body: b2,
                    },
                ) => {
                    assert_eq!(p1, "x");
                    assert_eq!(*b1, Expr::Var("x".to_string()));
                    assert_eq!(p2, "y");
                    assert_eq!(*b2, Expr::Var("y".to_string()));
                }
                _ => panic!("Expected lambda applications"),
            },
            _ => panic!("Expected application"),
        }
    }

    #[test]
    fn test_parse_expr_complex_application() {
        let result = test_parse_expr("(\\x => x)(\\y => y)(\\z => z)").unwrap();
        match result.expr {
            Expr::Application { func, arg } => {
                match *func {
                    Expr::Application {
                        func: inner_func,
                        arg: inner_arg,
                    } => match (*inner_func, *inner_arg) {
                        (Expr::Lambda { param: p1, .. }, Expr::Lambda { param: p2, .. }) => {
                            assert_eq!(p1, "x");
                            assert_eq!(p2, "y");
                        }
                        _ => panic!("Expected lambda applications in inner"),
                    },
                    _ => panic!("Expected nested application in func"),
                }
                match *arg {
                    Expr::Lambda { param: p3, .. } => {
                        assert_eq!(p3, "z");
                    }
                    _ => panic!("Expected lambda in arg"),
                }
            }
            _ => panic!("Expected application"),
        }
    }

    #[test]
    fn test_parse_expr_nested_lambda() {
        let result = test_parse_expr("\\x => \\y => x").unwrap();
        match result.expr {
            Expr::Lambda { param, body } => {
                assert_eq!(param, "x");
                match *body {
                    Expr::Lambda {
                        param: inner_param,
                        body: inner_body,
                    } => {
                        assert_eq!(inner_param, "y");
                        assert_eq!(*inner_body, Expr::Var("x".to_string()));
                    }
                    _ => panic!("Expected nested lambda"),
                }
            }
            _ => panic!("Expected lambda expression"),
        }
    }

    #[test]
    fn test_parse_expr_error_empty() {
        let result = test_parse_expr("");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_expr_error_unmatched_paren() {
        let result = test_parse_expr("(x");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_expr_error_lambda_missing_param() {
        let result = test_parse_expr("\\");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_expr_error_lambda_missing_arrow() {
        let result = test_parse_expr("\\x");
        assert!(result.is_err());
    }

    // Declaration parsing tests
    #[test]
    fn test_parse_type_dec_simple() {
        let mut parser = Parser::from_str("f : Bool").unwrap();
        let result = parser.parse_type_dec().unwrap();
        assert_eq!(result.type_dec.name, "f");
        assert_eq!(result.type_dec.ty, Type::Atom("Bool".to_string()));
    }

    #[test]
    fn test_parse_func_dec_simple() {
        let mut parser = Parser::from_str("f = x").unwrap();
        let result = parser.parse_func_dec().unwrap();
        assert_eq!(result.func_dec.name, "f");
        assert_eq!(result.func_dec.body, Expr::Var("x".to_string()));
    }

    #[test]
    fn test_parse_dec_complete() {
        let input = "f : Bool -> Bool\nf = \\x => x";
        let mut parser = Parser::from_str(input).unwrap();
        let result = parser.parse_dec().unwrap();

        assert_eq!(result.dec.name, "f");
        match result.dec.ty_dec.ty {
            Type::Function { input, output } => {
                assert_eq!(*input, Type::Atom("Bool".to_string()));
                assert_eq!(*output, Type::Atom("Bool".to_string()));
            }
            _ => panic!("Expected function type"),
        }

        match result.dec.func_dec.body {
            Expr::Lambda { param, body } => {
                assert_eq!(param, "x");
                assert_eq!(*body, Expr::Var("x".to_string()));
            }
            _ => panic!("Expected lambda expression"),
        }
    }

    #[test]
    fn test_parse_dec_error_mismatched_names() {
        let input = "f : Bool\ng = x";
        let mut parser = Parser::from_str(input).unwrap();
        let result = parser.parse_dec();
        assert!(result.is_err());
        match result.unwrap_err() {
            ParseError::InvalidSyntax { message } => {
                assert!(message.contains("does not match"));
            }
            _ => panic!("Expected InvalidSyntax error"),
        }
    }

    // Program parsing tests
    #[test]
    fn test_parse_program_simple() {
        let input = r#"
id : Bool -> Bool
id = \x => x

main : Unit
main = id
"#;
        let result = test_parse_program(input).unwrap();

        assert_eq!(result.program.decs.len(), 1);
        assert_eq!(result.program.decs[0].name, "id");
        assert_eq!(result.program.main.name, "main");
        assert_eq!(
            result.program.main.ty_dec.ty,
            Type::Atom("Unit".to_string())
        );
    }

    #[test]
    fn test_parse_program_multiple_decs() {
        let input = r#"
id : Bool -> Bool
id = \x => x

const : Bool -> Bool -> Bool
const = \x => \y => x

main : Unit
main = id
"#;
        let result = test_parse_program(input).unwrap();

        assert_eq!(result.program.decs.len(), 2);
        assert_eq!(result.program.decs[0].name, "id");
        assert_eq!(result.program.decs[1].name, "const");
        assert_eq!(result.program.main.name, "main");
    }

    #[test]
    fn test_parse_program_error_no_main() {
        let input = r#"
id : Bool -> Bool
id = \x => x
"#;
        let result = test_parse_program(input);
        assert!(result.is_err());
        match result.unwrap_err() {
            ParseError::InvalidSyntax { message } => {
                assert!(message.contains("main declaration"));
            }
            _ => panic!("Expected InvalidSyntax error"),
        }
    }

    #[test]
    fn test_parse_program_error_multiple_main() {
        let input = r#"
main : Unit
main = x

main : Unit  
main = y
"#;
        let result = test_parse_program(input);
        assert!(result.is_err());
        match result.unwrap_err() {
            ParseError::InvalidSyntax { message } => {
                assert!(message.contains("exactly one main"));
            }
            _ => panic!("Expected InvalidSyntax error"),
        }
    }

    #[test]
    fn test_parse_program_error_main_wrong_type() {
        let input = r#"
main : Bool
main = x
"#;
        let result = test_parse_program(input);
        // This should actually parse successfully since is_main_dec only checks for "Unit" type
        // But if main has wrong type, it won't be recognized as main, causing "no main" error
        assert!(result.is_err());
    }

    // Integration tests with comments and whitespace
    #[test]
    fn test_parse_program_with_comments() {
        let input = r#"
-- Identity function
id : Bool -> Bool
id = \x => x

-- Main program
main : Unit
main = id  -- Apply identity
"#;
        let result = test_parse_program(input).unwrap();
        assert_eq!(result.program.decs.len(), 1);
        assert_eq!(result.program.main.name, "main");
    }

    #[test]
    fn test_parse_program_mixed_whitespace() {
        let input = "id : Bool -> Bool\nid = \\x => x\n\nmain : Unit\nmain = id";
        let result = test_parse_program(input).unwrap();
        assert_eq!(result.program.decs.len(), 1);
        assert_eq!(result.program.main.name, "main");
    }

    #[test]
    fn test_parse_program_complex_types() {
        let input = r#"
curry : (Bool -> Bool -> Bool) -> Bool -> Bool -> Bool
curry = \f => \x => \y => f x y

main : Unit
main = curry
"#;
        let result = test_parse_program(input).unwrap();

        // Check that the complex type parsed correctly
        match &result.program.decs[0].ty_dec.ty {
            Type::Function { input, output } => match input.as_ref() {
                Type::Function {
                    input: inner1,
                    output: inner2,
                } => {
                    assert_eq!(**inner1, Type::Atom("Bool".to_string()));
                    match inner2.as_ref() {
                        Type::Function {
                            input: inner3,
                            output: inner4,
                        } => {
                            assert_eq!(**inner3, Type::Atom("Bool".to_string()));
                            assert_eq!(**inner4, Type::Atom("Bool".to_string()));
                        }
                        _ => panic!("Expected nested function type"),
                    }
                }
                _ => panic!("Expected function type in input"),
            },
            _ => panic!("Expected function type"),
        }
    }

    // Error handling tests
    #[test]
    fn test_parser_lexer_error_propagation() {
        let result = Parser::from_str("@invalid");
        assert!(result.is_err());
        match result.unwrap_err() {
            ParseError::LexerError(_) => {} // Expected
            _ => panic!("Expected LexerError"),
        }
    }

    #[test]
    fn test_unexpected_token_error() {
        let result = test_parse_type(":");
        assert!(result.is_err());
        match result.unwrap_err() {
            ParseError::UnexpectedToken { expected, found } => {
                assert!(expected.contains("Identifier"));
            }
            _ => panic!("Expected UnexpectedToken error"),
        }
    }

    #[test]
    fn test_coordinate_tracking() {
        let input = "  f  :  Bool";
        let mut parser = Parser::from_str(input).unwrap();
        let result = parser.parse_type_dec().unwrap();

        // Should track coordinates correctly despite whitespace
        assert_eq!(result.row_start, 1);
        assert_eq!(result.col_start, 3); // 'f' starts at column 3
    }

    // Edge cases
    #[test]
    fn test_empty_program() {
        let result = test_parse_program("");
        assert!(result.is_err());
    }

    #[test]
    fn test_only_whitespace_program() {
        let result = test_parse_program("   \n  \t  ");
        assert!(result.is_err());
    }

    #[test]
    fn test_complex_lambda_calculus_program() {
        let input = r#"
-- Church booleans
true : Bool -> Bool -> Bool
true = \x => \y => x

false : Bool -> Bool -> Bool  
false = \x => \y => y

-- Boolean operations
and : (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool -> Bool -> Bool
and = \p => \q => \x => \y => p (q x y) y

main : Unit
main = true
"#;
        let result = test_parse_program(input).unwrap();
        // assert_eq!(result.program.decs.len(), 3);
        assert_eq!(result.program.main.name, "main");
    }

    // Test lambda with =>, not ->
    #[test]
    fn test_lambda_with_double_arrow() {
        let result = test_parse_expr("\\x => \\y => x").unwrap();
        match result.expr {
            Expr::Lambda { param, body } => {
                assert_eq!(param, "x");
                match *body {
                    Expr::Lambda {
                        param: inner_param,
                        body: inner_body,
                    } => {
                        assert_eq!(inner_param, "y");
                        assert_eq!(*inner_body, Expr::Var("x".to_string()));
                    }
                    _ => panic!("Expected nested lambda"),
                }
            }
            _ => panic!("Expected lambda expression"),
        }
    }

    #[test]
    fn test_lambda_wrong_arrow_type() {
        let result = test_parse_expr("\\x -> x");
        assert!(result.is_err());
    }
}
