use crate::lexer::{CoordToken, Lexer, LexerError, Token};
use crate::syntax::{
    CoordDec, CoordExpr, CoordFuncDec, CoordProgram, CoordType, CoordTypeDec, Dec, Expr, FuncDec,
    Program, Type, TypeDec,
};
use std::cmp::{max, min};
use std::fmt::Error;
use std::fs;
use std::path::Path;
#[derive(Debug, PartialEq)]
pub enum ParseError {
    LexerError(LexerError),
    UnexpectedToken { expected: Vec<Token>, found: Token },
    UnexpectedEof { expected: Vec<Token> },
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
    eof: CoordToken,
    position: usize,
}

#[derive(Debug, Clone, PartialEq)]
enum ExprWrap {
    Epsilon,
    Var {
        expr: Expr,
        token: CoordToken,
    },
    Bool {
        expr: Expr,
        token: CoordToken,
    },
    Paren {
        expr: Box<ExprWrap>,
        fst_token: CoordToken,
        last_token: CoordToken,
    },
    Lambda {
        param: String,
        body: Box<ExprWrap>,
        fst_token: CoordToken,
        last_token: CoordToken,
    },
    Application {
        func: Box<ExprWrap>,
        arg: Box<ExprWrap>,
        fst_token: CoordToken,
        last_token: CoordToken,
    },
}

impl ExprWrap {
    fn to_expr(&self) -> Option<CoordExpr> {
        match self {
            ExprWrap::Epsilon => None,
            ExprWrap::Var { expr, token } => Some(CoordExpr {
                expr: expr.clone(),
                row_start: token.row,
                col_start: token.col,
                row_end: token.row,
                col_end: token.col + token.size - 1,
            }),
            ExprWrap::Bool { expr, token } => Some(CoordExpr {
                expr: expr.clone(),
                row_start: token.row,
                col_start: token.col,
                row_end: token.row,
                col_end: token.col + token.size - 1,
            }),
            ExprWrap::Paren {
                expr,
                fst_token,
                last_token,
            } => Some(CoordExpr {
                expr: expr.to_expr().unwrap().expr,
                row_start: fst_token.row,
                col_start: fst_token.col,
                row_end: last_token.row,
                col_end: last_token.col + last_token.size - 1,
            }),
            ExprWrap::Lambda {
                param,
                body,
                fst_token,
                last_token,
            } => Some(CoordExpr {
                expr: Expr::Lambda {
                    param: param.clone(),
                    body: Box::new(body.to_expr().unwrap().expr),
                },
                row_start: fst_token.row,
                col_start: fst_token.col,
                row_end: last_token.row,
                col_end: last_token.col + last_token.size - 1,
            }),
            ExprWrap::Application {
                func,
                arg,
                fst_token,
                last_token,
            } => Some(CoordExpr {
                expr: Expr::Application {
                    func: Box::new(func.to_expr().unwrap().expr),
                    arg: Box::new(arg.to_expr().unwrap().expr),
                },
                row_start: fst_token.row,
                col_start: fst_token.col,
                row_end: last_token.row,
                col_end: last_token.col + last_token.size - 1,
            }),
        }
    }

    fn get_fst_token(&self) -> CoordToken {
        match self {
            ExprWrap::Epsilon => unreachable!(),
            ExprWrap::Var { token, .. } => token.clone(),
            ExprWrap::Bool { token, .. } => token.clone(),
            ExprWrap::Paren { fst_token, .. } => fst_token.clone(),
            ExprWrap::Lambda { fst_token, .. } => fst_token.clone(),
            ExprWrap::Application { fst_token, .. } => fst_token.clone(),
        }
    }

    fn get_last_token(&self) -> CoordToken {
        match self {
            ExprWrap::Epsilon => unreachable!(),
            ExprWrap::Var { token, .. } => token.clone(),
            ExprWrap::Bool { token, .. } => token.clone(),
            ExprWrap::Paren { last_token, .. } => last_token.clone(),
            ExprWrap::Lambda { last_token, .. } => last_token.clone(),
            ExprWrap::Application { last_token, .. } => last_token.clone(),
        }
    }
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
        if let Some(eof) = tokens.clone().last() {
            Ok(Self {
                tokens,
                eof: eof.clone(),
                position: 0,
            })
        } else {
            Err(ParseError::InvalidSyntax {
                message: format!("no EOF found"),
            })
        }
    }

    fn peek(&self) -> CoordToken {
        let peek = self.tokens.get(self.position);
        match peek {
            None => self.eof.clone(),
            Some(t) => t.clone(),
        }
    }

    fn advance(&mut self) {
        let coord_token = self.tokens.get(self.position);
        if coord_token.is_some() && !coord_token.unwrap().is_token(Token::EOF) {
            self.position += 1;
        }
    }

    fn expect_token(&mut self, expected: Token) -> Result<CoordToken, ParseError> {
        let peek = self.peek();
        if peek.is_token(expected.clone()) {
            let res = peek.clone();
            self.advance();
            Ok(res)
        } else {
            Err(ParseError::UnexpectedToken {
                expected: vec![expected],
                found: peek.token.clone(),
            })
        }
    }

    // Program := Dec {Dec}
    pub fn parse_program(&mut self) -> Result<CoordProgram, ParseError> {
        let mut coord_decs = Vec::new();
        let mut main = None;
        loop {
            let token = self.peek();
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
                    message: format!("Program must contain at exactly one main declaration"),
                });
            }
        }

        if main.is_none() {
            return Err(ParseError::InvalidSyntax {
                message: format!("Program must contain at exactly one main declaration"),
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
        let peek = self.peek();
        match peek {
            CoordToken {
                token: Token::Identifier(name),
                row,
                col,
                ..
            } => {
                let name = name.clone();
                let row_start = row;
                let col_start = col;
                self.advance();
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
            }
            t => Err(ParseError::UnexpectedToken {
                expected: vec![Token::Identifier("?".to_string())],
                found: t.token.clone(),
            }),
        }
    }

    // FuncDec := Identifier = Expr
    fn parse_func_dec(&mut self) -> Result<CoordFuncDec, ParseError> {
        let peek = self.peek();
        match peek {
            CoordToken {
                token: Token::Identifier(name),
                row,
                col,
                ..
            } => {
                let name = name.clone();
                let row_start = row;
                let col_start = col;
                self.advance();
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
            }
            _ => {
                return Err(ParseError::InvalidSyntax {
                    message: format!("Type declaration expects identifier"),
                });
            }
        }
    }

    // Type := Identifier | (Type) | Type -> Type
    // Right-associative: A -> B -> C is A -> (B -> C)
    fn parse_type(&mut self) -> Result<CoordType, ParseError> {
        let peek = self.peek();
        match peek {
            // must be a type
            CoordToken {
                token: Token::EOF, ..
            } => Err(ParseError::UnexpectedEof {
                expected: vec![Token::Identifier(format!("?")), Token::LeftParen],
            }),
            CoordToken {
                token: Token::Identifier(_),
                ..
            } => self.parse_type_atom(),
            CoordToken {
                token: Token::LeftParen,
                ..
            } => self.parse_type_paren(),
            _ => self.parse_type_arrow(),
        }
    }
    // Type := ( Type ) [-> Type]
    fn parse_type_paren(&mut self) -> Result<CoordType, ParseError> {
        let CoordToken { row, col, .. } = self.expect_token(Token::LeftParen)?;
        let row_start = row;
        let col_start = col;
        let ty = self.parse_type()?;
        let CoordToken { row, col, size, .. } = self.expect_token(Token::RightParen)?;
        let row_end = row;
        let col_end = col + size - 1;
        let next = self.peek();
        match next {
            CoordToken {
                token: Token::RightArrow,
                ..
            } => {
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
            _ => {
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
    fn parse_type_atom(&mut self) -> Result<CoordType, ParseError> {
        let peek = self.peek();
        match peek {
            // must be a type
            CoordToken {
                token: Token::EOF, ..
            } => Err(ParseError::UnexpectedEof {
                expected: vec![Token::Identifier("?".to_string())],
            }),
            CoordToken {
                token: Token::Identifier(name),
                row,
                col,
                size,
            } => {
                let name = name.clone();
                let row_start = row;
                let col_start = col;
                self.advance();
                let ty = Type::Atom(name);
                let next = self.peek();
                match next {
                    CoordToken {
                        token: Token::RightArrow,
                        ..
                    } => {
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
                    _ => {
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
            t => Err(ParseError::UnexpectedToken {
                expected: vec![Token::Identifier("?".to_string())],
                found: t.token.clone(),
            }),
        }
    }

    // Type := -> Type
    fn parse_type_arrow(&mut self) -> Result<CoordType, ParseError> {
        let peek = self.peek();
        match peek {
            // must be a type
            CoordToken {
                token: Token::EOF, ..
            } => Err(ParseError::UnexpectedEof {
                expected: vec![Token::RightArrow],
            }),
            CoordToken {
                token: Token::RightArrow,
                ..
            } => {
                self.advance();
                let ty = self.parse_type()?;
                Ok(ty)
            }
            t => Err(ParseError::UnexpectedToken {
                expected: vec![Token::Identifier("?".to_string())],
                found: t.token.clone(),
            }),
        }
    }

    // Expr := Var | Bool | λ Identifier => Expr | (Expr) | Expr Expr
    fn parse_expr(&mut self) -> Result<CoordExpr, ParseError> {
        let wrap = self.parse_expr_wrap()?;
        let expr = wrap.to_expr();
        match expr {
            Some(coord) => Ok(coord),
            None => Err(ParseError::InvalidSyntax {
                message: format!("expects an expression"),
            }),
        }
    }

    // ExprWrap := Var | Bool | λ Identifier => ExprWrap | (ExprWrap) | ExprWrap ExprWrap | Epsilon
    fn parse_expr_wrap(&mut self) -> Result<ExprWrap, ParseError> {
        self.parse_expr_wrap_app()
    }

    // ExprWrap := ExprWrap ExprWrap
    fn parse_expr_wrap_app(&mut self) -> Result<ExprWrap, ParseError> {
        let fst_token = self.peek();
        let mut atom = self.parse_expr_wrap_atom()?;
        if ExprWrap::Epsilon == atom {
            return Ok(ExprWrap::Epsilon);
        }
        loop {
            let CoordToken{col,..} = self.peek();
            if col == 1 {
                return Ok(atom);
            }
            let next = self.parse_expr_wrap_atom()?;
            if ExprWrap::Epsilon == next {
                return Ok(atom);
            }
           
            let last_token = next.get_last_token();
            atom = ExprWrap::Application {
                func: Box::new(atom),
                arg: Box::new(next),
                fst_token: fst_token.clone(),
                last_token,
            };
        }
    }
    // ExprWrap := Var | Bool | λ Identifier => ExprWrap | (ExprWrap) | Epsilon
    fn parse_expr_wrap_atom(&mut self) -> Result<ExprWrap, ParseError> {
        let peek = self.peek();
        match peek {
            CoordToken {
                token: Token::Identifier(_),
                ..
            } => Ok(self.parse_expr_wrap_var()?),

            CoordToken {
                token: Token::BooleanLiteral(_),
                ..
            } => Ok(self.parse_expr_wrap_bool()?),

            CoordToken {
                token: Token::Lambda,
                ..
            } => Ok(self.parse_expr_wrap_lambda()?),

            CoordToken {
                token: Token::LeftParen,
                ..
            } => Ok(self.parse_expr_wrap_paren()?),

            _ => Ok(ExprWrap::Epsilon),
        }
    }

    // ExprWrap := Var | Epsilon
    fn parse_expr_wrap_var(&mut self) -> Result<ExprWrap, ParseError> {
        let peek = self.peek();
        match peek {
            CoordToken {
                token: Token::Identifier(name),
                row,
                col,
                size,
            } => {
                self.advance();
                let expr = Expr::Var(name.clone());
                let token = CoordToken {
                    token: Token::Identifier(name.clone()),
                    row,
                    col,
                    size,
                };
                let wrap = ExprWrap::Var { expr, token };
                Ok(wrap)
            }
            _ => Ok(ExprWrap::Epsilon),
        }
    }

    // ExprWrap := Bool | Epsilon
    fn parse_expr_wrap_bool(&mut self) -> Result<ExprWrap, ParseError> {
        let peek = self.peek();
        match peek {
            CoordToken {
                token: Token::BooleanLiteral(b),
                row,
                col,
                size,
            } => {
                self.advance();
                let expr = Expr::Bool(b);
                let token = CoordToken {
                    token: Token::BooleanLiteral(b),
                    row,
                    col,
                    size,
                };
                let wrap = ExprWrap::Bool { expr, token };
                Ok(wrap)
            }
            _ => Ok(ExprWrap::Epsilon),
        }
    }

    // ExprWrap := λ Identifier => ExprWrap
    fn parse_expr_wrap_lambda(&mut self) -> Result<ExprWrap, ParseError> {
        let peek = self.peek();
        match peek {
            CoordToken {
                token: Token::Lambda,
                row,
                col,
                ..
            } => {
                let fst_token = CoordToken {
                    token: Token::Lambda,
                    row,
                    col,
                    size: 1,
                };
                self.advance();
                let peek = self.peek();
                let param = match peek {
                    CoordToken {
                        token: Token::Identifier(name),
                        ..
                    } => {
                        self.advance();
                        name
                    }
                    _ => {
                        return Err(ParseError::UnexpectedToken {
                            expected: vec![Token::Identifier("".to_string())],
                            found: peek.token.clone(),
                        });
                    }
                };

                self.expect_token(Token::RightArrowDouble)?;
                let body = self.parse_expr_wrap()?;
                let last_token = body.get_last_token();
                let wrap = ExprWrap::Lambda {
                    param,
                    body: Box::new(body),
                    fst_token,
                    last_token,
                };
                Ok(wrap)
            }
            _ => Ok(ExprWrap::Epsilon),
        }
    }

    // ExprWrap := (ExprWrap) | Epsilon
    fn parse_expr_wrap_paren(&mut self) -> Result<ExprWrap, ParseError> {
        let peek = self.peek();
        match peek {
            CoordToken {
                token: Token::LeftParen,
                ..
            } => {
                self.advance();
                let expr = self.parse_expr_wrap()?;
                self.expect_token(Token::RightParen)?;
                if ExprWrap::Epsilon == expr {
                    return Err(ParseError::InvalidSyntax {
                        message: format!("expects an expression"),
                    });
                }
                Ok(expr)
            }
            _ => Ok(ExprWrap::Epsilon),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::*;
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
                assert_eq!(found, Token::Colon);
                // expected should contain Identifier
                assert!(expected.into_iter().any(|t| t.is_identifier()));
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
    fn delete() {
        let input = r#"
zero : (Bool -> Bool) -> Bool -> Bool
zero = \x => x
--zero = \f => \x => f (f x) 

main : Unit
main = unit
        "#;

        let result = test_parse_program(input).unwrap();
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

-- Church numerals
zero : (Bool -> Bool) -> Bool -> Bool
zero = \f => \x => x
one : (Bool -> Bool) -> Bool -> Bool
one = \f => \x => f x
two : (Bool -> Bool) -> Bool -> Bool
two = \f => \x => f (f x)
three : (Bool -> Bool) -> Bool -> Bool
three = \f => \x => f (f (f x))

main : Unit
main = unit
"#;
        let result = test_parse_program(input).unwrap();
        assert_eq!(result.program.decs.len(), 7);
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
