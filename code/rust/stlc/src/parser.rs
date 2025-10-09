use crate::lexer::{CoordToken, Lexer, LexerError, Token};
use crate::syntax::{
    CoordDec, CoordExpr, CoordFuncDec, CoordProgram, CoordType, CoordTypeDec, Dec, Expr, FuncDec,
    Program, Type, TypeDec,
};
use std::cmp::{max, min};
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
enum TypeWrap {
    Epsilon,
    Prim {
        ty: Type,
        token: CoordToken,
    },
    Paren {
        ty: Box<TypeWrap>,
        fst_token: CoordToken,
        last_token: CoordToken,
    },
    Function {
        input: Box<TypeWrap>,
        output: Box<TypeWrap>,
        fst_token: CoordToken,
        last_token: CoordToken,
    },
}
impl TypeWrap {
    fn to_type(&self) -> Option<CoordType> {
        match self {
            TypeWrap::Epsilon => None,
            TypeWrap::Prim { ty, token } => Some(CoordType {
                ty: ty.clone(),
                row_start: token.row,
                col_start: token.col,
                row_end: token.row,
                col_end: token.col + token.size - 1,
            }),
            TypeWrap::Paren {
                ty,
                fst_token,
                last_token,
            } => Some(CoordType {
                ty: ty.to_type().unwrap().ty,
                row_start: fst_token.row,
                col_start: fst_token.col,
                row_end: last_token.row,
                col_end: last_token.col + last_token.size - 1,
            }),
            TypeWrap::Function {
                input,
                output,
                fst_token,
                last_token,
            } => Some(CoordType {
                ty: Type::Function {
                    input: Box::new(input.to_type().unwrap().ty),
                    output: Box::new(output.to_type().unwrap().ty),
                },
                row_start: fst_token.row,
                col_start: fst_token.col,
                row_end: last_token.row,
                col_end: last_token.col + last_token.size - 1,
            }),
        }
    }
    fn get_last_token(&self) -> CoordToken {
        match self {
            TypeWrap::Epsilon => unreachable!(),
            TypeWrap::Prim { token, .. } => token.clone(),
            TypeWrap::Paren { last_token, .. } => last_token.clone(),
            TypeWrap::Function { last_token, .. } => last_token.clone(),
        }
    }
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
    If {
        pred: Box<ExprWrap>,
        conseq: Box<ExprWrap>,
        alter: Box<ExprWrap>,
        fst_token: CoordToken,
        last_token: CoordToken,
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
            ExprWrap::If {
                pred,
                conseq,
                alter,
                fst_token,
                last_token,
            } => Some(CoordExpr {
                expr: Expr::If {
                    pred: Box::new(pred.to_expr().unwrap().expr),
                    conseq: Box::new(conseq.to_expr().unwrap().expr),
                    alter: Box::new(alter.to_expr().unwrap().expr),
                },
                row_start: fst_token.row,
                col_start: fst_token.col,
                row_end: last_token.row,
                col_end: last_token.col,
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

    fn get_last_token(&self) -> CoordToken {
        match self {
            ExprWrap::Epsilon => unreachable!(),
            ExprWrap::Var { token, .. } => token.clone(),
            ExprWrap::Bool { token, .. } => token.clone(),
            ExprWrap::Paren { last_token, .. } => last_token.clone(),
            ExprWrap::Lambda { last_token, .. } => last_token.clone(),
            ExprWrap::Application { last_token, .. } => last_token.clone(),
            ExprWrap::If { last_token, .. } => last_token.clone(),
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
        self.position += 1;
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
        return dec.name == "main" && dec.ty_dec.ty == Type::Prim("Unit".to_string());
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
        let wrap = self.parse_type_wrap()?;
        let ty = wrap.to_type();
        match ty {
            Some(coord) => Ok(coord),
            None => Err(ParseError::InvalidSyntax {
                message: format!("expects a type"),
            }),
        }
    }

    // TypeWrap := Identifier | (TypeWrap) | TypeWrap -> TypeWrap | Epsilon
    fn parse_type_wrap(&mut self) -> Result<TypeWrap, ParseError> {
        self.parse_type_wrap_arrow()
    }

    // TypeWrap := TypeWrap -> TypeWrap | Epsilon
    fn parse_type_wrap_arrow(&mut self) -> Result<TypeWrap, ParseError> {
        let fst_token = self.peek();
        let mut atom = self.parse_type_wrap_atom()?;
        if TypeWrap::Epsilon == atom {
            return Ok(TypeWrap::Epsilon);
        }
        loop {
            let coord = self.peek();
            if coord.col == 1 {
                return Ok(atom);
            }
            if !coord.is_token(Token::RightArrow) {
                return Ok(atom);
            }
            self.advance();
            let rest = self.parse_type_wrap_arrow()?;
            if TypeWrap::Epsilon == rest {
                return Ok(atom);
            }

            let last_token = rest.get_last_token();
            atom = TypeWrap::Function {
                input: Box::new(atom),
                output: Box::new(rest),
                fst_token: fst_token.clone(),
                last_token: last_token,
            }
        }
    }

    // TypeWrap := Identifier | (TypeWrap) | Epsilon
    fn parse_type_wrap_atom(&mut self) -> Result<TypeWrap, ParseError> {
        let peek = self.peek();
        match peek {
            CoordToken {
                token: Token::Identifier(_),
                ..
            } => self.parse_type_wrap_prim(),
            CoordToken {
                token: Token::LeftParen,
                ..
            } => self.parse_type_wrap_paren(),
            _ => Ok(TypeWrap::Epsilon),
        }
    }

    // TypeWrap := Identifier | Epsilon
    fn parse_type_wrap_prim(&mut self) -> Result<TypeWrap, ParseError> {
        let peek = self.peek();
        match peek {
            CoordToken {
                token: Token::Identifier(name),
                row,
                col,
                size,
            } => {
                self.advance();
                let ty = Type::Prim(name.clone());
                let token = CoordToken {
                    token: Token::Identifier(name.clone()),
                    row,
                    col,
                    size,
                };
                let wrap = TypeWrap::Prim { ty: ty, token };
                Ok(wrap)
            }
            _ => Ok(TypeWrap::Epsilon),
        }
    }

    // TypeWrap := (TypeWrap) | Epsilon
    fn parse_type_wrap_paren(&mut self) -> Result<TypeWrap, ParseError> {
        let peek = self.peek();
        match peek {
            CoordToken {
                token: Token::LeftParen,
                ..
            } => {
                let fst_token = self.peek();
                self.advance();
                let ty = self.parse_type_wrap()?;
                let last_token = self.expect_token(Token::RightParen)?;
                if TypeWrap::Epsilon == ty {
                    return Err(ParseError::InvalidSyntax {
                        message: format!("expects an type"),
                    });
                }
                Ok(TypeWrap::Paren {
                    ty: Box::new(ty),
                    fst_token,
                    last_token,
                })
            }
            _ => Ok(TypeWrap::Epsilon),
        }
    }

    // Expr := Var | Bool | λ Identifier => Expr | (Expr) | if Expr then Expr else Expr | Expr Expr
    pub fn parse_expr(&mut self) -> Result<CoordExpr, ParseError> {
        let wrap = self.parse_expr_wrap_non_epsilon()?;
        let expr = wrap.to_expr();
        match expr {
            Some(coord) => Ok(coord),
            None => Err(ParseError::InvalidSyntax {
                message: format!("expects an expression"),
            }),
        }
    }

    // ExprWrap := Var | Bool | λ Identifier => ExprWrap | (ExprWrap) |
    //             if ExprWrap then ExprWrap else ExprWrap |ExprWrap ExprWrap | Epsilon
    fn parse_expr_wrap(&mut self) -> Result<ExprWrap, ParseError> {
        self.parse_expr_wrap_app()
    }

    fn parse_expr_wrap_non_epsilon(&mut self) -> Result<ExprWrap, ParseError> {
        let expr = self.parse_expr_wrap()?;
        if ExprWrap::Epsilon == expr {
            return Err(ParseError::InvalidSyntax {
                message: format!("expects an expression"),
            });
        }
        Ok(expr)
    }

    // ExprWrap := ExprWrap ExprWrap | Epsilon
    fn parse_expr_wrap_app(&mut self) -> Result<ExprWrap, ParseError> {
        let fst_token = self.peek();
        let mut atom = self.parse_expr_wrap_atom()?;
        if ExprWrap::Epsilon == atom {
            return Ok(ExprWrap::Epsilon);
        }
        loop {
            let CoordToken { col, .. } = self.peek();
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
                token: Token::IF, ..
            } => Ok(self.parse_expr_wrap_if()?),
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

    // ExprWrap := if ExprWrap then ExprWrap else ExprWrap  | Epsilon
    fn parse_expr_wrap_if(&mut self) -> Result<ExprWrap, ParseError> {
        let peek = self.peek();
        match peek {
            CoordToken {
                token: Token::IF,
                row,
                col,
                ..
            } => {
                let fst_token = CoordToken {
                    token: Token::IF,
                    row,
                    col,
                    size: 2,
                };
                self.advance();
                let pred = self.parse_expr_wrap_non_epsilon()?;
                self.expect_token(Token::THEN)?;
                let conseq = self.parse_expr_wrap_non_epsilon()?;
                self.expect_token(Token::ELSE)?;
                let alter = self.parse_expr_wrap_non_epsilon()?;
                let last_token = alter.get_last_token();
                Ok(ExprWrap::If {
                    pred: Box::new(pred),
                    conseq: Box::new(conseq),
                    alter: Box::new(alter),
                    fst_token,
                    last_token,
                })
            }
            _ => Ok(ExprWrap::Epsilon),
        }
    }
    // ExprWrap := λ Identifier => ExprWrap | Epsilon
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
                let body = self.parse_expr_wrap_non_epsilon()?;
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
                let fst_token = self.peek();
                self.advance();
                let expr = self.parse_expr_wrap_non_epsilon()?;
                let last_token = self.expect_token(Token::RightParen)?;
                Ok(ExprWrap::Paren {
                    expr: Box::new(expr),
                    fst_token,
                    last_token,
                })
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
        assert_eq!(result.ty, Type::Prim("Bool".to_string()));
        assert_eq!(result.row_start, 1);
        assert_eq!(result.col_start, 1);
    }

    #[test]
    fn test_parse_type_parenthesized() {
        let result = test_parse_type("(Bool)").unwrap();
        assert_eq!(result.ty, Type::Prim("Bool".to_string()));
    }

    #[test]
    fn test_parse_type_function_simple() {
        let result = test_parse_type("Bool -> Int").unwrap();
        match result.ty {
            Type::Function { input, output } => {
                assert_eq!(*input, Type::Prim("Bool".to_string()));
                assert_eq!(*output, Type::Prim("Int".to_string()));
            }
            _ => panic!("Expected function type"),
        }
    }

    #[test]
    fn test_parse_type_function_right_associative() {
        let result = test_parse_type("Bool -> Int -> String").unwrap();
        match result.ty {
            Type::Function { input, output } => {
                assert_eq!(*input, Type::Prim("Bool".to_string()));
                match *output {
                    Type::Function {
                        input: inner_input,
                        output: inner_output,
                    } => {
                        assert_eq!(*inner_input, Type::Prim("Int".to_string()));
                        assert_eq!(*inner_output, Type::Prim("String".to_string()));
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
                assert_eq!(*input, Type::Prim("Bool".to_string()));
                assert_eq!(*output, Type::Prim("Int".to_string()));
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
                        assert_eq!(*inner_input, Type::Prim("Bool".to_string()));
                        assert_eq!(*inner_output, Type::Prim("Int".to_string()));
                    }
                    _ => panic!("Expected nested function type in input"),
                }
                assert_eq!(*output, Type::Prim("String".to_string()));
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
                        assert_eq!(*inner_input, Type::Prim("Bool".to_string()));
                        assert_eq!(*inner_output, Type::Prim("Int".to_string()));
                    }
                    _ => panic!("Expected nested function type in input"),
                }
                assert_eq!(*output, Type::Prim("String".to_string()));
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
        assert_eq!(result.type_dec.ty, Type::Prim("Bool".to_string()));
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
                assert_eq!(*input, Type::Prim("Bool".to_string()));
                assert_eq!(*output, Type::Prim("Bool".to_string()));
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
            Type::Prim("Unit".to_string())
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
                    assert_eq!(**inner1, Type::Prim("Bool".to_string()));
                    match inner2.as_ref() {
                        Type::Function {
                            input: inner3,
                            output: inner4,
                        } => {
                            assert_eq!(**inner3, Type::Prim("Bool".to_string()));
                            assert_eq!(**inner4, Type::Prim("Bool".to_string()));
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
            ParseError::InvalidSyntax { message } => {
                assert!(message.contains("expects a type"))
            }
            _ => panic!("Expected  InvalidSyntax error"),
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
    #[test]
    fn test_parse_expr_if_simple() {
        let result = test_parse_expr("if True then x else y").unwrap();
        match result.expr {
            Expr::If { pred, conseq, alter } => {
                assert_eq!(*pred, Expr::Bool(true));
                assert_eq!(*conseq, Expr::Var("x".to_string()));
                assert_eq!(*alter, Expr::Var("y".to_string()));
            }
            _ => panic!("Expected If expression"),
        }
    }

    #[test]
    fn test_parse_expr_if_nested() {
        let result = test_parse_expr("if False then if True then x else y else z").unwrap();
        match result.expr {
            Expr::If { pred, conseq, alter } => {
                assert_eq!(*pred, Expr::Bool(false));
                match *conseq {
                    Expr::If { pred: p2, conseq: c2, alter: a2 } => {
                        assert_eq!(*p2, Expr::Bool(true));
                        assert_eq!(*c2, Expr::Var("x".to_string()));
                        assert_eq!(*a2, Expr::Var("y".to_string()));
                    }
                    _ => panic!("Expected nested If in conseq"),
                }
                assert_eq!(*alter, Expr::Var("z".to_string()));
            }
            _ => panic!("Expected If expression"),
        }
    }

    #[test]
    fn test_parse_expr_if_error_missing_then() {
        let result = test_parse_expr("if True x else y");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_expr_if_error_missing_else() {
        let result = test_parse_expr("if True then x y");
        assert!(result.is_err());
    }
}
