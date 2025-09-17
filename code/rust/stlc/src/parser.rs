use crate::lexer::{Lexer, LexerError, Token};
use crate::syntax::{Dec, Expr, FuncDec, Program, Type, TypeDec};
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

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

enum ExprWraper {
    Epsilon,
    Atom(Box<Expr>),                                 // for x, lambda x: Ty => e, (e)
    Application { func: Box<Expr>, arg: Box<Expr> }, // for e1 e2
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

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }

    fn advance(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.position);
        if token.is_some() && token != Some(&Token::EOF) {
            self.position += 1;
        }
        token
    }

    fn expect_token(&mut self, expected: Token) -> Result<&Token, ParseError> {
        let token = self.advance().ok_or(ParseError::UnexpectedToken {
            expected: expected.to_string(),
            found: "EOF".to_string(),
        })?;
        if *token == expected {
            Ok(token)
        } else {
            Err(ParseError::UnexpectedToken {
                expected: expected.to_string(),
                found: token.to_string(),
            })
        }
    }

    fn expect_identifier(&mut self) -> Result<&Token, ParseError> {
        let token = self.advance().ok_or(ParseError::UnexpectedToken {
            expected: Token::Identifier("?".to_string()).to_string(),
            found: "EOF".to_string(),
        })?;
        match token {
            Token::Identifier(_) => Ok(token),
            _ => Err(ParseError::UnexpectedToken {
                expected: Token::Identifier("?".to_string()).to_string(),
                found: token.to_string(),
            }),
        }
    }

    // Program := Dec {Dec}
    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut decs = Vec::new();
        let mut main = None;
        while let Some(token) = self.peek() {
            if *token == Token::EOF {
                break;
            }
            let dec = self.parse_dec()?;
            if !Self::is_main_dec(&dec) {
                decs.push(dec);
            } else if main.is_none() {
                main = Some(dec);
            } else {
                return Err(ParseError::InvalidSyntax {
                    message: "Program must contain at exactly one main declaration".to_string(),
                });
            }
        }

        if decs.is_empty() {
            return Err(ParseError::InvalidSyntax {
                message: "Program must contain at least one declaration".to_string(),
            });
        }
        if main.is_none() {
            return Err(ParseError::InvalidSyntax {
                message: "Program must contain at exactly one main declaration".to_string(),
            });
        }

        Ok(Program {
            decs: decs,
            main: main.unwrap(),
        })
    }

    fn is_main_dec(dec: &Dec) -> bool {
        return dec.name == "main" && dec.ty_dec.ty == Type::Atom("Unit".to_string());
    }

    // Dec := TypeDec "\n" FuncDec
    fn parse_dec(&mut self) -> Result<Dec, ParseError> {
        let ty_dec = self.parse_type_dec()?;
        let func_dec = self.parse_func_dec()?;
        if ty_dec.name != func_dec.name {
            return Err(ParseError::InvalidSyntax {
                message: format!(
                    "Type declaration name '{}' does not match function declaration name '{}'",
                    ty_dec.name, func_dec.name
                ),
            });
        }
        Ok(Dec {
            name: ty_dec.name.clone(),
            ty_dec,
            func_dec,
        })
    }

    // TypeDec := Identifier : Type
    fn parse_type_dec(&mut self) -> Result<TypeDec, ParseError> {
        if let Token::Identifier(name) = self.expect_identifier()? {
            let name = name.clone();
            self.expect_token(Token::Colon)?;
            let ty = self.parse_type()?;
            Ok(TypeDec { name, ty })
        } else {
            return Err(ParseError::InvalidSyntax {
                message: "Type declaration expects identifier".to_string(),
            });
        }
    }

    // FuncDec := Identifier = Expr
    fn parse_func_dec(&mut self) -> Result<FuncDec, ParseError> {
        if let Token::Identifier(name) = self.expect_identifier()? {
            let name = name.clone();
            self.expect_token(Token::Equals)?;
            let body = self.parse_expr()?;
            Ok(FuncDec { name, body })
        } else {
            return Err(ParseError::InvalidSyntax {
                message: "Type declaration expects identifier".to_string(),
            });
        }
    }

    // Type := Identifier | (Type) | Type -> Type
    // Right-associative: A -> B -> C is A -> (B -> C)
    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let peek = self.peek();
        match peek {
            None => Err(ParseError::InvalidSyntax {
                message: "expects a token".to_string(),
            }),
            Some(Token::Identifier(_)) => self.parse_type_ident(),
            Some(Token::LeftParen) => self.parse_type_paren(),
            _ => self.parse_type_arrow(),
        }
    }
    fn parse_type_paren(&mut self) -> Result<Type, ParseError> {
        self.expect_token(Token::LeftParen)?;
        let ty = self.parse_type()?;
        self.expect_token(Token::RightParen)?;
        let next = self.peek();
        match next {
            None => Err(ParseError::InvalidSyntax {
                message: "expects a token".to_string(),
            }),
            Some(Token::RightArrow) => {
                let right = self.parse_type_arrow()?;
                Ok(Type::Function {
                    input: Box::new(ty),
                    output: Box::new(right),
                })
            }
            Some(_) => Ok(ty),
        }
    }

    fn parse_type_ident(&mut self) -> Result<Type, ParseError> {
        let peek = self.peek();
        match peek {
            None => Err(ParseError::InvalidSyntax {
                message: "expects a token".to_string(),
            }),
            Some(Token::Identifier(name)) => {
                let ty = Type::Atom(name.clone());
                self.advance();
                let next = self.peek();
                match next {
                    None => Err(ParseError::InvalidSyntax {
                        message: "expects a token".to_string(),
                    }),
                    Some(Token::RightArrow) => {
                        let right = self.parse_type_arrow()?;
                        Ok(Type::Function {
                            input: Box::new(ty),
                            output: Box::new(right),
                        })
                    }
                    Some(_) => Ok(ty),
                }
            }
            Some(t) => Err(ParseError::UnexpectedToken {
                expected: "Identifier".to_string(),
                found: t.to_string(),
            }),
        }
    }

    fn parse_type_arrow(&mut self) -> Result<Type, ParseError> {
        let peek = self.peek();
        match peek {
            None => Err(ParseError::InvalidSyntax {
                message: "expects a token".to_string(),
            }),
            Some(Token::RightArrow) => {
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

    // Expr := Identifier | λ Identifier => Expr | (Expr) | Expr Expr
    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        let expr = self.parse_expr_wrapper()?;
        match expr {
            ExprWraper::Epsilon => Err(ParseError::InvalidSyntax {
                message: "expects an expression".to_string(),
            }),
            ExprWraper::Atom(e) => Ok(*e),
            ExprWraper::Application { func, arg } => {
                let e = Expr::Application { func, arg };
                Ok(e)
            }
        }
    }

    fn parse_expr_wrapper(&mut self) -> Result<ExprWraper, ParseError> {
        let peek = self.peek();
        match peek {
            None => Err(ParseError::InvalidSyntax {
                message: "expects an expression".to_string(),
            }),
            Some(Token::Identifier(_)) => Ok(self.parse_expr_var()?),
            Some(Token::Lambda) => Ok(self.parse_expr_lambda()?),
            Some(Token::LeftParen) => Ok(self.parse_expr_paren()?),
            _ => Ok(ExprWraper::Epsilon),
        }
    }

    fn parse_expr_var(&mut self) -> Result<ExprWraper, ParseError> {
        let peek = self.peek();
        match peek {
            None => Err(ParseError::InvalidSyntax {
                message: "expects an expression".to_string(),
            }),
            Some(Token::Identifier(name)) => {
                let expr = Expr::Var(name.clone());
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
                            func: Box::new(expr),
                            arg: func,
                        };
                        let ew = ExprWraper::Application {
                            func: Box::new(app1),
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

    fn parse_expr_lambda(&mut self) -> Result<ExprWraper, ParseError> {
        let peek = self.peek();
        match peek {
            None => Err(ParseError::InvalidSyntax {
                message: "expects Lambda".to_string(),
            }),
            Some(Token::Lambda) => {
                self.advance();
                let ident = self.expect_identifier()?;
                match ident {
                    Token::Identifier(name) => {
                        let name = name.clone();
                        self.expect_token(Token::RightArrowDouble)?;
                        let body = self.parse_expr()?;
                        let lambda = Expr::Lambda {
                            param: name,
                            body: Box::new(body),
                        };
                        Ok(ExprWraper::Atom(Box::new(lambda)))
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
                    func: Box::new(func),
                    arg: Box::new(arg),
                };
                let ew = ExprWraper::Application {
                    func: Box::new(app),
                    arg: atom,
                };
                Ok(ew)
            }
            ExprWraper::Application {
                func: func1,
                arg: arg1,
            } => {
                let app = Expr::Application {
                    func: Box::new(func),
                    arg: Box::new(arg),
                };
                let app1 = Expr::Application {
                    func: Box::new(app),
                    arg: func1,
                };
                let ew = ExprWraper::Application {
                    func: Box::new(app1),
                    arg: arg1,
                };
                Ok(ew)
            }
        }
    }

    fn parse_expr_paren(&mut self) -> Result<ExprWraper, ParseError> {
        let peek = self.peek();
        match peek {
            None => Err(ParseError::InvalidSyntax {
                message: "expects LeftParen".to_string(),
            }),
            Some(Token::LeftParen) => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect_token(Token::RightParen)?;
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
                            func: Box::new(expr),
                            arg: func,
                        };
                        let ew = ExprWraper::Application {
                            func: Box::new(app),
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
        test_parse_type("(Bool)");
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
        test_parse_expr("\\x => x");
    }

    #[test]
    fn test_parse_expr3() {
        let s = r#"(\x => x)(\y => y)"#;
        test_parse_expr(s);
    }

    #[test]
    fn test_parse_expr4() {
        let s = r#"(\x => x)(\y => y)(\z => z)"#;
        test_parse_expr(s);
    }
}
