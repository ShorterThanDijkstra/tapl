use crate::lexer::{Lexer, LexerError, Token};
use crate::syntax::{Dec, Expr, FuncDec, Type, TypeDec};
#[derive(Debug, PartialEq)]
pub enum ParseError {
    LexerError(LexerError),
    UnexpectedToken { expected: String, found: Token },
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
        if token.is_some() && self.tokens[self.position] != Token::EOF {
            self.position += 1;
        }
        token
    }

    fn expect(&mut self, expected_description: &str) -> Result<&Token, ParseError> {
        match self.advance() {
            Some(Token::EOF) => Err(ParseError::UnexpectedEof {
                expected: expected_description.to_string(),
            }),
            Some(token) => Ok(token),
            None => Err(ParseError::UnexpectedEof {
                expected: expected_description.to_string(),
            }),
        }
    }

    fn expect_token(&mut self, expected: Token, description: &str) -> Result<(), ParseError> {
        let token = self.expect(description)?;
        if *token == expected {
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: description.to_string(),
                found: token.clone(),
            })
        }
    }

    fn expect_identifier(&mut self) -> Result<String, ParseError> {
        let token = self.expect("identifier")?;
        match token {
            Token::Identifier(name) => Ok(name.clone()),
            _ => Err(ParseError::UnexpectedToken {
                expected: "identifier".to_string(),
                found: token.clone(),
            }),
        }
    }

    // Program := Dec {Dec}
    pub fn parse_program(&mut self) -> Result<Vec<Dec>, ParseError> {
        let mut declarations = Vec::new();

        while let Some(token) = self.peek() {
            if *token == Token::EOF {
                break;
            }
            declarations.push(self.parse_dec()?);
        }

        if declarations.is_empty() {
            return Err(ParseError::InvalidSyntax {
                message: "Program must contain at least one declaration".to_string(),
            });
        }

        Ok(declarations)
    }

    // Dec := TypeDec "\n" FuncDec
    fn parse_dec(&mut self) -> Result<Dec, ParseError> {
        let ty_dec = self.parse_type_dec()?;

        let func_dec = self.parse_func_dec()?;

        // Validate that the names match
        if ty_dec.name != func_dec.name {
            return Err(ParseError::InvalidSyntax {
                message: format!(
                    "Type declaration name '{}' does not match function declaration name '{}'",
                    ty_dec.name, func_dec.name
                ),
            });
        }

        Ok(Dec { ty_dec, func_dec })
    }

    // TypeDec := Identifier : Type
    fn parse_type_dec(&mut self) -> Result<TypeDec, ParseError> {
        let name = self.expect_identifier()?;
        self.expect_token(Token::Colon, "colon (:)")?;
        let ty = self.parse_type()?;
        Ok(TypeDec { name, ty })
    }

    // FuncDec := Identifier = Expr
    fn parse_func_dec(&mut self) -> Result<FuncDec, ParseError> {
        let name = self.expect_identifier()?;
        self.expect_token(Token::Equals, "equals (=)")?;
        let expr = self.parse_expr()?;
        Ok(FuncDec {name:name, body:expr})
    }

    // Type := Identifier | Type -> Type
    // Right-associative: A -> B -> C is A -> (B -> C)
    fn parse_type(&mut self) -> Result<Type, ParseError> {
        self.parse_type_arrow()
    }

    fn parse_type_arrow(&mut self) -> Result<Type, ParseError> {
        let mut left = self.parse_type_primary()?;

        while let Some(Token::Arrow) = self.peek() {
            self.advance(); // consume ->
            let right = self.parse_type_arrow()?; // right-associative
            left = Type::Function {
                input: Box::new(left),
                output: Box::new(right),
            };
            break; // right-associative, so we only do this once per level
        }

        Ok(left)
    }

    fn parse_type_primary(&mut self) -> Result<Type, ParseError> {
        match self.peek() {
            Some(Token::Identifier(_)) => {
                let name = self.expect_identifier()?;
                Ok(Type::Primary(name))
            }
            Some(Token::LeftParen) => {
                self.advance(); // consume (
                let ty = self.parse_type()?;
                self.expect_token(Token::RightParen, "right parenthesis )")?;
                Ok(ty)
            }
            Some(token) => Err(ParseError::UnexpectedToken {
                expected: "type".to_string(),
                found: token.clone(),
            }),
            None => Err(ParseError::UnexpectedEof {
                expected: "type".to_string(),
            }),
        }
    }

    // Expr := Identifier | λ Identifier -> Expr | Expr Expr
    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_expr_application()
    }

    // Handle application (left-associative)
    fn parse_expr_application(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_expr_primary()?;

        while let Some(token) = self.peek() {
            match token {
                Token::Identifier(_) | Token::LeftParen => {
                    let right = self.parse_expr_primary()?;
                    left = Expr::Application {
                        func: Box::new(left),
                        arg: Box::new(right),
                    };
                }
                _ => break,
            }
        }

        Ok(left)
    }

    fn parse_expr_primary(&mut self) -> Result<Expr, ParseError> {
        match self.peek() {
            Some(Token::Identifier(_)) => {
                let name = self.expect_identifier()?;
                Ok(Expr::Identifier(name))
            }
            Some(Token::Lambda) => {
                self.advance(); // consume λ
                let param = self.expect_identifier()?;
                self.expect_token(Token::Arrow, "arrow (->)")?;
                let body = self.parse_expr()?;
                Ok(Expr::Lambda {
                    param,
                    body: Box::new(body),
                })
            }
            Some(Token::LeftParen) => {
                self.advance(); // consume (
                let expr = self.parse_expr()?;
                self.expect_token(Token::RightParen, "right parenthesis )")?;
                Ok(expr)
            }
            Some(token) => Err(ParseError::UnexpectedToken {
                expected: "expression".to_string(),
                found: token.clone(),
            }),
            None => Err(ParseError::UnexpectedEof {
                expected: "expression".to_string(),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_declaration() {
        let input = "id : Bool -> Bool\nid = λx -> x";
        let mut parser = Parser::new(input).unwrap();
        let program = parser.parse_program().unwrap();

        assert_eq!(program.len(), 1);
        let dec = &program[0];

        assert_eq!(dec.ty_dec.name, "id");
        assert_eq!(dec.func_dec.name, "id");

        match &dec.ty_dec.ty {
            Type::Function { input, output } => {
                assert_eq!(**input, Type::Primary("Bool".to_string()));
                assert_eq!(**output, Type::Primary("Bool".to_string()));
            }
            _ => panic!("Expected function type"),
        }

        match &dec.func_dec.body {
            Expr::Identifier(name) => assert_eq!(name, "x"),
            _ => panic!("Expected identifier expression"),
        }
    }

    #[test]
    fn test_complex_type() {
        let input = "f : Bool -> (Bool -> Bool)\nf = λx -> λy -> x";
        let mut parser = Parser::new(input).unwrap();
        let program = parser.parse_program().unwrap();

        let dec = &program[0];
        match &dec.ty_dec.ty {
            Type::Function { input, output } => {
                assert_eq!(**input, Type::Primary("Bool".to_string()));
                match output.as_ref() {
                    Type::Function {
                        input: inner_input,
                        output: inner_output,
                    } => {
                        assert_eq!(**inner_input, Type::Primary("Bool".to_string()));
                        assert_eq!(**inner_output, Type::Primary("Bool".to_string()));
                    }
                    _ => panic!("Expected nested function type"),
                }
            }
            _ => panic!("Expected function type"),
        }
    }

    #[test]
    fn test_application_expression() {
        let input = "app : Bool -> Bool\napp = λf -> f x";
        let mut parser = Parser::new(input).unwrap();
        let program = parser.parse_program().unwrap();

        let dec = &program[0];
        match &dec.func_dec.body {
            Expr::Application { func, arg } => {
                match func.as_ref() {
                    Expr::Identifier(name) => assert_eq!(name, "f"),
                    _ => panic!("Expected identifier in function position"),
                }
                match arg.as_ref() {
                    Expr::Identifier(name) => assert_eq!(name, "x"),
                    _ => panic!("Expected identifier in argument position"),
                }
            }
            _ => panic!("Expected application expression"),
        }
    }

    #[test]
    fn test_nested_lambda() {
        let input = "curry : Bool -> Bool -> Bool\ncurry = λx -> λy -> λz -> x";
        let mut parser = Parser::new(input).unwrap();
        let program = parser.parse_program().unwrap();

        let dec = &program[0];

        // The body should be λy -> λz -> x
        match &dec.func_dec.body {
            Expr::Lambda { param, body } => {
                assert_eq!(param, "y");
                match body.as_ref() {
                    Expr::Lambda {
                        param: inner_param,
                        body: inner_body,
                    } => {
                        assert_eq!(inner_param, "z");
                        match inner_body.as_ref() {
                            Expr::Identifier(name) => assert_eq!(name, "x"),
                            _ => panic!("Expected identifier in innermost lambda"),
                        }
                    }
                    _ => panic!("Expected nested lambda"),
                }
            }
            _ => panic!("Expected lambda expression"),
        }
    }

    #[test]
    fn test_multiple_declarations() {
        let input = r#"
id : Bool -> Bool
id = λx -> x

const : Bool -> Bool -> Bool  
const = λx -> λy -> x
        "#;

        let mut parser = Parser::new(input).unwrap();
        let program = parser.parse_program().unwrap();

        assert_eq!(program.len(), 2);
        assert_eq!(program[0].ty_dec.name, "id");
        assert_eq!(program[1].ty_dec.name, "const");
    }

    #[test]
    fn test_parenthesized_expressions() {
        let input = "app : Bool -> Bool\napp = λf -> (f x)";
        let mut parser = Parser::new(input).unwrap();
        let program = parser.parse_program().unwrap();

        // Should parse correctly - parentheses don't change the structure
        // but are handled properly
        assert_eq!(program.len(), 1);
    }

    #[test]
    fn test_left_associative_application() {
        let input = "app : Bool -> Bool\napp = λf -> f x y";
        let mut parser = Parser::new(input).unwrap();
        let program = parser.parse_program().unwrap();

        let dec = &program[0];
        match &dec.func_dec.body {
            Expr::Application { func, arg } => {
                // Should be ((f x) y)
                match func.as_ref() {
                    Expr::Application {
                        func: inner_func,
                        arg: inner_arg,
                    } => match (inner_func.as_ref(), inner_arg.as_ref()) {
                        (Expr::Identifier(f), Expr::Identifier(x)) => {
                            assert_eq!(f, "f");
                            assert_eq!(x, "x");
                        }
                        _ => panic!("Expected f x in inner application"),
                    },
                    _ => panic!("Expected nested application"),
                }
                match arg.as_ref() {
                    Expr::Identifier(name) => assert_eq!(name, "y"),
                    _ => panic!("Expected y as final argument"),
                }
            }
            _ => panic!("Expected application expression"),
        }
    }

    #[test]
    fn test_error_mismatched_names() {
        let input = "f : Bool -> Bool\ng = λx -> x";
        let mut parser = Parser::new(input).unwrap();
        match parser.parse_program() {
            Err(ParseError::InvalidSyntax { message }) => {
                assert!(message.contains("does not match"));
            }
            _ => panic!("Expected syntax error for mismatched names"),
        }
    }

    #[test]
    fn test_error_non_lambda_function() {
        let input = "f : Bool -> Bool\nf = x";
        let mut parser = Parser::new(input).unwrap();
        match parser.parse_program() {
            Err(ParseError::InvalidSyntax { message }) => {
                assert!(message.contains("must be a lambda"));
            }
            _ => panic!("Expected syntax error for non-lambda function"),
        }
    }

    #[test]
    fn test_right_associative_types() {
        let input = "f : Bool -> Bool -> Bool -> Bool\nf = λx -> λy -> λz -> x";
        let mut parser = Parser::new(input).unwrap();
        let program = parser.parse_program().unwrap();

        // Bool -> Bool -> Bool -> Bool should parse as Bool -> (Bool -> (Bool -> Bool))
        let dec = &program[0];
        match &dec.ty_dec.ty {
            Type::Function { input, output } => {
                assert_eq!(**input, Type::Primary("Bool".to_string()));
                match output.as_ref() {
                    Type::Function {
                        input: input2,
                        output: output2,
                    } => {
                        assert_eq!(**input2, Type::Primary("Bool".to_string()));
                        match output2.as_ref() {
                            Type::Function {
                                input: input3,
                                output: output3,
                            } => {
                                assert_eq!(**input3, Type::Primary("Bool".to_string()));
                                assert_eq!(**output3, Type::Primary("Bool".to_string()));
                            }
                            _ => panic!("Expected third level function type"),
                        }
                    }
                    _ => panic!("Expected second level function type"),
                }
            }
            _ => panic!("Expected function type"),
        }
    }
}
