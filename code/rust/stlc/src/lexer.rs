use std::fmt;
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    // BooleanLiteral(bool),
    Colon,            // :
    RightArrow,       // ->
    Equals,           // =
    Lambda,           // λ | \
    LeftParen,        // (
    RightParen,       // )
    RightArrowDouble, // =>
    EOF,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Identifier(str) => write!(f, "{}", str),
            Self::Colon => write!(f, ":"),
            Self::RightArrow => write!(f, "->"),
            Self::Equals => write!(f, "="),
            Self::Lambda => write!(f, "λ"),
            Self::LeftParen => write!(f, "("),
            Self::RightParen => write!(f, ")"),
            Self::RightArrowDouble => write!(f, "=>"),
            Self::EOF => write!(f, "EOF"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum LexerError {
    UnexpectedChar { expected: Vec<char>, found: char },
    UnknownChar { found: char },
}

pub struct Lexer {
    input: Vec<char>,
    position: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let chars: Vec<char> = input.chars().collect();
        Self {
            input: chars,
            position: 0,
        }
    }

    fn peek(&self) -> Option<char> {
        self.input.get(self.position).copied()
    }

    fn advance(&mut self) {
        self.position += 1;
    }

    fn peek_many(&self, n: usize) -> Option<String> {
        let end_pos = self.position + n;
        if end_pos > self.input.len() {
            return None;
        }
        Some(self.input[self.position..end_pos].iter().collect())
    }

    fn advance_many(&mut self, n: usize) {
        self.position += n;
    }

    fn skip_whitespaces(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn skip_comment(&mut self) {
        if self.peek() != Some('-') {
            return;
        }
        let next_pos = self.position + 1;
        if self.input.get(next_pos) == Some(&'-') {
            self.advance_many(2);
            while let Some(ch) = self.peek() {
                if ch == '\n' {
                    break;
                }
                self.advance();
            }
        }
    }

    fn is_identifier_start(c: char) -> bool {
        c.is_ascii_alphabetic() || c == '_'
    }

    fn is_identifier_rest(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }

    fn read_identifier(&mut self) -> Option<String> {
        let fst = self.peek()?;
        if !Self::is_identifier_start(fst) {
            return None;
        }
        let mut result = String::new();
        result.push(fst);
        self.advance();
        while let Some(ch) = self.peek() {
            if Self::is_identifier_rest(ch) {
                result.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        Some(result)
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        loop {
            self.skip_whitespaces();
            match self.peek() {
                None => return Ok(Token::EOF),
                Some('-') => {
                    let next_pos = self.position + 1;
                    match self.input.get(next_pos) {
                        Some('-') => {
                            self.skip_comment();
                            continue;
                        }
                        Some('>') => {
                            self.advance_many(2);
                            return Ok(Token::RightArrow);
                        }
                        Some(other) => {
                            return Err(LexerError::UnexpectedChar {
                                expected: vec!['-', '>'],
                                found: *other,
                            });
                        }
                        None => {
                            return Err(LexerError::UnexpectedChar {
                                expected: vec!['-', '>'],
                                found: '\0',
                            });
                        }
                    }
                }
                Some('=') => {
                    let next_pos = self.position + 1;
                    match self.input.get(next_pos) {
                        Some('>') => {
                            self.advance_many(2);
                            return Ok(Token::RightArrowDouble);
                        }
                        Some(' ') => {
                            self.advance();
                            return Ok(Token::Equals);
                        }

                        Some(other) => {
                            return Err(LexerError::UnexpectedChar {
                                expected: vec![' ', '>'],
                                found: *other,
                            });
                        }
                        None => {
                            return Err(LexerError::UnexpectedChar {
                                expected: vec!['-', '>'],
                                found: '\0',
                            });
                        }
                    }
                }
                Some(':') => {
                    self.advance();
                    return Ok(Token::Colon);
                }

                Some('\\') | Some('λ') => {
                    self.advance();
                    return Ok(Token::Lambda);
                }
                Some('(') => {
                    self.advance();
                    return Ok(Token::LeftParen);
                }
                Some(')') => {
                    self.advance();
                    return Ok(Token::RightParen);
                }
                Some(ch) if Self::is_identifier_start(ch) => {
                    let identifier = self.read_identifier().expect("error: next_token");
                    return Ok(match identifier.as_str() {
                        // "True" => Token::BooleanLiteral(true),
                        // "False" => Token::BooleanLiteral(false),
                        _ => Token::Identifier(identifier),
                    });
                }
                Some(ch) => {
                    return Err(LexerError::UnknownChar { found: ch });
                }
            }
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token()?;
            let is_eof = token == Token::EOF;
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_input() {
        let mut lexer = Lexer::new("");
        assert_eq!(lexer.next_token().unwrap(), Token::EOF);
    }

    #[test]
    fn test_whitespace_only() {
        let mut lexer = Lexer::new("   \t\n  ");
        assert_eq!(lexer.next_token().unwrap(), Token::EOF);
    }

    #[test]
    fn test_single_tokens() {
        let test_cases = vec![
            (":", Token::Colon),
            ("=", Token::Equals),
            ("λ", Token::Lambda),
            ("(", Token::LeftParen),
            (")", Token::RightParen),
            ("->", Token::RightArrow),
        ];

        for (input, expected) in test_cases {
            let mut lexer = Lexer::new(input);
            assert_eq!(
                lexer.next_token().unwrap(),
                expected,
                "Failed for input: {}",
                input
            );
            assert_eq!(lexer.next_token().unwrap(), Token::EOF);
        }
    }

    #[test]
    fn test_identifiers() {
        let test_cases = vec![
            ("x", "x"),
            ("hello", "hello"),
            ("_var", "_var"),
            ("var123", "var123"),
            ("_123abc", "_123abc"),
            ("CamelCase", "CamelCase"),
        ];

        for (input, expected) in test_cases {
            let mut lexer = Lexer::new(input);
            match lexer.next_token().unwrap() {
                Token::Identifier(name) => assert_eq!(name, expected),
                other => panic!("Expected identifier, got {:?}", other),
            }
            assert_eq!(lexer.next_token().unwrap(), Token::EOF);
        }
    }

    #[test]
    /*   fn test_boolean_literals() {
        let mut lexer = Lexer::new("True");
        assert_eq!(lexer.next_token().unwrap(), Token::BooleanLiteral(true));
        assert_eq!(lexer.next_token().unwrap(), Token::EOF);

        let mut lexer = Lexer::new("False");
        assert_eq!(lexer.next_token().unwrap(), Token::BooleanLiteral(false));
        assert_eq!(lexer.next_token().unwrap(), Token::EOF);
    } */
    #[test]
    fn test_comments() {
        let mut lexer = Lexer::new("-- this is a comment\nx");
        assert_eq!(
            lexer.next_token().unwrap(),
            Token::Identifier("x".to_string())
        );
        assert_eq!(lexer.next_token().unwrap(), Token::EOF);
    }

    #[test]
    fn test_comment_at_end() {
        let mut lexer = Lexer::new("x -- comment at end");
        assert_eq!(
            lexer.next_token().unwrap(),
            Token::Identifier("x".to_string())
        );
        assert_eq!(lexer.next_token().unwrap(), Token::EOF);
    }

    #[test]
    fn test_multiple_comments() {
        let mut lexer = Lexer::new("-- comment 1\n-- comment 2\nx");
        assert_eq!(
            lexer.next_token().unwrap(),
            Token::Identifier("x".to_string())
        );
        assert_eq!(lexer.next_token().unwrap(), Token::EOF);
    }

    #[test]
    fn test_lambda_expression() {
        let mut lexer = Lexer::new("λx -> x");
        let expected = vec![
            Token::Lambda,
            Token::Identifier("x".to_string()),
            Token::RightArrow,
            Token::Identifier("x".to_string()),
            Token::EOF,
        ];

        for expected_token in expected {
            assert_eq!(lexer.next_token().unwrap(), expected_token);
        }
    }

    #[test]
    fn test_function_definition() {
        let mut lexer = Lexer::new("f : Bool -> Bool\nf = λx -> x");
        let expected = vec![
            Token::Identifier("f".to_string()),
            Token::Colon,
            Token::Identifier("Bool".to_string()),
            Token::RightArrow,
            Token::Identifier("Bool".to_string()),
            Token::Identifier("f".to_string()),
            Token::Equals,
            Token::Lambda,
            Token::Identifier("x".to_string()),
            Token::RightArrow,
            Token::Identifier("x".to_string()),
            Token::EOF,
        ];

        for expected_token in expected {
            assert_eq!(lexer.next_token().unwrap(), expected_token);
        }
    }

    #[test]
    fn test_complex_expression() {
        let mut lexer = Lexer::new("(λx -> (λy -> x)) True False");
        let expected = vec![
            Token::LeftParen,
            Token::Lambda,
            Token::Identifier("x".to_string()),
            Token::RightArrow,
            Token::LeftParen,
            Token::Lambda,
            Token::Identifier("y".to_string()),
            Token::RightArrow,
            Token::Identifier("x".to_string()),
            Token::RightParen,
            Token::RightParen,
            Token::Identifier("True".to_string()),
            Token::Identifier("False".to_string()),
            // Token::BooleanLiteral(true),
            // Token::BooleanLiteral(false),
            Token::EOF,
        ];

        for expected_token in expected {
            assert_eq!(lexer.next_token().unwrap(), expected_token);
        }
    }

    #[test]
    fn test_tokenize_helper() {
        let mut lexer = Lexer::new("x : Bool");
        let tokens = lexer.tokenize().unwrap();
        let expected = vec![
            Token::Identifier("x".to_string()),
            Token::Colon,
            Token::Identifier("Bool".to_string()),
            Token::EOF,
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_whitespace_between_tokens() {
        let mut lexer = Lexer::new("  x   :   Bool  ->  Bool  ");
        let expected = vec![
            Token::Identifier("x".to_string()),
            Token::Colon,
            Token::Identifier("Bool".to_string()),
            Token::RightArrow,
            Token::Identifier("Bool".to_string()),
            Token::EOF,
        ];

        for expected_token in expected {
            assert_eq!(lexer.next_token().unwrap(), expected_token);
        }
    }

    #[test]
    fn test_mixed_whitespace() {
        let mut lexer = Lexer::new("x\t:\n\rBool");
        let expected = vec![
            Token::Identifier("x".to_string()),
            Token::Colon,
            Token::Identifier("Bool".to_string()),
            Token::EOF,
        ];

        for expected_token in expected {
            assert_eq!(lexer.next_token().unwrap(), expected_token);
        }
    }

    // 错误情况测试
    #[test]
    fn test_unexpected_character() {
        let mut lexer = Lexer::new("@");
        match lexer.next_token() {
            Err(LexerError::UnexpectedChar { found, .. }) => assert_eq!(found, '@'),
            Err(LexerError::UnknownChar { found }) => assert_eq!(found, '@'),
            Ok(token) => panic!("Expected error, got token: {:?}", token),
        }
    }

    #[test]
    fn test_dash_without_arrow_or_comment() {
        let mut lexer = Lexer::new("-x");
        match lexer.next_token() {
            Err(LexerError::UnexpectedChar { found, expected }) => {
                assert_eq!(found, 'x');
                assert_eq!(expected, vec!['-', '>']);
            }
            Err(LexerError::UnknownChar { found }) => {
                panic!(
                    "Expected UnexpectedChar error, got UnknownChar({:?}) error",
                    found
                )
            }
            Ok(token) => panic!("Expected error, got token: {:?}", token),
        }
    }

    #[test]
    fn test_dash_at_end() {
        let mut lexer = Lexer::new("-");
        match lexer.next_token() {
            Err(LexerError::UnexpectedChar { expected, .. }) => {
                assert_eq!(expected, vec!['-', '>']);
            }

            Err(LexerError::UnknownChar { found }) => {
                panic!(
                    "Expected UnexpectedChar error, got UnknownChar({:?}) error",
                    found
                )
            }
            Ok(token) => panic!("Expected error, got token: {:?}", token),
        }
    }

    #[test]
    fn test_identifier_with_numbers() {
        let mut lexer = Lexer::new("var123 _var456");
        assert_eq!(
            lexer.next_token().unwrap(),
            Token::Identifier("var123".to_string())
        );
        assert_eq!(
            lexer.next_token().unwrap(),
            Token::Identifier("_var456".to_string())
        );
        assert_eq!(lexer.next_token().unwrap(), Token::EOF);
    }

    #[test]
    fn test_underscore_identifier() {
        let mut lexer = Lexer::new("_");
        assert_eq!(
            lexer.next_token().unwrap(),
            Token::Identifier("_".to_string())
        );
        assert_eq!(lexer.next_token().unwrap(), Token::EOF);
    }

    #[test]
    fn test_consecutive_tokens() {
        let mut lexer = Lexer::new("()λ->:");
        let expected = vec![
            Token::LeftParen,
            Token::RightParen,
            Token::Lambda,
            Token::RightArrow,
            Token::Colon,
            Token::EOF,
        ];

        for expected_token in expected {
            assert_eq!(lexer.next_token().unwrap(), expected_token);
        }
    }

    #[test]
    fn test_comment_without_newline() {
        let mut lexer = Lexer::new("x -- comment");
        assert_eq!(
            lexer.next_token().unwrap(),
            Token::Identifier("x".to_string())
        );
        assert_eq!(lexer.next_token().unwrap(), Token::EOF);
    }

    #[test]
    fn test_unicode_lambda() {
        let mut lexer = Lexer::new("λ");
        assert_eq!(lexer.next_token().unwrap(), Token::Lambda);
        assert_eq!(lexer.next_token().unwrap(), Token::EOF);
    }

    #[test]
    fn test_real_world_example() {
        let input = r#"
        -- Identity function
        id : Bool -> Bool
        id = λx -> x

        -- Apply function
        apply : (Bool -> Bool) -> Bool -> Bool
        apply = λf -> λx -> f x
        "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();

        // 验证一些关键 token
        assert!(tokens.contains(&Token::Identifier("id".to_string())));
        assert!(tokens.contains(&Token::Identifier("apply".to_string())));
        assert!(tokens.contains(&Token::Lambda));
        assert!(tokens.contains(&Token::RightArrow));
        assert!(tokens.contains(&Token::Colon));
        assert!(tokens.contains(&Token::Equals));
        assert_eq!(tokens.last(), Some(&Token::EOF));
    }
}
