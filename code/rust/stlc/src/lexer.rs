use std::{
    char::REPLACEMENT_CHARACTER,
    fmt::{self},
};
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    BooleanLiteral(bool),
    IF,
    THEN,
    ELSE,
    Colon,            // :
    RightArrow,       // ->
    Equals,           // =
    Lambda,           // λ | \
    LeftParen,        // (
    RightParen,       // )
    RightArrowDouble, // =>
    EOF,
}
impl Token {
    pub fn size(&self) -> usize {
        match self {
            Token::Identifier(s) => s.len(),
            Token::BooleanLiteral(true) => 4,
            Token::BooleanLiteral(false) => 5,
            Token::IF => 2,
            Token::THEN => 4,
            Token::ELSE => 4,
            Token::Colon => 1,
            Token::RightArrow => 2,
            Token::Equals => 1,
            Token::Lambda => 1,
            Token::LeftParen => 1,
            Token::RightParen => 1,
            Token::RightArrowDouble => 2,
            Token::EOF => 0,
        }
    }
    pub fn is_identifier(&self) -> bool {
        matches!(self, Token::Identifier(_))
    }
}
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Identifier(str) => write!(f, "{}", str),
            Self::BooleanLiteral(true) => write!(f, "True"),
            Self::BooleanLiteral(false) => write!(f, "False"),
            Self::IF => write!(f, "if"),
            Self::THEN => write!(f, "then"),
            Self::ELSE => write!(f, "else"),
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

#[derive(Debug, Clone, PartialEq)]
pub struct CoordToken {
    pub token: Token,
    pub row: usize, // starts with 1
    pub col: usize, // starts with 1
    pub size: usize,
}

impl fmt::Display for CoordToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({}, {})", self.token, self.row, self.col)
    }
}

impl CoordToken {
    pub fn is_token(&self, token: Token) -> bool {
        self.token == token
    }
}

#[derive(Debug, PartialEq)]
pub enum LexerError {
    UnexpectedChar { expected: Vec<char>, found: char },
    UnknownChar { found: char },
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexerError::UnexpectedChar { expected, found } => {
                write!(
                    f,
                    "Unexpected character: expected one of {:?}, found '{}'",
                    expected, found
                )
            }
            LexerError::UnknownChar { found } => {
                let display_char = if *found == REPLACEMENT_CHARACTER {
                    "\\u{FFFD}".to_string()
                } else {
                    found.to_string()
                };
                write!(f, "Unknown character: '{}'", display_char)
            }
        }
    }
    
}

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    row: usize, // starts with 1
    col: usize, // starts with 1
}

impl Lexer {
    const RESERVED_WORDS: &'static [&'static str] = &["True", "False", "if", "then", "else"];
    pub fn new(input: &str) -> Self {
        let chars: Vec<char> = input.chars().collect();
        Self {
            input: chars,
            position: 0,
            row: 1,
            col: 1,
        }
    }

    fn wrap_token(&self, token: Token) -> CoordToken {
        CoordToken {
            token: token.clone(),
            row: self.row,
            col: self.col,
            size: token.size(),
        }
    }
    fn wrap_token_row_col(&self, token: Token, row: usize, col: usize) -> CoordToken {
        CoordToken {
            token: token.clone(),
            row: row,
            col: col,
            size: token.size(),
        }
    }

    fn peek(&self) -> Option<char> {
        self.input.get(self.position).copied()
    }

    fn advance(&mut self) {
        // \n
        match self.peek() {
            Some(peek) => {
                if peek == '\n' {
                    self.position += 1;
                    self.newline();
                } else {
                    self.position += 1;
                    self.col += 1;
                }
            }
            None => {}
        }
    }
    fn newline(&mut self) {
        self.row += 1;
        self.col = 1;
    }

    fn peek_many(&self, n: usize) -> Option<String> {
        let end_pos = self.position + n;
        if end_pos > self.input.len() {
            return None;
        }
        Some(self.input[self.position..end_pos].iter().collect())
    }

    fn advance_many(&mut self, n: usize) {
        for _ in 0..n {
            self.advance();
        }
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

    pub fn next_token(&mut self) -> Result<CoordToken, LexerError> {
        loop {
            self.skip_whitespaces();
            match self.peek() {
                None => {
                    return Ok(self.wrap_token(Token::EOF));
                }
                Some('-') => {
                    let next_pos = self.position + 1;
                    match self.input.get(next_pos) {
                        Some('-') => {
                            self.skip_comment();
                            continue;
                        }
                        Some('>') => {
                            let token = self.wrap_token(Token::RightArrow);
                            self.advance_many(2);
                            return Ok(token);
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
                            let token = self.wrap_token(Token::RightArrowDouble);
                            self.advance_many(2);
                            return Ok(token);
                        }
                        _ => {
                            let token = self.wrap_token(Token::Equals);
                            self.advance();
                            return Ok(token);
                        }
                    }
                }
                Some(':') => {
                    let token = self.wrap_token(Token::Colon);
                    self.advance();
                    return Ok(token);
                }

                Some('\\') | Some('λ') => {
                    let token = self.wrap_token(Token::Lambda);
                    self.advance();
                    return Ok(token);
                }
                Some('(') => {
                    let token = self.wrap_token(Token::LeftParen);
                    self.advance();
                    return Ok(token);
                }
                Some(')') => {
                    let token = self.wrap_token(Token::RightParen);
                    self.advance();
                    return Ok(token);
                }
                Some(ch) if Self::is_identifier_start(ch) => {
                    let row = self.row;
                    let col = self.col;
                    let identifier = self.read_identifier().expect("error: next_token");
                    let token = match identifier.as_str() {
                        "True" => Token::BooleanLiteral(true),
                        "False" => Token::BooleanLiteral(false),
                        "if" => Token::IF,
                        "then" => Token::THEN,
                        "else" => Token::ELSE,
                        _ => {
                            assert!(!Self::RESERVED_WORDS.contains(&identifier.as_str()));
                            Token::Identifier(identifier)
                        }
                    };
                    let coord_token = self.wrap_token_row_col(token, row, col);
                    return Ok(coord_token);
                }
                Some(ch) => {
                    return Err(LexerError::UnknownChar { found: ch });
                }
            }
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<CoordToken>, LexerError> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token()?;
            let is_eof = token.is_token(Token::EOF);
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
    use std::iter::zip;

    use super::*;

    #[test]
    fn test_empty_input() {
        let mut lexer = Lexer::new("");
        assert!(lexer.next_token().unwrap().is_token(Token::EOF));
    }

    #[test]
    fn test_whitespace_only() {
        let mut lexer = Lexer::new("   \t\n  ");
        assert!(lexer.next_token().unwrap().is_token(Token::EOF));
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
                lexer.next_token().unwrap().is_token(expected),
                true,
                "Failed for input: {}",
                input
            );
            assert!(lexer.next_token().unwrap().is_token(Token::EOF));
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
                CoordToken {
                    token: Token::Identifier(name),
                    ..
                } => assert_eq!(name, expected),
                other => panic!("Expected identifier, got {:?}", other),
            }
            assert!(lexer.next_token().unwrap().is_token(Token::EOF));
        }
    }

    #[test]
    fn test_boolean_literals() {
        let mut lexer = Lexer::new("True");
        assert!(
            lexer
                .next_token()
                .unwrap()
                .is_token(Token::BooleanLiteral(true))
        );
        assert!(lexer.next_token().unwrap().is_token(Token::EOF));

        let mut lexer = Lexer::new("False");
        assert!(
            lexer
                .next_token()
                .unwrap()
                .is_token(Token::BooleanLiteral(false))
        );
        assert!(lexer.next_token().unwrap().is_token(Token::EOF));
    }
    #[test]
    fn test_comments() {
        let mut lexer = Lexer::new("-- this is a comment\nx");
        assert!(
            lexer
                .next_token()
                .unwrap()
                .is_token(Token::Identifier("x".to_string()))
        );
        assert!(lexer.next_token().unwrap().is_token(Token::EOF));
    }

    #[test]
    fn test_comment_at_end() {
        let mut lexer = Lexer::new("x -- comment at end");
        assert!(
            lexer
                .next_token()
                .unwrap()
                .is_token(Token::Identifier("x".to_string()))
        );
        assert!(lexer.next_token().unwrap().is_token(Token::EOF));
    }

    #[test]
    fn test_multiple_comments() {
        let mut lexer = Lexer::new("-- comment 1\n-- comment 2\nx");
        assert_eq!(
            lexer
                .next_token()
                .unwrap()
                .is_token(Token::Identifier("x".to_string())),
            true
        );
        assert!(lexer.next_token().unwrap().is_token(Token::EOF));
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
            assert!(lexer.next_token().unwrap().is_token(expected_token));
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
            assert!(lexer.next_token().unwrap().is_token(expected_token));
        }
    }

    #[test]
    fn test_complex() {
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
            Token::BooleanLiteral(true),
            Token::BooleanLiteral(false),
            Token::EOF,
        ];

        for expected_token in expected {
            assert!(lexer.next_token().unwrap().is_token(expected_token));
        }
    }

    #[test]
    fn test_tokenize_helper() {
        let mut lexer = Lexer::new("x : Bool");
        let tokens = lexer.tokenize().unwrap();
        let expecteds = vec![
            Token::Identifier("x".to_string()),
            Token::Colon,
            Token::Identifier("Bool".to_string()),
            Token::EOF,
        ];
        for (token, expected) in zip(tokens, expecteds) {
            assert!(token.is_token(expected));
        }
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
            assert!(lexer.next_token().unwrap().is_token(expected_token));
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
            assert!(lexer.next_token().unwrap().is_token(expected_token));
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
            lexer
                .next_token()
                .unwrap()
                .is_token(Token::Identifier("var123".to_string())),
            true
        );
        assert_eq!(
            lexer
                .next_token()
                .unwrap()
                .is_token(Token::Identifier("_var456".to_string())),
            true
        );
        assert!(lexer.next_token().unwrap().is_token(Token::EOF));
    }

    #[test]
    fn test_underscore_identifier() {
        let mut lexer = Lexer::new("_");
        assert_eq!(
            lexer
                .next_token()
                .unwrap()
                .is_token(Token::Identifier("_".to_string())),
            true
        );
        assert!(lexer.next_token().unwrap().is_token(Token::EOF));
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
            assert!(lexer.next_token().unwrap().is_token(expected_token));
        }
    }

    #[test]
    fn test_comment_without_newline() {
        let mut lexer = Lexer::new("x -- comment");
        assert_eq!(
            lexer
                .next_token()
                .unwrap()
                .is_token(Token::Identifier("x".to_string())),
            true
        );
        assert!(lexer.next_token().unwrap().is_token(Token::EOF));
    }

    #[test]
    fn test_unicode_lambda() {
        let mut lexer = Lexer::new("λ");
        assert!(lexer.next_token().unwrap().is_token(Token::Lambda));
        assert!(lexer.next_token().unwrap().is_token(Token::EOF));
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
        assert!(
            tokens
                .clone()
                .into_iter()
                .map(|coord_token| coord_token.token)
                .collect::<Vec<Token>>()
                .contains(&Token::Identifier("id".to_string()))
        );
        assert!(
            tokens
                .clone()
                .into_iter()
                .map(|coord_token| coord_token.token)
                .collect::<Vec<Token>>()
                .contains(&Token::Identifier("apply".to_string()))
        );
        assert!(
            tokens
                .clone()
                .into_iter()
                .map(|coord_token| coord_token.token)
                .collect::<Vec<Token>>()
                .contains(&Token::Lambda)
        );
        assert!(
            tokens
                .clone()
                .into_iter()
                .map(|coord_token| coord_token.token)
                .collect::<Vec<Token>>()
                .contains(&Token::RightArrow)
        );
        assert!(
            tokens
                .clone()
                .into_iter()
                .map(|coord_token| coord_token.token)
                .collect::<Vec<Token>>()
                .contains(&Token::Colon)
        );
        assert!(
            tokens
                .clone()
                .into_iter()
                .map(|coord_token| coord_token.token)
                .collect::<Vec<Token>>()
                .contains(&Token::Equals)
        );
        assert!(tokens.last().unwrap().is_token(Token::EOF));
    }
    #[test]
    fn test_reserved_words_true() {
        let mut lexer = Lexer::new("True");
        match lexer.next_token().unwrap() {
            CoordToken {
                token: Token::BooleanLiteral(true),
                ..
            } => {}
            other => panic!("Expected BooleanLiteral(true), got {:?}", other),
        }
        assert!(lexer.next_token().unwrap().is_token(Token::EOF));
    }

    #[test]
    fn test_reserved_words_false() {
        let mut lexer = Lexer::new("False");
        match lexer.next_token().unwrap() {
            CoordToken {
                token: Token::BooleanLiteral(false),
                ..
            } => {}
            other => panic!("Expected BooleanLiteral(false), got {:?}", other),
        }
        assert!(lexer.next_token().unwrap().is_token(Token::EOF));
    }

    #[test]
    fn test_reserved_words_if() {
        let mut lexer = Lexer::new("if");
        match lexer.next_token().unwrap() {
            CoordToken {
                token: Token::IF, ..
            } => {}
            other => panic!("Expected IF, got {:?}", other),
        }
        assert!(lexer.next_token().unwrap().is_token(Token::EOF));
    }

    #[test]
    fn test_reserved_words_then() {
        let mut lexer = Lexer::new("then");
        match lexer.next_token().unwrap() {
            CoordToken {
                token: Token::THEN, ..
            } => {}
            other => panic!("Expected THEN, got {:?}", other),
        }
        assert!(lexer.next_token().unwrap().is_token(Token::EOF));
    }

    #[test]
    fn test_reserved_words_else() {
        let mut lexer = Lexer::new("else");
        match lexer.next_token().unwrap() {
            CoordToken {
                token: Token::ELSE, ..
            } => {}
            other => panic!("Expected ELSE, got {:?}", other),
        }
        assert!(lexer.next_token().unwrap().is_token(Token::EOF));
    }

    #[test]
    fn test_reserved_words_mixed() {
        let mut lexer = Lexer::new("if then else True False");
        let expected = vec![
            Token::IF,
            Token::THEN,
            Token::ELSE,
            Token::BooleanLiteral(true),
            Token::BooleanLiteral(false),
            Token::EOF,
        ];
        for expected_token in expected {
            assert!(lexer.next_token().unwrap().is_token(expected_token));
        }
    }
}
