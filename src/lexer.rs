use std::{fmt::Display, rc::Rc};

use crate::error::BooleExprError;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
pub struct Precedence(pub u32);

impl Precedence {
    pub fn min() -> Self {
        Self(0)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum OperatorToken {
    Tilde,
    Ampersand,
    Pipe,
    Arrow,
    DoubleArrow,
}

impl OperatorToken {
    pub fn is_binary(&self) -> bool {
        *self != OperatorToken::Tilde
    }

    pub fn lexeme(&self) -> &'static str {
        match self {
            OperatorToken::Tilde => "~",
            OperatorToken::Ampersand => "&",
            OperatorToken::Pipe => "|",
            OperatorToken::Arrow => "->",
            OperatorToken::DoubleArrow => "<->",
        }
    }

    pub fn precedende(&self) -> Precedence {
        match self {
            OperatorToken::Arrow | OperatorToken::DoubleArrow => Precedence(1),
            OperatorToken::Ampersand | OperatorToken::Pipe => Precedence(2),
            OperatorToken::Tilde => Precedence(3),
        }
    }
}

// TODO: I should replace String by Rc<str>?
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token {
    Operator(OperatorToken),
    Identifier(Rc<str>),
    LParen,
    RParen,
    Eof,
}

impl Token {
    pub fn lexeme(&self) -> &str {
        match self {
            Token::Operator(op) => op.lexeme(),
            Token::Identifier(name) => name,
            Token::LParen => "(",
            Token::RParen => ")",
            Token::Eof => "EOF",
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.lexeme().fmt(f)
    }
}

#[derive(Debug)]
pub struct Lexer {
    tokens: Vec<Token>,
    index: usize,
}

impl Lexer {
    pub fn from_str(input: &str) -> Result<Self, BooleExprError> {
        let chars: Vec<char> = input.chars().collect();
        Self::from_chars(&chars)
    }

    pub fn from_chars(input: &[char]) -> Result<Self, BooleExprError> {
        let tokens = TokenGenerator::new(input).collect::<Result<Vec<_>, BooleExprError>>()?;
        Ok(Lexer { tokens, index: 0 })
    }

    pub fn consume(&mut self) -> Token {
        let token = self.peek();
        self.index += 1;
        token
    }

    pub fn peek(&self) -> Token {
        self.tokens.get(self.index).cloned().unwrap_or(Token::Eof)
    }

    // A slice to the remaining tokens
    pub fn as_slice(&self) -> &[Token] {
        &self.tokens[self.index..]
    }
}

#[derive(Debug)]
pub struct TokenGenerator<'a> {
    input: &'a [char],
    index: usize,
}

impl<'a> TokenGenerator<'a> {
    fn new(input: &'a [char]) -> Self {
        TokenGenerator { input, index: 0 }
    }

    fn lex_double_arrow(&mut self) -> Result<Token, BooleExprError> {
        self.index += 1;
        match self.input.get(self.index..self.index + 2) {
            Some(['-', '>']) => {
                self.index += 2;
                Ok(Token::Operator(OperatorToken::DoubleArrow))
            }
            Some(other) => {
                self.index += 2;
                self.error(&other.iter().collect::<String>())
            },
            None => self.error("<"),
        }
    }

    fn lex_arrow(&mut self) -> Result<Token, BooleExprError> {
        self.index += 1;
        match self.input.get(self.index) {
            Some('>') => {
                self.index += 1;
                Ok(Token::Operator(OperatorToken::Arrow))
            }
            Some(chr) => {
                self.index += 1;
                self.error(&format!("-{chr}"))
            },
            None => self.error("-"),
        }
    }

    fn lex_identifier(&mut self) -> Result<Token, BooleExprError> {
        let mut attr = String::new();
        for &chr in &self.input[self.index..] {
            if chr.is_ascii_alphanumeric() {
                attr.push(chr);
            } else {
                break;
            }
        }
        self.index += attr.len();
        Ok(Token::Identifier(Rc::from(attr)))
    }

    fn error(&self, erronous_lexeme: &str) -> Result<Token, BooleExprError> {
        Err(BooleExprError::LexerError(erronous_lexeme.to_string()))
    }

    fn ignore_whitespace(&mut self) {
        for chr in &self.input[self.index..] {
            if chr.is_whitespace() {
                self.index += 1;
            } else {
                break;
            }
        }
    }
}

impl Iterator for TokenGenerator<'_> {
    type Item = Result<Token, BooleExprError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index == self.input.len() {
            self.index += 1;
            return Some(Ok(Token::Eof));
        } else if self.index > self.input.len() {
            return None;
        }

        self.ignore_whitespace();

        match self.input[self.index] {
            '(' => {
                self.index += 1;
                Some(Ok(Token::LParen))
            }
            ')' => {
                self.index += 1;
                Some(Ok(Token::RParen))
            }
            '&' => {
                self.index += 1;
                Some(Ok(Token::Operator(OperatorToken::Ampersand)))
            }
            '|' => {
                self.index += 1;
                Some(Ok(Token::Operator(OperatorToken::Pipe)))
            }
            '~' => {
                self.index += 1;
                Some(Ok(Token::Operator(OperatorToken::Tilde)))
            }
            '<' => Some(self.lex_double_arrow()),
            '-' => Some(self.lex_arrow()),
            chr if chr.is_ascii_alphabetic() => Some(self.lex_identifier()),
            other => {
                self.index += 1;
                Some(self.error(&other.to_string()))
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer_with_correct_lexemes() {
        let lexer = Lexer::from_str("a -><->cd&").unwrap();
        let expected = [
            Token::Identifier(Rc::from("a")),
            Token::Operator(OperatorToken::Arrow),
            Token::Operator(OperatorToken::DoubleArrow),
            Token::Identifier(Rc::from("cd")),
            Token::Operator(OperatorToken::Ampersand),
            Token::Eof
        ];
        assert_eq!(lexer.as_slice(), expected);

        let lexer = Lexer::from_str("a|b~").unwrap();
        let expected = [
            Token::Identifier(Rc::from("a")),
            Token::Operator(OperatorToken::Pipe),
            Token::Identifier(Rc::from("b")),
            Token::Operator(OperatorToken::Tilde),
            Token::Eof
        ];
        assert_eq!(lexer.as_slice(), expected);

        let lexer = Lexer::from_str("").unwrap();
        let expected = [
            Token::Eof,
        ];
        assert_eq!(lexer.as_slice(), expected);
    }

    #[test]
    fn lexer_with_incorrect_lexemes() {
        assert!(Lexer::from_str("<<-ab").is_err());
        assert!(Lexer::from_str("((->>c").is_err());
        assert!(Lexer::from_str("a\"c").is_err());

        // // Non ascii characters are not supported
        assert!(Lexer::from_str("ñ").is_err());
    }
}
