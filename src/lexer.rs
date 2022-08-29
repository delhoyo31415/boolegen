use std::{cmp::Ordering, fmt::Display, iter::Peekable, rc::Rc};

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
    tokens: Peekable<TokenGenerator>,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            tokens: TokenGenerator::new(input.chars()).peekable(),
        }
    }

    pub fn consume(&mut self) -> Result<Token, BooleExprError> {
        self.tokens.next().unwrap_or(Ok(Token::Eof))
    }

    pub fn peek(&mut self) -> Result<Token, BooleExprError> {
        self.tokens.peek().cloned().unwrap_or(Ok(Token::Eof))
    }
}

#[derive(Debug, Clone)]
struct TokenGenerator {
    input: Vec<char>,
    index: usize,
}

impl TokenGenerator {
    fn new<I>(input: I) -> Self
    where
        I: IntoIterator<Item = char>,
    {
        Self {
            input: input.into_iter().collect(),
            index: 0,
        }
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
            }
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
            }
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

    fn lex_next(&mut self) -> Option<<Self as Iterator>::Item> {
        self.ignore_whitespace();
        match self.input.get(self.index)? {
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
            }
        }
    }
}

impl Iterator for TokenGenerator {
    type Item = Result<Token, BooleExprError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.index.cmp(&self.input.len()) {
            Ordering::Less => self.lex_next(),
            Ordering::Equal => {
                self.index += 1;
                Some(Ok(Token::Eof))
            }
            Ordering::Greater => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn vec_tokens(s: &str) -> Result<Vec<Token>, BooleExprError> {
        Lexer::new(s).tokens.collect::<Result<Vec<_>, _>>()
    }

    #[test]
    fn lexer_with_correct_lexemes() {
        let tokens = vec_tokens("a -><->cd&").unwrap();
        let expected = [
            Token::Identifier(Rc::from("a")),
            Token::Operator(OperatorToken::Arrow),
            Token::Operator(OperatorToken::DoubleArrow),
            Token::Identifier(Rc::from("cd")),
            Token::Operator(OperatorToken::Ampersand),
            Token::Eof,
        ];
        assert_eq!(tokens, expected);

        let tokens = vec_tokens("a|b~").unwrap();
        let expected = [
            Token::Identifier(Rc::from("a")),
            Token::Operator(OperatorToken::Pipe),
            Token::Identifier(Rc::from("b")),
            Token::Operator(OperatorToken::Tilde),
            Token::Eof,
        ];
        assert_eq!(tokens, expected);

        let tokens = vec_tokens("").unwrap();
        let expected = [Token::Eof];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn lexer_with_incorrect_lexemes() {
        assert!(vec_tokens(" a ").is_ok());
        assert!(vec_tokens(" a").is_ok());
        assert!(vec_tokens("a ").is_ok());

        assert!(vec_tokens("<<-ab").is_err());
        assert!(vec_tokens("((->>c").is_err());
        assert!(vec_tokens("a\"c").is_err());

        // Non ascii characters are not supported
        assert!(vec_tokens("ñ").is_err());

        // Identifiers cannot start with digits
        assert!(vec_tokens("1fi -> fa").is_err());
    }
}
