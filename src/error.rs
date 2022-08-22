use std::{error::Error, fmt::Display};
#[derive(Debug)]


// TODO: Implement adequate traits for an error
pub enum BooleExprError {
    LexerError(String),
    ParserError(String)
}

// https://doc.rust-lang.org/std/fmt/index.html
impl Display for BooleExprError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BooleExprError::LexerError(chars) => write!(f, "Token '{chars}' not recognized"),
            BooleExprError::ParserError(msg) => msg.fmt(f)
        }
    }
}

impl Error for BooleExprError {}