// Copyright (C) 2022 Pablo del Hoyo Abad <pablodelhoyo1314@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

use std::{error::Error, fmt::Display};
#[derive(Debug, Clone)]
pub enum BooleExprError {
    LexerError(String),
    ParserError(String),
}

impl Display for BooleExprError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BooleExprError::LexerError(chars) => write!(f, "Token '{chars}' not recognized"),
            BooleExprError::ParserError(msg) => msg.fmt(f),
        }
    }
}

impl Error for BooleExprError {}

#[derive(Debug, Clone)]
pub enum FileGeneratorError {
    InvalidExpression(String),
}

impl Display for FileGeneratorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FileGeneratorError::InvalidExpression(msg) => msg.fmt(f),
        }
    }
}

impl Error for FileGeneratorError {}
