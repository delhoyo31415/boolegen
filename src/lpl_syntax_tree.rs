use std::{convert::TryInto, str::FromStr};

use crate::{
    error::BoolegenError,
    parser::{Env, ExpressionNode, SyntaxTree},
};

pub trait StrExt {
    fn can_be_lpl_encoded(&self) -> bool;
}

impl<S: AsRef<str>> StrExt for S {
    // LPL boole only accepts as a variable a string which is
    // alphabetic and whose first letter is capitalized
    fn can_be_lpl_encoded(&self) -> bool {
        let mut chars = self.as_ref().chars();
        if let Some(first_letter) = chars.next() {
            first_letter.is_ascii_uppercase() && chars.all(char::is_alphabetic)
        } else {
            false
        }
    }
}

// A syntax tree which can be written to a LPL File
pub struct LplSyntaxTree {
    inner: SyntaxTree,
}

impl LplSyntaxTree {
    pub fn root(&self) -> &ExpressionNode {
        self.inner.root()
    }

    pub fn env(&self) -> &Env {
        self.inner.env()
    }

    pub fn transform_equivalent(&mut self) {
        self.inner.transform_equivalent()
    }
}

impl TryFrom<SyntaxTree> for LplSyntaxTree {
    type Error = BoolegenError;

    fn try_from(tree: SyntaxTree) -> Result<Self, Self::Error> {
        let incorrect_var = tree.env().names().find(|name| !name.can_be_lpl_encoded());
        match incorrect_var {
            Some(var) => Err(BoolegenError::InvalidLplVar(var.to_string())),
            None => Ok(LplSyntaxTree { inner: tree }),
        }
    }
}

impl FromStr for LplSyntaxTree {
    type Err = BoolegenError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let tree: SyntaxTree = s.parse()?;
        tree.try_into()
    }
}
