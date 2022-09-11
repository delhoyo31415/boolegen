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

use std::{cmp::Ordering, collections::HashMap, fmt::Display, rc::Rc, str::FromStr};

use crate::{
    boolean_value::BooleanValue,
    error::BooleExprError,
    lexer::{Lexer, Token},
};
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
pub struct Precedence(u32);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinaryOperator {
    And,
    Or,
    Conditional,
    Biconditional,
}

impl BinaryOperator {
    pub fn symbol(&self) -> &'static str {
        match self {
            BinaryOperator::And => "&",
            BinaryOperator::Or => "|",
            BinaryOperator::Conditional => "->",
            BinaryOperator::Biconditional => "<->",
        }
    }

    pub fn apply(&self, first_op: BooleanValue, second_op: BooleanValue) -> BooleanValue {
        match self {
            BinaryOperator::And => first_op.and(second_op),
            BinaryOperator::Or => first_op.or(second_op),
            BinaryOperator::Conditional => first_op.conditional(second_op),
            BinaryOperator::Biconditional => first_op.biconditional(second_op),
        }
    }

    pub fn is_associative(&self) -> bool {
        *self == BinaryOperator::And || *self == BinaryOperator::Or
    }

    pub fn precedence(&self) -> Precedence {
        match self {
            BinaryOperator::And | BinaryOperator::Or => Precedence(2),
            BinaryOperator::Conditional | BinaryOperator::Biconditional => Precedence(1),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Operator {
    Binary(BinaryOperator),
    Not,
}

impl Operator {
    pub fn precedence(&self) -> Precedence {
        match self {
            Operator::Binary(op) => op.precedence(),
            Operator::Not => Precedence(3),
        }
    }

    pub fn symbol(&self) -> &'static str {
        match self {
            Operator::Binary(op) => op.symbol(),
            Operator::Not => "~",
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExpressionNode {
    BinaryExpression(Box<ExpressionNode>, Box<ExpressionNode>, BinaryOperator),
    NotExpression(Box<ExpressionNode>),
    Var(Rc<str>),
}

impl ExpressionNode {
    pub fn is_var(&self) -> bool {
        matches!(self, ExpressionNode::Var(..))
    }

    pub fn postorder_traversal(&self) -> impl Iterator<Item = &Self> {
        PostorderTraversal::new(self)
    }

    pub fn preorder_traversal(&self) -> impl Iterator<Item = &Self> {
        PreorderTraversal { stack: vec![self] }
    }

    // THINK: should I store this property in the node itself?
    pub fn degree(&self) -> usize {
        self.preorder_traversal()
            .filter(|&node| !node.is_var())
            .count()
    }

    pub fn transform_equivalent(&mut self) {
        // the transformation only applies to binary operators
        if let ExpressionNode::BinaryExpression(lhs, rhs, _) = self {
            lhs.transform_equivalent();
            rhs.transform_equivalent();
            self.transform_current_equivalent();
        }
    }

    // Use the associative property of AND and OR binary operators
    // to reestructure this node so that when printed, it uses less parenthesis
    // What is actually being implemented is a left rotation
    // It is guaranteed that the result of eval will be the same, but no the structure
    fn transform_current_equivalent(&mut self) {
        if let ExpressionNode::BinaryExpression(lhs, rhs, op) = self {
            if let ExpressionNode::BinaryExpression(rhs_lhs, rhs_rhs, rhs_op) = &mut **rhs {
                if op == rhs_op && op.is_associative() {
                    std::mem::swap(lhs, rhs_rhs);
                    std::mem::swap(rhs_lhs, rhs_rhs);
                    std::mem::swap(lhs, rhs);
                }
            }
        }
    }

    pub fn eval(&self, env: &Env, inputs: &[BooleanValue]) -> BooleanValue {
        match self {
            ExpressionNode::NotExpression(not_expr) => not_expr.eval(env, inputs).not(),
            ExpressionNode::BinaryExpression(lhs, rhs, op) => {
                let lhs = lhs.eval(env, inputs);
                let rhs = rhs.eval(env, inputs);
                op.apply(lhs, rhs)
            }
            ExpressionNode::Var(name) => {
                let pos = env.get_position(name).expect("Name not found in given env");
                inputs
                    .get(pos)
                    .cloned()
                    .expect("Boolean value not supplied")
            }
        }
    }

    fn infix_notation(&self) -> String {
        match self {
            ExpressionNode::BinaryExpression(lhs, rhs, op) => {
                let lhs = if let ExpressionNode::BinaryExpression(.., lhs_op) = &**lhs {
                    if lhs_op.precedence() > op.precedence() || op == lhs_op && op.is_associative()
                    {
                        lhs.infix_notation()
                    } else {
                        format!("({})", lhs.infix_notation())
                    }
                } else {
                    lhs.infix_notation()
                };

                let rhs = match &**rhs {
                    ExpressionNode::BinaryExpression(.., rhs_op)
                        if rhs_op.precedence() <= op.precedence() =>
                    {
                        format!("({})", rhs.infix_notation())
                    }
                    _ => rhs.infix_notation(),
                };

                format!("{} {} {}", lhs, op.symbol(), rhs)
            }

            ExpressionNode::NotExpression(not_expr) => {
                let parenthesized = not_expr.infix_notation();
                if not_expr.is_var() {
                    format!("~{parenthesized}")
                } else {
                    format!("~({parenthesized})")
                }
            }
            ExpressionNode::Var(name) => name.to_string(),
        }
    }
}

impl Display for ExpressionNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.infix_notation().fmt(f)
    }
}

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self { lexer }
    }

    pub fn parse(&mut self) -> Result<SyntaxTree, BooleExprError> {
        let expr = self.parse_expression(None)?;
        if self.lexer.peek()? == Token::Eof {
            Ok(SyntaxTree::new(expr))
        } else {
            self.error("Missing opening parenthesis".to_string())
        }
    }

    fn parse_prefix(&mut self) -> Result<Box<ExpressionNode>, BooleExprError> {
        let lhs = match self.lexer.consume()? {
            Token::Tilde => {
                let expr = self.parse_expression(Some(Operator::Not))?;
                Box::new(ExpressionNode::NotExpression(expr))
            }
            Token::Identifier(name) => Box::new(ExpressionNode::Var(name)),
            Token::LParen => {
                let expr = self.parse_expression(None)?;
                if self.lexer.consume()? != Token::RParen {
                    return self.error("Missing closing parenthesis".to_string());
                }
                expr
            }
            wrong_token => {
                return self.error(format!(
                    "Unexpected token '{wrong_token}', expected identifier, ~ or ("
                ))
            }
        };

        Ok(lhs)
    }

    fn stop_recursion(
        &self,
        binary_op: BinaryOperator,
        prev_op: Option<Operator>,
    ) -> Result<bool, BooleExprError> {
        if let Some(prev_op) = prev_op {
            match prev_op.precedence().cmp(&binary_op.precedence()) {
                Ordering::Equal => {
                    if Operator::Binary(binary_op) == prev_op && binary_op.is_associative() {
                        Ok(true)
                    } else {
                        self.error(format!(
                            "Ambiguous expression, '{}' followed by '{}'",
                            prev_op.symbol(),
                            binary_op.symbol()
                        ))
                    }
                }
                Ordering::Greater => Ok(true),
                Ordering::Less => Ok(false),
            }
        } else {
            Ok(false)
        }
    }

    fn parse_infix(
        &mut self,
        mut lhs: Box<ExpressionNode>,
        prev_op: Option<Operator>,
    ) -> Result<Box<ExpressionNode>, BooleExprError> {
        loop {
            let token = self.lexer.peek()?;

            match token {
                Token::Ampersand | Token::Pipe | Token::Arrow | Token::DoubleArrow => {
                    let op = self.parse_binary_operator(&token).unwrap();

                    if self.stop_recursion(op, prev_op)? {
                        break;
                    }

                    self.lexer.consume()?;
                    let rhs = self.parse_expression(Some(Operator::Binary(op)))?;
                    lhs = Box::new(ExpressionNode::BinaryExpression(lhs, rhs, op))
                }
                Token::RParen | Token::Eof => break,
                wrong_token => {
                    return self.error(format!(
                        "Unexpected token '{wrong_token}', expected binary operator, '(' or EOF"
                    ))
                }
            }
        }
        Ok(lhs)
    }

    fn parse_expression(
        &mut self,
        prev_op: Option<Operator>,
    ) -> Result<Box<ExpressionNode>, BooleExprError> {
        let lhs = self.parse_prefix()?;
        self.parse_infix(lhs, prev_op)
    }

    // If op is not a binary operator, return None
    fn parse_binary_operator(&self, token: &Token) -> Option<BinaryOperator> {
        match token {
            Token::Ampersand => Some(BinaryOperator::And),
            Token::Arrow => Some(BinaryOperator::Conditional),
            Token::DoubleArrow => Some(BinaryOperator::Biconditional),
            Token::Pipe => Some(BinaryOperator::Or),
            _ => None,
        }
    }

    fn error<T>(&self, msg: String) -> Result<T, BooleExprError> {
        Err(BooleExprError::ParserError(msg))
    }
}

#[derive(Debug, Default, Clone)]
pub struct Env {
    // Map the position to the string
    names: Vec<Rc<str>>,
    // Maps a name to the first position where string has been found
    map: HashMap<Rc<str>, usize>,
}

impl Env {
    pub fn new() -> Self {
        Default::default()
    }

    fn from_node(node: &ExpressionNode) -> Self {
        let mut env = Env::new();

        for node in node.preorder_traversal() {
            if let ExpressionNode::Var(name) = node {
                env.add(Rc::clone(name))
            }
        }

        env
    }

    // Creates a new env containing all the variables
    // in the given iterator of envs
    pub fn merged<'a, I>(envs: I) -> Env
    where
        I: IntoIterator<Item = &'a Env>,
    {
        let mut new_env = Env::new();
        for env in envs {
            new_env.add_all(env.names())
        }
        new_env
    }

    pub fn add_all<I, T>(&mut self, vars: I)
    where
        I: IntoIterator<Item = T>,
        T: AsRef<str>,
    {
        for var in vars {
            self.add(var.as_ref().into())
        }
    }

    pub fn var_count(&self) -> usize {
        self.names.len()
    }

    pub fn add(&mut self, name: Rc<str>) {
        let len = self.map.len();

        if self.map.get(&name).is_none() {
            self.map.insert(Rc::clone(&name), len);
            self.names.push(name);
        }
    }

    // Returns the position of a given name, starting at 0
    pub fn get_position(&self, name: &str) -> Option<usize> {
        self.map.get(name).cloned()
    }

    pub fn get_name(&self, position: usize) -> Option<&str> {
        self.names.get(position).map(|name| name.as_ref())
    }

    pub fn names(&self) -> impl Iterator<Item = &Rc<str>> {
        self.names.iter()
    }
}

#[derive(Debug)]
pub struct SyntaxTree {
    root: Box<ExpressionNode>,
    // Maps a variable to the position where it has been found first
    // while parsing
    env: Env,
}

impl SyntaxTree {
    pub fn eval(&self, inputs: &[BooleanValue]) -> BooleanValue {
        if inputs.len() == self.env.var_count() {
            self.root.eval(&self.env, inputs)
        } else {
            panic!(
                "The expression has {} variables but {} have been supplied",
                self.env.var_count(),
                inputs.len()
            )
        }
    }

    pub fn degree(&self) -> usize {
        self.root.degree()
    }

    pub fn preorder_traversal(&self) -> impl Iterator<Item = &ExpressionNode> {
        self.root.preorder_traversal()
    }

    pub fn postorder_traversal(&self) -> impl Iterator<Item = &ExpressionNode> {
        self.root.postorder_traversal()
    }

    pub fn transform_equivalent(&mut self) {
        self.root.transform_equivalent();
    }

    pub fn root(&self) -> &ExpressionNode {
        &self.root
    }

    pub fn env(&self) -> &Env {
        &self.env
    }

    fn new(root: Box<ExpressionNode>) -> Self {
        let env = Env::from_node(&root);
        Self { root, env }
    }
}

impl FromStr for SyntaxTree {
    type Err = BooleExprError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lexer = Lexer::new(s);
        Parser::new(lexer).parse()
    }
}

impl Display for SyntaxTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.root().fmt(f)
    }
}

struct PreorderTraversal<'a> {
    stack: Vec<&'a ExpressionNode>,
}

impl<'a> Iterator for PreorderTraversal<'a> {
    type Item = &'a ExpressionNode;

    fn next(&mut self) -> Option<Self::Item> {
        let node = self.stack.pop()?;

        match node {
            ExpressionNode::BinaryExpression(lhs, rhs, _) => {
                self.stack.push(rhs);
                self.stack.push(lhs);
            }
            ExpressionNode::NotExpression(not_expr) => self.stack.push(not_expr),
            ExpressionNode::Var(_) => (),
        }

        Some(node)
    }
}

struct PostorderTraversal<'a> {
    output: Vec<&'a ExpressionNode>,
}

impl<'a> PostorderTraversal<'a> {
    fn new(node: &'a ExpressionNode) -> Self {
        let mut input = vec![node];
        let mut output = Vec::new();

        while let Some(node) = input.pop() {
            match node {
                ExpressionNode::BinaryExpression(lhs, rhs, _) => {
                    input.push(lhs);
                    input.push(rhs);
                }
                ExpressionNode::NotExpression(expr) => input.push(expr),
                ExpressionNode::Var(_) => {}
            }
            output.push(node);
        }

        Self { output }
    }
}

impl<'a> Iterator for PostorderTraversal<'a> {
    type Item = &'a ExpressionNode;

    fn next(&mut self) -> Option<Self::Item> {
        self.output.pop()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parser_correctly_parses_expression() {
        let tree = "a -> ~((c | b) & d)".parse::<SyntaxTree>();

        let expected = Box::new(ExpressionNode::BinaryExpression(
            Box::new(ExpressionNode::Var(Rc::from("a"))),
            Box::new(ExpressionNode::NotExpression(Box::new(
                ExpressionNode::BinaryExpression(
                    Box::new(ExpressionNode::BinaryExpression(
                        Box::new(ExpressionNode::Var(Rc::from("c"))),
                        Box::new(ExpressionNode::Var(Rc::from("b"))),
                        BinaryOperator::Or,
                    )),
                    Box::new(ExpressionNode::Var(Rc::from("d"))),
                    BinaryOperator::And,
                ),
            ))),
            BinaryOperator::Conditional,
        ));

        assert_eq!(tree.unwrap().root, expected);
    }

    #[test]
    fn parser_detects_errors() {
        assert!("".parse::<SyntaxTree>().is_err());
        assert!("(a".parse::<SyntaxTree>().is_err());
        assert!("a) -> c".parse::<SyntaxTree>().is_err());
        assert!("a~b".parse::<SyntaxTree>().is_err());
        assert!("a b <-> d".parse::<SyntaxTree>().is_err());
        assert!("a & ()".parse::<SyntaxTree>().is_err());

        // These expressions are ambiguous expressions
        assert!("a -> b -> c".parse::<SyntaxTree>().is_err());
        assert!("a | b & c".parse::<SyntaxTree>().is_err());
        assert!("a | b | c <-> c & c -> d".parse::<SyntaxTree>().is_err());
    }

    #[test]
    fn test_env() {
        let tree = SyntaxTree::from_str("a -> b & c & a & b ").unwrap();
        let env = tree.env();

        assert_eq!(env.var_count(), 3);
        assert_eq!(env.get_name(1).unwrap(), "b");
        assert_eq!(
            env.names().map(AsRef::as_ref).collect::<Vec<_>>(),
            vec!["a", "b", "c"]
        );
    }

    #[test]
    fn syntax_tree_evaluates_expression_correctly() {
        // Notice this a tautology, so for any value a,b the expression is always true
        let tree: SyntaxTree = "(a -> b) <-> (~a | b)".parse().unwrap();

        assert_eq!(
            tree.eval(&[BooleanValue::False, BooleanValue::False]),
            BooleanValue::True
        );
        assert_eq!(
            tree.eval(&[BooleanValue::False, BooleanValue::True]),
            BooleanValue::True
        );
        assert_eq!(
            tree.eval(&[BooleanValue::True, BooleanValue::False]),
            BooleanValue::True
        );
        assert_eq!(
            tree.eval(&[BooleanValue::True, BooleanValue::True]),
            BooleanValue::True
        );
    }

    #[test]
    fn tree_correctly_transformed() {
        let mut tree: SyntaxTree = "a | (b | c)".parse().unwrap();
        tree.transform_equivalent();
        let expected: SyntaxTree = "a | b | c".parse().unwrap();
        assert_eq!(tree.root(), expected.root());

        let mut tree: SyntaxTree = "a -> ((b -> c) | ((d -> c) | (a & d)))".parse().unwrap();
        tree.transform_equivalent();
        let expected: SyntaxTree = "a -> ((b -> c) | (d -> c) | (a & d))".parse().unwrap();
        assert_eq!(tree.root(), expected.root());
    }

    #[test]
    fn tree_traversed_in_postorder() {
        let tree: SyntaxTree = "a & b -> c | ~d".parse().unwrap();
        let mut postorder = tree.postorder_traversal();

        assert_eq!(postorder.next().unwrap().to_string(), "a");
        assert_eq!(postorder.next().unwrap().to_string(), "b");
        assert_eq!(postorder.next().unwrap().to_string(), "a & b");
        assert_eq!(postorder.next().unwrap().to_string(), "c");
        assert_eq!(postorder.next().unwrap().to_string(), "d");
        assert_eq!(postorder.next().unwrap().to_string(), "~d");
        assert_eq!(postorder.next().unwrap().to_string(), "c | ~d");
        assert_eq!(postorder.next().unwrap().to_string(), "a & b -> c | ~d");
        assert_eq!(postorder.next(), None);
    }

    #[test]
    fn test_merge_envs() {
        let mut first = Env::new();
        first.add_all(&["first", "second"]);
        first.add_all(&["third", "second", "third"]);

        assert_eq!(
            first.names().map(AsRef::as_ref).collect::<Vec<_>>(),
            &["first", "second", "third"]
        );

        let mut second = Env::new();
        second.add_all(&["fourth", "fifth", "fourth"]);

        let merged = Env::merged(&[first, second]);

        assert_eq!(
            merged.names().map(AsRef::as_ref).collect::<Vec<_>>(),
            &["first", "second", "third", "fourth", "fifth"]
        );
    }
}
