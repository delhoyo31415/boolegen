use std::{collections::HashMap, rc::Rc, str::FromStr};

use crate::{
    boolean_value::BooleanValue,
    error::BooleExprError,
    lexer::{Lexer, OperatorToken, Precedence, Token},
};

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOperator {
    And,
    Or,
    Conditional,
    Biconditional,
}

impl BinaryOperator {
    pub fn apply(&self, first_op: BooleanValue, second_op: BooleanValue) -> BooleanValue {
        match self {
            BinaryOperator::And => first_op.and(second_op),
            BinaryOperator::Or => first_op.or(second_op),
            BinaryOperator::Conditional => first_op.conditional(second_op),
            BinaryOperator::Biconditional => first_op.biconditional(second_op),
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

    pub fn preorder_traversal(&self) -> impl Iterator<Item = &Self> {
        PreorderTraversal { stack: vec![self] }
    }

    pub fn degree(&self) -> usize {
        self.preorder_traversal()
            .filter(|&node| !node.is_var())
            .count()
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
}

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self { lexer }
    }

    pub fn parse(&mut self) -> Result<SyntaxTree, BooleExprError> {
        let expr = self.parse_expression(Precedence::min())?;
        if self.lexer.peek()? == Token::Eof {
            Ok(SyntaxTree::new(expr))
        } else {
            self.error("Missing opening parenthesis".to_string())
        }
    }

    fn parse_prefix(&mut self) -> Result<Box<ExpressionNode>, BooleExprError> {
        let lhs = match self.lexer.consume()? {
            Token::Operator(OperatorToken::Tilde) => {
                let expr = self.parse_expression(OperatorToken::Tilde.precedende())?;
                Box::new(ExpressionNode::NotExpression(expr))
            }
            Token::Identifier(name) => Box::new(ExpressionNode::Var(name)),
            Token::LParen => {
                let expr = self.parse_expression(Precedence::min())?;
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

    fn parse_infix(
        &mut self,
        mut lhs: Box<ExpressionNode>,
        precedence: Precedence,
    ) -> Result<Box<ExpressionNode>, BooleExprError> {
        loop {
            match self.lexer.peek()? {
                Token::Operator(op) if op.is_binary() => {
                    // This implies that in case of a tie, the operator will be associated to the left
                    if precedence >= op.precedende() {
                        break;
                    }
                    self.lexer.consume()?;

                    let rhs = self.parse_expression(op.precedende())?;
                    let op = self.parse_binary_operator(&op).unwrap();

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
        precedence: Precedence,
    ) -> Result<Box<ExpressionNode>, BooleExprError> {
        let lhs = self.parse_prefix()?;
        self.parse_infix(lhs, precedence)
    }

    // If op is not a binary operator, return None
    fn parse_binary_operator(&self, op: &OperatorToken) -> Option<BinaryOperator> {
        match op {
            OperatorToken::Ampersand => Some(BinaryOperator::And),
            OperatorToken::Arrow => Some(BinaryOperator::Conditional),
            OperatorToken::DoubleArrow => Some(BinaryOperator::Biconditional),
            OperatorToken::Pipe => Some(BinaryOperator::Or),
            _ => None,
        }
    }

    fn error<T>(&self, msg: String) -> Result<T, BooleExprError> {
        Err(BooleExprError::ParserError(msg))
    }
}

#[derive(Debug)]
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

    pub fn names_iter(&self) -> impl Iterator<Item = &str> {
        self.names.iter().map(|name| name.as_ref())
    }
}

impl Default for Env {
    fn default() -> Self {
        Self {
            names: Vec::new(),
            map: HashMap::new(),
        }
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

impl FromStr for SyntaxTree {
    type Err = BooleExprError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lexer = Lexer::new(s);
        Parser::new(lexer).parse()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parser_correctly_parses_expression() {
        let tree = "a -> ~(c | b & d)".parse::<SyntaxTree>();

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
    }

    #[test]
    fn test_env() {
        let tree = SyntaxTree::from_str("a -> b & c & a & b ").unwrap();
        let env = tree.env();

        assert_eq!(env.var_count(), 3);
        assert_eq!(env.get_name(1).unwrap(), "b");
        assert_eq!(env.names_iter().collect::<Vec<_>>(), vec!["a", "b", "c"]);
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
}
