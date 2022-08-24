use std::{collections::HashMap, rc::Rc};

use crate::{
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

// I create a new type instead of using bool because I don't
// want the value returned by the operators to be used in the same places where bool is
// allowed (i.e if statements)
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum BooleanValue {
    True,
    False,
}

impl BooleanValue {
    fn and(&self, other: BooleanValue) -> BooleanValue {
        if *self == BooleanValue::True && other == BooleanValue::True {
            BooleanValue::True
        } else {
            BooleanValue::False
        }
    }

    fn or(&self, other: BooleanValue) -> BooleanValue {
        if *self == BooleanValue::True || other == BooleanValue::True {
            BooleanValue::True
        } else {
            BooleanValue::False
        }
    }

    fn conditional(&self, other: BooleanValue) -> BooleanValue {
        if *self == BooleanValue::True && other == BooleanValue::False {
            BooleanValue::False
        } else {
            BooleanValue::True
        }
    }

    fn biconditional(&self, other: BooleanValue) -> BooleanValue {
        if *self == other {
            BooleanValue::True
        } else {
            BooleanValue::False
        }
    }

    fn not(&self) -> BooleanValue {
        if *self == BooleanValue::True {
            BooleanValue::False
        } else {
            BooleanValue::True
        }
    }
}

impl BinaryOperator {
    fn apply(&self, first_op: BooleanValue, second_op: BooleanValue) -> BooleanValue {
        match self {
            BinaryOperator::And => first_op.and(second_op),
            BinaryOperator::Or => first_op.or(second_op),
            BinaryOperator::Conditional => first_op.conditional(second_op),
            BinaryOperator::Biconditional => first_op.biconditional(second_op),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum ExpressionNode {
    BinaryExpression(Box<ExpressionNode>, Box<ExpressionNode>, BinaryOperator),
    NotExpression(Box<ExpressionNode>),
    Var(Rc<str>),
}

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Parser { lexer }
    }

    pub fn parse_from_str(s: &str) -> Result<SyntaxTree, BooleExprError> {
        let lexer = Lexer::from_str(s)?;
        Self::new(lexer).parse()
    }

    pub fn parse(&mut self) -> Result<SyntaxTree, BooleExprError> {
        let expr = self.parse_expression(Precedence::min())?;
        if self.lexer.peek() == Token::Eof {
            Ok(SyntaxTree::new(expr))
        } else {
            self.error("Missing opening parenthesis".to_string())
        }
    }

    fn parse_prefix(&mut self) -> Result<Box<ExpressionNode>, BooleExprError> {
        let lhs = match self.lexer.consume() {
            Token::Operator(OperatorToken::Tilde) => {
                let expr = self.parse_expression(OperatorToken::Tilde.precedende())?;
                Box::new(ExpressionNode::NotExpression(expr))
            }
            Token::Identifier(name) => Box::new(ExpressionNode::Var(name)),
            Token::LParen => {
                let expr = self.parse_expression(Precedence::min())?;
                if self.lexer.consume() != Token::RParen {
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
            match self.lexer.peek() {
                Token::Operator(op) if op.is_binary() => {
                    // This implies that in case of a tie, the operator will be associated to the left
                    if precedence >= op.precedende() {
                        break;
                    }
                    self.lexer.consume();

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
        let expr = self.parse_infix(lhs, precedence);
        expr
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
struct Env {
    map: HashMap<Rc<str>, usize>,
}

impl Env {
    fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    fn from_node(root: &ExpressionNode) -> Self {
        let mut env = Env::new();

        let mut stack = vec![root];
        while let Some(expr) = stack.pop() {
            match expr {
                ExpressionNode::BinaryExpression(lhs, rhs, _) => {
                    stack.push(rhs);
                    stack.push(lhs)
                }
                ExpressionNode::NotExpression(not_expr) => stack.push(not_expr),
                ExpressionNode::Var(name) => env.add(name),
            }
        }

        env
    }

    fn len(&self) -> usize {
        self.map.len()
    }

    fn add(&mut self, name: &Rc<str>) {
        let len = self.map.len();
        self.map.entry(Rc::clone(name)).or_insert(len);
    }

    fn get_position(&self, name: &str) -> Option<usize> {
        self.map.get(name).cloned()
    }

    fn names<'a>(&'a self) -> impl Iterator<Item = &'a str> {
        self.map.keys().map(|name| name.as_ref())
    }
}

fn interpret(node: &ExpressionNode, env: &Env, inputs: &[BooleanValue]) -> BooleanValue {
    match node {
        ExpressionNode::BinaryExpression(lhs, rhs, op) => {
            let lhs = interpret(lhs, env, inputs);
            let rhs = interpret(rhs, env, inputs);
            op.apply(lhs, rhs)
        },
        ExpressionNode::NotExpression(not_expr) => {
            let value = interpret(not_expr, env, inputs);
            value.not()
        },
        ExpressionNode::Var(name) => {
            let pos = env.get_position(name).unwrap();
            inputs[pos]
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
        if inputs.len() == self.env.len() {
            interpret(&self.root, &self.env, inputs)
        } else {
            panic!("The expression has {} variables but {} have been supplied", self.env.len(), inputs.len())
        }
    }

    fn new(root: Box<ExpressionNode>) -> Self {
        let env = Env::from_node(&root);
        Self { root, env }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parser_correctly_parses_expression() {
        let tree = Parser::parse_from_str("a -> ~(c | b & d)").unwrap();

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

        assert_eq!(tree.root, expected);
    }

    #[test]
    fn parser_detects_errors() {
        assert!(Parser::parse_from_str("").is_err());
        assert!(Parser::parse_from_str("(a").is_err());
        assert!(Parser::parse_from_str("a) -> c").is_err());
        assert!(Parser::parse_from_str("a~b").is_err());
        assert!(Parser::parse_from_str("a b <-> d").is_err());
        assert!(Parser::parse_from_str("a & ()").is_err());
    }
}
