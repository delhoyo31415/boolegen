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
    fn and(&self, other: &BooleanValue) -> BooleanValue {
        if *self == BooleanValue::True && *other == BooleanValue::True {
            BooleanValue::True
        } else {
            BooleanValue::False
        }
    }

    fn or(&self, other: &BooleanValue) -> BooleanValue {
        if *self == BooleanValue::True || *other == BooleanValue::True {
            BooleanValue::True
        } else {
            BooleanValue::False
        }
    }

    fn conditional(&self, other: &BooleanValue) -> BooleanValue {
        if *self == BooleanValue::True && *other == BooleanValue::False {
            BooleanValue::False
        } else {
            BooleanValue::True
        }
    }

    fn biconditional(&self, other: &BooleanValue) -> BooleanValue {
        if *self == *other {
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
    fn apply(&self, first_op: &BooleanValue, second_op: &BooleanValue) -> BooleanValue {
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
    BinaryExpression {
        lhs: Box<ExpressionNode>,
        rhs: Box<ExpressionNode>,
        op: BinaryOperator,
    },
    NotExpression(Box<ExpressionNode>),
    Var(String),
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
            Ok(SyntaxTree { root: expr })
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

                    lhs = Box::new(ExpressionNode::BinaryExpression { lhs, rhs, op })
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
pub struct SyntaxTree {
    root: Box<ExpressionNode>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parser_correctly_parses_expression() {
        let tree = Parser::parse_from_str(
            "a -> ~(c | b & d)"
        ).unwrap();
        
        let expected = Box::new(ExpressionNode::BinaryExpression {
            lhs: Box::new(ExpressionNode::Var("a".to_string())), 
            rhs: Box::new(ExpressionNode::NotExpression(
                Box::new(ExpressionNode::BinaryExpression { 
                    lhs: Box::new(ExpressionNode::BinaryExpression { 
                        lhs: Box::new(ExpressionNode::Var("c".to_string())), 
                        rhs: Box::new(ExpressionNode::Var("b".to_string())),
                        op: BinaryOperator::Or
                    }), 
                    rhs: Box::new(ExpressionNode::Var("d".to_string())), 
                    op: BinaryOperator::And 
                })
            )),
            op: BinaryOperator::Conditional 
        });

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