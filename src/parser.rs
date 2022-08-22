use crate::{
    error::BooleExprError,
    lexer::{Lexer, OperatorToken, Token, Precedence},
};

#[derive(Debug)]
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

#[derive(Debug)]
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

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Box<ExpressionNode>, BooleExprError> {
        let mut lhs = match self.lexer.consume() {
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
                    "Unexpected token '{wrong_token:?}', expected identifier, ~ or ("
                ))
            }
        };

        loop {
            match self.lexer.peek() {
                Token::Operator(op) => {
                    // This implies that in case of a tie, the operator will be associated to the left
                    if precedence >= op.precedende() {
                        break;
                    }
                    self.lexer.consume();

                    let rhs = self.parse_expression(op.precedende())?;
                    let op = self.parse_binary_operator(&op).unwrap();

                    lhs = Box::new(ExpressionNode::BinaryExpression { lhs, rhs, op })
                },
                Token::RParen | Token::Eof => break,
                wrong_token => {
                    return self.error(format!(
                        "Unexpected token '{wrong_token:?}', expected binary operator, '(' or EOF"
                    ))
                }
            }
        }

        Ok(lhs)
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