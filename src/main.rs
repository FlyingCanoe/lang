use std::fmt;
use std::fmt::{Display, Formatter};

use lexer::{OpCode, Token};
use parser::{parser, Expr, Operation};

mod compile;
mod lexer;
mod parser;

fn main() {
    let expr = parser("123 + (3 * 45)");
    dbg!(expr.inner.eval());
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Token::Int(num) => write!(f, "Number({})", num),
            Token::OpCode(op) => match op {
                OpCode::Add => write!(f, "'+'"),
                OpCode::Sub => write!(f, "'-'"),
                OpCode::Mul => write!(f, "'*'"),
                OpCode::Div => write!(f, "'/'"),
            },
            Token::LeftParenthesis => write!(f, "'('"),
            Token::RightParenthesis => write!(f, "')'"),
            Token::EOF => write!(f, "'End Of File'"),
        }
    }
}
