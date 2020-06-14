use std::str::FromStr;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum OpCode {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Token {
    Int(isize),
    LeftParenthesis,
    RightParenthesis,
    OpCode(OpCode),
    EOF,
}

impl Token {
    pub fn same(self, other: Self) -> bool {
        match self {
            Token::Int(_) => {
                if let Token::Int(_) = other {
                    true
                } else {
                    false
                }
            }
            Token::LeftParenthesis => {
                if let Token::LeftParenthesis = other {
                    true
                } else {
                    false
                }
            }
            Token::RightParenthesis => {
                if let Token::RightParenthesis = other {
                    true
                } else {
                    false
                }
            }
            Token::OpCode(_) => {
                if let Token::OpCode(_) = other {
                    true
                } else {
                    false
                }
            }
            Token::EOF => {
                if let Token::EOF = other {
                    true
                } else {
                    false
                }
            }
        }
    }
}

pub fn lexer(text: &str) -> Vec<Token> {
    lexer_helper(text, vec![])
}

fn lexer_helper(text: &str, mut token_list: Vec<Token>) -> Vec<Token> {
    match text.chars().next() {
        Some(ch) => match ch {
            '+' => {
                token_list.push(Token::OpCode(OpCode::Add));
                lexer_helper(&text[1..], token_list)
            }
            '-' => {
                token_list.push(Token::OpCode(OpCode::Sub));
                lexer_helper(&text[1..], token_list)
            }
            '*' => {
                token_list.push(Token::OpCode(OpCode::Mul));
                lexer_helper(&text[1..], token_list)
            }
            '/' => {
                token_list.push(Token::OpCode(OpCode::Div));
                lexer_helper(&text[1..], token_list)
            }
            '(' => {
                token_list.push(Token::LeftParenthesis);
                lexer_helper(&text[1..], token_list)
            }
            ')' => {
                token_list.push(Token::RightParenthesis);
                lexer_helper(&text[1..], token_list)
            }
            _ if ch.is_ascii_digit() => lexe_number(&text, token_list),
            _ => lexer_helper(&text[1..], token_list),
        },
        None => {
            token_list.push(Token::EOF);
            token_list
        }
    }
}

fn lexe_number(text: &str, mut token_list: Vec<Token>) -> Vec<Token> {
    let num = text.chars().take_while(|ch| ch.is_ascii_digit()).count();
    let token = isize::from_str(text.split_at(num).0).unwrap();
    token_list.push(Token::Int(token));
    lexer_helper(&text[num..], token_list)
}

mod test {
    use super::*;

    #[test]
    fn lexer_test() {
        let list = lexer("1 +-*/()22");
        assert_eq!(
            list,
            vec![
                Token::Int(1),
                Token::OpCode(OpCode::Add),
                Token::OpCode(OpCode::Sub),
                Token::OpCode(OpCode::Mul),
                Token::OpCode(OpCode::Div),
                Token::LeftParenthesis,
                Token::RightParenthesis,
                Token::Int(22),
                Token::EOF
            ]
        )
    }
}
