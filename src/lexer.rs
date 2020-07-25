use std::str::FromStr;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum OpCode {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Int(isize),
    Keyword(Keyword),
    LeftParenthesis,
    RightParenthesis,
    AssigmentSymbol,
    Identifier(String),
    OpCode(OpCode),
    NewLine,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Keyword {
    Let,
}

impl Token {
    /// a helper function that try that convert a ident token to a keyword token if it actually correspond to a keyword.
    /// It will return None if the conversion is not possible and the token is not already a keyword
    fn try_ident_to_keyword(&self) -> Option<Keyword> {
        return if let Token::Identifier(string) = self {
            let text = string.as_str();

            match text {
                "let" => Some(Keyword::Let),
                _ => None,
            }
        } else if let Token::Keyword(keyword) = self {
            Some(*keyword)
        } else {
            None
        };
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
            '=' => {
                token_list.push(Token::AssigmentSymbol);
                lexer_helper(&text[1..], token_list)
            }
            _ if ch.is_ascii_digit() => lexe_number(&text, token_list),
            _ if ch.is_alphanumeric() => lexe_ident(text, token_list),
            '\n' => {
                token_list.push(Token::NewLine);
                lexer_helper(&text[1..], token_list)
            }
            _ => lexer_helper(&text[1..], token_list),
        },
        None => token_list,
    }
}

fn lexe_number(text: &str, mut token_list: Vec<Token>) -> Vec<Token> {
    let num = text.chars().take_while(|ch| ch.is_ascii_digit()).count();
    let token = isize::from_str(text.split_at(num).0).unwrap();
    token_list.push(Token::Int(token));
    lexer_helper(&text[num..], token_list)
}

fn lexe_ident(text: &str, mut token_list: Vec<Token>) -> Vec<Token> {
    return if let Some(ident) = text.split_whitespace().next() {
        let token = Token::Identifier(ident.to_string());
        if let Some(keyword) = token.try_ident_to_keyword() {
            token_list.push(Token::Keyword(keyword));
        } else {
            token_list.push(token)
        }
        lexer_helper(&text[ident.len()..], token_list)
    } else {
        panic!()
    };
}

mod test {
    use super::{lexer, OpCode, Token,  Keyword};

    #[test]
    fn lexer_test() {
        let list = lexer("let x =1\n\
                                         +-*/()22");
        assert_eq!(
            list,
            vec![
                Token::Keyword(Keyword::Let),
                Token::Identifier("x".to_string()),
                Token::AssigmentSymbol,
                Token::Int(1),
                Token::NewLine,
                Token::OpCode(OpCode::Add),
                Token::OpCode(OpCode::Sub),
                Token::OpCode(OpCode::Mul),
                Token::OpCode(OpCode::Div),
                Token::LeftParenthesis,
                Token::RightParenthesis,
                Token::Int(22),
            ]
        )
    }
}
