use std::str::FromStr;

#[derive(Debug, Copy, Clone)]
pub enum Error {
    UnknownToken,
    InvalidInteger,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq)]
/// Token that are produce by the lexer. See [`lexer`](fn.lexer.html) for more information.
pub enum Token {
    Integer(u64),
    Keyword(Keyword),
    LeftParenthesis,
    RightParenthesis,
    AssigmentSymbol,
    Identifier(String),
    OpCode(Operator),
    NewLine,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Keyword {
    Let,
}

/// Tokenize the given code into a vec of [`Token`](enum.Token.html) with the following lexical grammar.
/// ***
/// Lexical Grammar
/// ---
/// - Token::OpCode(Operator::Add) = "+"
/// - Token::OpCode(Operator::Sub) = "-"
/// - Token::OpCode(Operator::Mul) = "*"
/// - Token::OpCode(Operator::Div) = "/"
/// - Token::LeftParenthesis = "("
/// - Token::RightParenthesis = ")"
/// - Token::AssigmentSymbol = "="
/// - Token::NewLine = "\n"
/// - Token::Integer = (is_ascii_digit() == true)+
/// - Token::Keyword(Keyword::Let) = "let"
/// - Token::Identifier = (is_alphanumeric() == true)+
/// - _ = (is_whitespace())
pub fn lexer(text: &str) -> Result<Vec<Token>, Error> {
    lexer_helper(text, vec![])
}

/// ***
/// Lexical Grammar
/// ---
/// - Token::OpCode(Operator::Add) = "+"
/// - Token::OpCode(Operator::Sub) = "-"
/// - Token::OpCode(Operator::Mul) = "*"
/// - Token::OpCode(Operator::Div) = "/"
/// - Token::LeftParenthesis = "("
/// - Token::RightParenthesis = ")"
/// - Token::AssigmentSymbol = "="
/// - Token::NewLine = "\n"
/// - Token::Int = (is_ascii_digit() == true)+
/// - Token::Keyword(Keyword::Let) = "let"
/// - Token::Identifier = (is_alphanumeric() == true)+
/// - _ = (is_whitespace())
fn lexer_helper(text: &str, mut token_list: Vec<Token>) -> Result<Vec<Token>, Error> {
    let ch = match text.chars().next() {
        Some(ch) => ch,
        None => return Ok(token_list),
    };

    return if let Some(token) = tokenize_operator(ch) {
        token_list.push(token);
        lexer_helper(&text[1..], token_list)
    } else if let Some(token) = tokenize_symbol(ch) {
        token_list.push(token);
        lexer_helper(&text[1..], token_list)
    } else if let Some((token, integer_len)) = tokenize_integer(text)? {
        token_list.push(token);
        lexer_helper(&text[integer_len..], token_list)
    } else if let Some(token) = tokenize_keyword(text) {
        token_list.push(token);
        lexer_helper(&text[3..], token_list)
    } else if let Some((token, ident_len)) = tokenize_identifier(text) {
        token_list.push(token);
        lexer_helper(&text[ident_len..], token_list)
    } else if ch.is_whitespace() {
        lexer_helper(&text[1..], token_list)
    } else {
        Err(Error::UnknownToken)
    };
}

/// - Token::OpCode(Operator::Add) = "+"
/// - Token::OpCode(Operator::Sub) = "-"
/// - Token::OpCode(Operator::Mul) = "*"
/// - Token::OpCode(Operator::Div) = "/"
fn tokenize_operator(ch: char) -> Option<Token> {
    match ch {
        '+' => Some(Token::OpCode(Operator::Add)),
        '-' => Some(Token::OpCode(Operator::Sub)),
        '*' => Some(Token::OpCode(Operator::Mul)),
        '/' => Some(Token::OpCode(Operator::Div)),
        _ => None,
    }
}

/// - Token::LeftParenthesis = "("
/// - Token::RightParenthesis = ")"
/// - Token::AssigmentSymbol = "="
/// - Token::NewLine = "\n"
fn tokenize_symbol(ch: char) -> Option<Token> {
    match ch {
        '(' => Some(Token::LeftParenthesis),
        ')' => Some(Token::RightParenthesis),
        '=' => Some(Token::AssigmentSymbol),
        '\n' => Some(Token::NewLine),
        _ => None,
    }
}

/// Token::Int = (is_ascii_digit() == true)+
fn tokenize_integer(text: &str) -> Result<Option<(Token, usize)>, Error> {
    let num = text.chars().take_while(|ch| ch.is_ascii_digit()).count();
    if num == 0 {
        return Ok(None);
    }
    match u64::from_str(&text[..num]) {
        Ok(int) => Ok(Some((Token::Integer(int), num))),
        Err(_) => Err(Error::InvalidInteger),
    }
}

/// Token::Keyword(Keyword::Let) = "let"
fn tokenize_keyword(text: &str) -> Option<Token> {
    if text.starts_with("let") {
        Some(Token::Keyword(Keyword::Let))
    } else {
        None
    }
}

/// Token::Identifier = (is_alphanumeric() == true)+
fn tokenize_identifier(text: &str) -> Option<(Token, usize)> {
    let identifier: String = text.chars().take_while(|ch| ch.is_alphanumeric()).collect();
    let len = identifier.len();
    if len != 0 {
        Some((Token::Identifier(identifier), len))
    } else {
        None
    }
}

mod test {
    use super::{lexer, Keyword, Operator, Token};

    #[test]
    fn lexer_test() {
        let list = lexer(
            "let x =1\n\
                  +-*/()22",
        )
        .unwrap();

        assert_eq!(
            list,
            vec![
                Token::Keyword(Keyword::Let),
                Token::Identifier("x".to_string()),
                Token::AssigmentSymbol,
                Token::Integer(1),
                Token::NewLine,
                Token::OpCode(Operator::Add),
                Token::OpCode(Operator::Sub),
                Token::OpCode(Operator::Mul),
                Token::OpCode(Operator::Div),
                Token::LeftParenthesis,
                Token::RightParenthesis,
                Token::Integer(22),
            ]
        )
    }
}
