use super::lexer::{lexer, OpCode, Token};
use crate::lexer::Keyword;

#[derive(Debug, Clone)]
pub enum Expr {
    Identifier(String),
    Number(isize),
    Op(Box<Operation>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    LetStatement(String, Expr),
}

#[derive(Debug, Clone)]
pub enum Operation {
    Add(Expr, Expr),
    Sub(Expr, Expr),
    Mul(Expr, Expr),
    Div(Expr, Expr),
}

#[derive(Debug, Clone)]
pub struct Ast {
    statement_list: Vec<Statement>,
    expresion: Expr,
}

impl Ast {
    fn new(inner: Expr, statement_list: Vec<Statement>) -> Self {
        Self {
            expresion: inner,
            statement_list,
        }
    }

    pub fn get_statement_list(self) -> Vec<Statement> {
        self.statement_list
    }

    pub fn get_expresion(self) -> Expr {
        self.expresion
    }
}

pub fn parser(input: &str) -> Ast {
    let token_list = lexer(input);

    let mut token_slice: &[Token] = &token_list;
    let mut statement_list = vec![];
    while let Some((statement, list)) = parse_statement(token_slice) {
        token_slice = list;
        statement_list.push(statement)
    }
    let (expr, token_list) = parse_expr(token_slice).unwrap();
    assert!(token_list.is_empty());
    Ast::new(expr, statement_list)
}

fn parse_statement(token_list: &[Token]) -> Option<(Statement, &[Token])> {
    let list = consume(Token::Keyword(Keyword::Let), token_list)?;
    if let Token::Identifier(ident) = list.first()? {
        let list = consume(Token::AssigmentSymbol, &list[1..])?;
        let (expr, list) = parse_expr(list)?;
        let list = consume(Token::NewLine, list)?;
        return Some((Statement::LetStatement(ident.clone(), expr), list));
    }
    None
}

fn parse_expr(token_list: &[Token]) -> Option<(Expr, &[Token])> {
    let (term1, list) = parse_term(token_list)?;
    loop {
        if let Some(token) = list.first() {
            match token {
                Token::OpCode(op) => {
                    let (term2, output_list) = parse_expr(&list[1..])?;
                    match op {
                        OpCode::Add => {
                            break Some((
                                Expr::Op(Box::new(Operation::Add(term1, term2))),
                                output_list,
                            ))
                        }
                        OpCode::Sub => {
                            break Some((
                                Expr::Op(Box::new(Operation::Sub(term1, term2))),
                                output_list,
                            ))
                        }
                        OpCode::Mul => {
                            break Some((
                                Expr::Op(Box::new(Operation::Mul(term1, term2))),
                                output_list,
                            ))
                        }
                        OpCode::Div => {
                            break Some((
                                Expr::Op(Box::new(Operation::Div(term1, term2))),
                                output_list,
                            ))
                        }
                    }
                }
                _ => {
                    return Some((term1, list));
                }
            }
        } else {
            return Some((term1, list));
        }
    }
}

fn parse_term(token_list: &[Token]) -> Option<(Expr, &[Token])> {
    let token = token_list.first().unwrap();
    match token {
        Token::LeftParenthesis => {
            let (expr, list) = parse_expr(&token_list[1..])?;
            Some((expr, consume(Token::RightParenthesis, list)?))
        }
        Token::Int(num) => Some((Expr::Number(*num), &token_list[1..])),
        Token::Identifier(ident) => Some((Expr::Identifier(ident.clone()), &token_list[1..])),
        _ => panic!("unexpected token {:?}", token),
    }
}

fn consume(token_type: Token, input: &[Token]) -> Option<&[Token]> {
    if *input.first().unwrap() == token_type {
        Some(&input[1..])
    } else {
        None
    }
}

mod test {
    use super::{parse_expr, parser, Expr, OpCode, Operation, Statement};
    use crate::lexer::Token;

    #[test]
    fn parse_statement() {
        let ast = parser(
            "let x = 2*10\n\
                      x + 5",
        );
        match ast.statement_list.first().unwrap().clone() {
            Statement::LetStatement(ident, expr) => {
                assert_eq!(ident, "x");
                match expr {
                    Expr::Op(op) => match *op {
                        Operation::Mul(expr1, expr2) => {
                            match expr1 {
                                Expr::Number(num) => assert_eq!(num, 2),
                                _ => panic!(),
                            }

                            match expr2 {
                                Expr::Number(num) => assert_eq!(num, 10),
                                _ => panic!(),
                            }
                        }
                        _ => panic!(),
                    },
                    _ => panic!(),
                }
            }
        }

        match ast.expresion {
            Expr::Op(op) => match *op {
                Operation::Add(expr1, expr2) => {
                    match expr1 {
                        Expr::Identifier(ident) => assert_eq!("x", ident),
                        _ => panic!(),
                    }
                    match expr2 {
                        Expr::Number(num) => assert_eq!(num, 5),
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    #[test]
    #[should_panic(expected = "unexpected token")]
    fn invalid_term() {
        let list: Vec<Token> = vec![Token::RightParenthesis, Token::LeftParenthesis];
        parse_expr(&list);
    }

    #[test]
    #[should_panic]
    fn invalid_term2() {
        let list = vec![
            Token::RightParenthesis,
            Token::Int(2),
            Token::OpCode(OpCode::Mul),
            Token::Int(2),
        ];
        parse_expr(&list);
    }

    #[test]
    #[should_panic]
    fn empty_parse() {
        parse_expr(&vec![]);
    }

    #[test]
    fn parse_literal_test() {
        let list = vec![Token::Int(1)];
        let (result, remainder) = parse_expr(&list).unwrap();
        assert!(remainder.is_empty());
        match result {
            Expr::Number(num) => assert_eq!(num, 1),
            _ => panic!(),
        }
    }

    #[test]
    fn parse_test() {
        let ast = parser("1 + (2 - (3*(   5 /     32)))");
        let _i = Box::new(1);
        match ast.expresion {
            Expr::Op(op) => match *op {
                Operation::Add(expr1, expr2) => {
                    match expr1 {
                        Expr::Number(num) => assert_eq!(num, 1),
                        _ => panic!(),
                    }

                    match expr2 {
                        Expr::Op(op) => match *op {
                            Operation::Sub(expr1, expr2) => {
                                match expr1 {
                                    Expr::Number(num) => assert_eq!(num, 2),
                                    _ => panic!(),
                                }

                                match expr2 {
                                    Expr::Op(op) => match *op {
                                        Operation::Mul(expr1, expr2) => {
                                            match expr1 {
                                                Expr::Number(num) => assert_eq!(num, 3),
                                                _ => panic!(),
                                            }

                                            match expr2 {
                                                Expr::Op(op) => match *op {
                                                    Operation::Div(expr1, expr2) => {
                                                        match expr1 {
                                                            Expr::Number(num) => assert_eq!(num, 5),
                                                            _ => panic!(),
                                                        }

                                                        match expr2 {
                                                            Expr::Number(num) => {
                                                                assert_eq!(num, 32)
                                                            }
                                                            _ => panic!(),
                                                        }
                                                    }
                                                    _ => panic!(),
                                                },
                                                _ => panic!(),
                                            }
                                        }
                                        _ => panic!(),
                                    },
                                    _ => panic!(),
                                }
                            }
                            _ => panic!(),
                        },
                        _ => panic!(),
                    };
                }
                _ => panic!(),
            },
            _ => panic!(),
        }
    }
}
