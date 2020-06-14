use super::lexer::{lexer, OpCode, Token};

#[derive(Debug)]
pub enum Expr {
    Number(isize),
    Op(Box<Operation>),
}

#[derive(Debug)]
pub enum Operation {
    Add(Expr, Expr),
    Sub(Expr, Expr),
    Mul(Expr, Expr),
    Div(Expr, Expr),
}

#[derive(Debug)]
pub struct Ast {
    pub inner: Expr,
}

impl Ast {
    fn new(inner: Expr) -> Self {
        Self { inner }
    }
}

pub fn parser(input: &str) -> Ast {
    let token_list = lexer(input);
    let (expr, _) = parse_expr(&token_list);
    Ast::new(expr)
}

fn parse_expr(token_list: &[Token]) -> (Expr, &[Token]) {
    let (term1, list) = parse_term(token_list);
    return if let Some(Token::OpCode(op)) = list.first() {
        let (term2, output_list) = parse_expr(&list[1..]);
        match op {
            OpCode::Add => (
                Expr::Op(Box::new(Operation::Add(term1, term2))),
                output_list,
            ),
            OpCode::Sub => (
                Expr::Op(Box::new(Operation::Sub(term1, term2))),
                output_list,
            ),
            OpCode::Mul => (
                Expr::Op(Box::new(Operation::Mul(term1, term2))),
                output_list,
            ),
            OpCode::Div => (
                Expr::Op(Box::new(Operation::Div(term1, term2))),
                output_list,
            ),
        }
    } else {
        (term1, list)
    };
}

fn parse_term(token_list: &[Token]) -> (Expr, &[Token]) {
    match *token_list.first().unwrap() {
        Token::LeftParenthesis => {
            let (expr, list) = parse_expr(&token_list[1..]);
            (expr, consume(Token::RightParenthesis, list))
        }
        Token::Int(num) => (Expr::Number(num), &token_list[1..]),
        _ => panic!("unexpected token {}", token_list.first().unwrap()),
    }
}

fn consume(token_type: Token, input: &[Token]) -> &[Token] {
    if input.first().unwrap().same(token_type) {
        &input[1..]
    } else {
        panic!("unexpected token {}", input.first().unwrap())
    }
}

mod test {
    use super::*;

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
        let (result, remainder) = parse_expr(&list);
        assert!(remainder.is_empty());
        match result {
            Expr::Number(num) => assert_eq!(num, 1),
            _ => panic!(),
        }
    }

    #[test]
    fn parse_test() {
        let ast = parser("1 + (2 - (3*(   5 /     32)))");
        match ast.inner {
            Expr::Op(op) => {
                match *op {
                    Operation::Add(expr1, expr2) => {
                        match expr1 {
                            Expr::Number(num) => assert_eq!(num, 1),
                            _ => panic!(),
                        }

                        match expr2 {
                            Expr::Op(op) => {
                                match *op {
                                    Operation::Sub(expr1, expr2) => {
                                        match expr1 {
                                            Expr::Number(num) => assert_eq!(num, 2),
                                            _ => panic!(),
                                        }

                                        match expr2 {
                                            Expr::Op(op) => {
                                                match *op {
                                                    Operation::Mul(expr1, expr2) => {
                                                        match expr1 {
                                                            Expr::Number(num) => assert_eq!(num, 3),
                                                            _ => panic!(),
                                                        }

                                                        match expr2 {
                                                            Expr::Op(op) => {
                                                                match *op {
                                                                    Operation::Div(expr1, expr2) => {
                                                                        match expr1 {
                                                                            Expr::Number(num) => assert_eq!(num, 5),
                                                                            _ => panic!(),
                                                                        }

                                                                        match expr2 {
                                                                            Expr::Number(num) => {
                                                                                assert_eq!(num, 32)
                                                                            },
                                                                            _ => panic!()
                                                                        }
                                                                    },
                                                                    _ => panic!(),
                                                                }
                                                            },
                                                            _ => panic!(),
                                                        }
                                                    },
                                                    _ => panic!(),
                                                }
                                            },
                                            _ => panic!(),
                                        }
                                    },
                                    _ => panic!(),
                                }
                            },
                            _ => panic!(),
                        };
                    },
                    _ => panic!(),
                }
            },
            _ => panic!(),
        }
    }
}
