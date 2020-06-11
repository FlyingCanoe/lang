use std::fmt::{Display, Formatter};
use std::str::FromStr;
use std::{fmt, mem};

use cranelift::prelude::*;
use cranelift_module::{Linkage, Module};
use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};

#[derive(Debug)]
enum Expr {
    Number(isize),
    Op(Box<Operation>),
}

#[derive(Debug)]
enum Operation {
    Add(Expr, Expr),
    Sub(Expr, Expr),
    Mul(Expr, Expr),
    Div(Expr, Expr),
}

#[derive(Debug)]
struct Ast {
    inner: Expr,
}

impl Ast {
    fn new(inner: Expr) -> Self {
        Self { inner }
    }
}

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
    fn same(self, other: Self) -> bool {
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

fn parser(input: &str) -> Ast {
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

fn main() {
    let expr = parser("123 + (3 * 45)");
    expr.inner.eval()
}

impl Expr {
    pub fn eval(&self) {
        let builder = SimpleJITBuilder::new(cranelift_module::default_libcall_names());
        let mut module: Module<SimpleJITBackend> = Module::new(builder);
        let mut ctx = module.make_context();
        let int = module.target_config().pointer_type();

        let mut function_builder = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut function_builder);

        let entry_block = builder.create_block();

        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let value = self.translate(&mut builder, int);
        builder.ins().return_(&[value]);

        ctx.func.signature.returns.push(AbiParam::new(int));

        let id = module
            .declare_function("eval", Linkage::Export, &ctx.func.signature)
            .unwrap();
        module
            .define_function(id, &mut ctx, &mut codegen::binemit::NullTrapSink {})
            .unwrap();
        module.clear_context(&mut ctx);
        module.finalize_definitions();
        let code = module.get_finalized_function(id);
        let foo = unsafe { mem::transmute::<_, fn() -> isize>(code) };
        let num = foo();
        dbg!(num);
    }

    fn translate(&self, builder: &mut FunctionBuilder, int: Type) -> Value {
        match self {
            Expr::Number(num) => builder.ins().iconst(int, *num as i64),
            Expr::Op(op) => match op.as_ref() {
                Operation::Add(term1, term2) => {
                    let value1 = term1.translate(builder, int);
                    let value2 = term2.translate(builder, int);
                    builder.ins().iadd(value1, value2)
                }
                Operation::Sub(term1, term2) => {
                    let value1 = term1.translate(builder, int);
                    let value2 = term2.translate(builder, int);
                    builder.ins().isub(value1, value2)
                }
                Operation::Mul(term1, term2) => {
                    let value1 = term1.translate(builder, int);
                    let value2 = term2.translate(builder, int);
                    builder.ins().imul(value1, value2)
                }
                Operation::Div(term1, term2) => {
                    let value1 = term1.translate(builder, int);
                    let value2 = term2.translate(builder, int);
                    builder.ins().sdiv(value1, value2)
                }
            },
        }
    }
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

#[cfg(test)]
mod tests {
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
        let (a, b) = parse_expr(&list);
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
}
