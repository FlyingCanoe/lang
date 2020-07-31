use parser::parser;

mod compile;
mod lexer;
mod parser;

fn main() {
    let ast = parser(
        "let x = 10 * 10 \n\
               x * 2",
    )
    .unwrap();
    dbg!(ast.run());
}
