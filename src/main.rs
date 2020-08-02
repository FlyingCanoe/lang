#![feature(option_unwrap_none, try_blocks)]
use std::process::exit;

extern crate pest;
#[macro_use]
extern crate pest_derive;

use parser::parser;

mod compile;
mod parser;

fn main() {
    let ast = match parser("let x = 6\nx +6") {
        Ok(ast) => ast,
        Err(error) => {
            println!("{}", error);
            exit(-1);
        }
    };
    dbg!(ast.run());
}
