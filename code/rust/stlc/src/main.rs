mod eval;
mod lexer;
mod parser;
mod repl;
mod syntax;
mod eval_de_bruijn;
mod repl_de_bruijn;
mod type_check;
use crate::parser::Parser as IceParser;
use crate::eval::eval_program;
use crate::type_check::check_program;
use clap::Parser;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
    #[arg(value_name = "FILE")]
    file: PathBuf,
}
fn eval_file() {
    let args = Args::parse();
    let program = IceParser::from_file(&args.file).unwrap().parse_program().unwrap().program;
    let check = check_program(&program);
    if !check.is_empty() {
        for e in check {
            eprintln!("{}", e);
        }
        return;
    }
    let res = eval_program(program); 
    match res {
        Ok(v) => println!("{}", v),
        Err(e) => eprintln!("{}", e),
    };
}
fn debruijn_eval_file() {
    let args = Args::parse();
    let program = IceParser::from_file(&args.file).unwrap().parse_program().unwrap().program;
    let res = eval_de_bruijn::eval_program(program); 
    match res {
        Ok(v) => println!("{}", v),
        Err(e) => eprintln!("{}", e),
    };
}
fn main() {
    // repl::repl();
    // repl_de_bruijn::repl();
    eval_file();
    // debruijn_eval_file();
}
