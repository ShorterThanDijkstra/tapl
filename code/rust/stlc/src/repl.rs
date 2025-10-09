use crate::parser::Parser;
use crate::syntax::Expr;
use crate::{eval::eval, parser};
use std::io::{self, Write};
// 假设你有 parse_expr(s: &str) -> Result<Expr, String>
fn parse_expr(s: &str) -> Result<Expr, String> {
    let from_str = Parser::from_str(s);

    if let Ok(mut parser) = from_str {
        if let Ok(expr) = parser.parse_expr() {
            Ok(expr.expr.clone())
        } else {
            Err(format!("解析错误"))
        }
    } else {
        Err(format!("解析错误"))
    }
}

pub fn repl() {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        if io::stdin().read_line(&mut input).is_err() {
            println!("读取失败");
            continue;
        }
        let input = input.trim();
        if input.is_empty() {
            continue;
        }
        match parse_expr(input) {
            Ok(expr) => {
                let result = eval(expr.clone());
                println!("=> {}", result);
            }
            Err(e) => println!("解析错误: {}", e),
        }
    }
}
