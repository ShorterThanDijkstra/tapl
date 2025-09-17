mod lexer;
mod parser;
mod syntax;
// use lexer::Lexer;
// use parser::Parser;
// fn main() {
//     let input = r#"
//         foo : Int -> String
//         foo i = show i

//         bar : String -> Bool
//         bar s = isEmpty s
//     "#;

//     let lexer = Lexer::new(input);
//     let mut parser = Parser::new(lexer);

//     match parser.parse_program() {
//         Ok(declarations) => {
//             println!("Successfully parsed {} declarations:", declarations.len());
//             for (i, decl) in declarations.iter().enumerate() {
//                 println!("{}. {:?}", i + 1, decl);
//             }
//         }
//         Err(e) => {
//             println!("Parse error: {}", e);
//         }
//     }
// }
//
fn main() {
    let some_value: Option<i32> = None;
    
    // 这里模式匹配会失败，因为没有 Some(value)
    if let Some(x) = some_value {
        println!("成功绑定值: {}", x); // 这行不会执行
    }
    
    println!("程序继续正常执行"); // 这行会执行
}
