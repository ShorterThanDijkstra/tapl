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

fn main() {

}