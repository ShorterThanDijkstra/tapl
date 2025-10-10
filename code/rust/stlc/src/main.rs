mod eval;
mod lexer;
mod parser;
mod repl;
mod syntax;
mod eval_de_bruijn;
mod repl_de_bruijn;
fn main() {
    // repl::repl();
    repl_de_bruijn::repl();
}
