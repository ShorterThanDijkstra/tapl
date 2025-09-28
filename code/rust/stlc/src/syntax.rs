use std::fmt;

/*
 * Program := Dec Dec*
 * Dec := TypeDec '\n' FuncDec '\n'
 * TypeDec := Identifier ':' Type
 * FuncDec := Identifier '=' Expr
 * Type := Atom | '(' Type '->' Type ')'
 * Expr := Var | Application | 'λ' Identifier '=>' Expr | (Expr) | Expr Expr
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub decs: Vec<Dec>,
    pub main: Dec,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Dec {
    pub name: String,
    pub ty_dec: TypeDec,
    pub func_dec: FuncDec,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDec {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDec {
    pub name: String,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Atom(String),
    Function { input: Box<Type>, output: Box<Type> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(String),
    Bool(bool),
    Application { func: Box<Expr>, arg: Box<Expr> },
    Lambda { param: String, body: Box<Expr> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct CoordProgram {
    pub program: Program,
    pub row_start: usize,
    pub col_start: usize,
    pub row_end: usize,
    pub col_end: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CoordDec {
    pub dec: Dec,
    pub row_start: usize,
    pub col_start: usize,
    pub row_end: usize,
    pub col_end: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CoordTypeDec {
    pub type_dec: TypeDec,
    pub row_start: usize,
    pub col_start: usize,
    pub row_end: usize,
    pub col_end: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CoordFuncDec {
    pub func_dec: FuncDec,
    pub row_start: usize,
    pub col_start: usize,
    pub row_end: usize,
    pub col_end: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CoordType {
    pub ty: Type,
    pub row_start: usize,
    pub col_start: usize,
    pub row_end: usize,
    pub col_end: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CoordExpr {
    pub expr: Expr,
    pub row_start: usize,
    pub col_start: usize,
    pub row_end: usize,
    pub col_end: usize,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for dec in &self.decs {
            writeln!(f, "{dec}")?;
        }
        writeln!(f, "main: {}", self.main)
    }
}

impl fmt::Display for Dec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.ty_dec)?;
        writeln!(f, "{}", self.func_dec)
    }
}

impl fmt::Display for TypeDec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{} : {})", self.name, self.ty)
    }
}

impl fmt::Display for FuncDec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {})", self.name, self.body)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Atom(s) => write!(f, "{s}"),
            Type::Function { input, output } => write!(f, "({input} -> {output})"),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Var(s) => write!(f, "{s}"),
            Expr::Bool(b) => write!(f, "{b}"),
            Expr::Application { func, arg } => write!(f, "({func} {arg})"),
            Expr::Lambda { param, body } => write!(f, "(λ{param} => {body})"),
        }
    }
}

impl fmt::Display for CoordProgram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} [{}:{}-{}:{}]",
            self.program, self.row_start, self.col_start, self.row_end, self.col_end
        )
    }
}

impl fmt::Display for CoordDec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} [{}:{}-{}:{}]",
            self.dec, self.row_start, self.col_start, self.row_end, self.col_end
        )
    }
}

impl fmt::Display for CoordTypeDec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} [{}:{}-{}:{}]",
            self.type_dec, self.row_start, self.col_start, self.row_end, self.col_end
        )
    }
}

impl fmt::Display for CoordFuncDec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} [{}:{}-{}:{}]",
            self.func_dec, self.row_start, self.col_start, self.row_end, self.col_end
        )
    }
}

impl fmt::Display for CoordType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} [{}:{}-{}:{}]",
            self.ty, self.row_start, self.col_start, self.row_end, self.col_end
        )
    }
}

impl fmt::Display for CoordExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} [{}:{}-{}:{}]",
            self.expr, self.row_start, self.col_start, self.row_end, self.col_end
        )
    }
}

