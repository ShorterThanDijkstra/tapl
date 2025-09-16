#[derive(Debug, Clone, PartialEq)]
pub struct Porgram {
    pub decs : Vec<Dec>
}

#[derive(Debug, Clone, PartialEq)]
pub struct Dec {
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
    Primary(String),
    Function {
        input: Box<Type>,
        output: Box<Type>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Identifier(String),
    Application {
        func: Box<Expr>,
        arg: Box<Expr>,
    },
    Lambda {
        param: String,
        body: Box<Expr>,
    },
}
