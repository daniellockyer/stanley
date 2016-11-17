pub enum Expression {
    BinaryExpression(BinaryExpressionData),
    UnaryExpression(UnaryExpressionData),
    BooleanLiteral(bool),
}

pub struct BinaryExpressionData {
    pub left: Box<Expression>,
    pub op: BinaryOperator,
    pub right: Box<Expression>
}

pub struct UnaryExpressionData {
    pub op: UnaryOperator,
    pub e: Box<Expression>
}

pub enum UnaryOperator {
    Negation,
    BitwiseNot,
    Not
}

pub enum BinaryOperator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulo
}

pub enum Types {
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64
}