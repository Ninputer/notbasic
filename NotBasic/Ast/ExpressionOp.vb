Public Enum ExpressionOp
    Undefined = 0
    'unary
    [Not]
    Plus
    Minus

    'binary
    Multiplication
    Divition
    Modulo
    Addition
    Substraction
    
    ShiftLeft
    ShiftRight

    Less
    LessEqual
    Greater
    GreaterEqual
    Equal
    NotEqual

    [And]
    [Xor]
    [Or]

End Enum
