
Class BinaryExpression
    Inherits Expression

    Private _expressionOp As ExpressionOp
    Private _left As Expression
    Private _right As Expression

    Sub New(expressionOp As ExpressionOp, left As Expression, right As Expression)
        ' TODO: Complete member initialization 
        _expressionOp = expressionOp
        _left = left
        _right = right
    End Sub

End Class
