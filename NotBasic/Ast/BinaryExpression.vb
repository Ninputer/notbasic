
Public Class BinaryExpression
    Inherits Expression

    Public Property Op As ExpressionOp
    Public Property Left As Expression
    Public Property Right As Expression

    Sub New(op As ExpressionOp, left As Expression, right As Expression)
        Me.Op = op
        Me.Left = left
        Me.Right = right
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
