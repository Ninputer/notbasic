
Public Class BinaryExpression
    Inherits Expression

    Private m_op As ExpressionOp
    Private m_left As Expression
    Private m_right As Expression

    Sub New(op As ExpressionOp, left As Expression, right As Expression)
        m_op = op
        m_left = left
        m_right = right
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
