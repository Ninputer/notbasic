Imports VBF.Compilers

Public Class UnaryExpression
    Inherits Expression

    Private m_op As ExpressionOp
    Private m_operand As Expression
    Public Sub New(opSpan As SourceSpan, op As ExpressionOp, operand As Expression)
        m_op = op
        m_operand = operand
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
