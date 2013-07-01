
Public Class ExpressionStatement
    Inherits Statement

    Public Property Expression As Expression

    Sub New(exp As Expression)
        Me.Expression = exp
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
