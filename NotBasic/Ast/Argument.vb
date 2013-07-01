
Public Class Argument
    Inherits SyntaxTreeNode
    Public Property ArgumentExpression As Expression

    Sub New(argexp As Expression)
        Me.ArgumentExpression = argexp
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
