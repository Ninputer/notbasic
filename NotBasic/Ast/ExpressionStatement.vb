
Public Class ExpressionStatement
    Inherits Statement

    Private _exp As Expression

    Sub New(exp As Expression)
        ' TODO: Complete member initialization 
        _exp = exp
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
