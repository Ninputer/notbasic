
Public Class ArrayLiteralExpression
    Inherits Expression

    Private _expList As IEnumerable(Of Expression)

    Sub New(expList As IEnumerable(Of Expression))
        ' TODO: Complete member initialization 
        _expList = expList
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
