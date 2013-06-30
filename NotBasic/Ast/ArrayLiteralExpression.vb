
Public Class ArrayLiteralExpression
    Inherits Expression

    Public Property Values As IEnumerable(Of Expression)

    Sub New(valueList As IEnumerable(Of Expression))
        Me.Values = valueList
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
