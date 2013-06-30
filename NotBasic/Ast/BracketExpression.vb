
Public Class BracketExpression
    Inherits Expression

    Public Property Indexable As Expression
    Public Property Arguments As IEnumerable(Of Argument)

    Sub New(indexable As Expression, arguments As IEnumerable(Of Argument))
        Me.Indexable = indexable
        Me.Arguments = arguments
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
