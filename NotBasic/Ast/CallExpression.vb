
Public Class CallExpression
    Inherits Expression

    Public Property Callable As Expression
    Public Property Arguments As IEnumerable(Of Argument)

    Sub New(obj As Expression, arguments As IEnumerable(Of Argument))
        Me.Callable = obj
        Me.Arguments = arguments
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
