
Class CallExpression
    Inherits Expression

    Private _callable As Object
    Private _arguments As IEnumerable(Of Argument)

    Sub New(callable As Object, arguments As IEnumerable(Of Argument))
        ' TODO: Complete member initialization 
        _callable = callable
        _arguments = arguments
    End Sub

End Class
