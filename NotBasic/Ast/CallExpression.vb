
Class CallExpression
    Inherits Expression

    Private _callable As Object
    Private _arguments As ArgumentList

    Sub New(callable As Object, arguments As ArgumentList)
        ' TODO: Complete member initialization 
        _callable = callable
        _arguments = arguments
    End Sub

End Class
