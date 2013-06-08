
Class BracketExpression
    Inherits Expression

    Private _indexable As Object
    Private _arguments As IEnumerable(Of Argument)

    Sub New(indexable As Object, arguments As IEnumerable(Of Argument))
        ' TODO: Complete member initialization 
        _indexable = indexable
        _arguments = arguments
    End Sub

End Class
