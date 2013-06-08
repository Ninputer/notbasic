
Public Class ArrayLiteralExpression
    Inherits Expression

    Private _expList As IEnumerable(Of Expression)

    Sub New(expList As IEnumerable(Of Expression))
        ' TODO: Complete member initialization 
        _expList = expList
    End Sub

End Class
