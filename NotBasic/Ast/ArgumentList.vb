Imports System.Collections.ObjectModel

Class ArgumentList
    Inherits Collection(Of Argument)

    Public Sub New(arguments As IEnumerable(Of Argument))
        MyBase.New(arguments.ToArray())
    End Sub
End Class
