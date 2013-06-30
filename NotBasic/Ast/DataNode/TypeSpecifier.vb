
Public Class TypeSpecifier
    Inherits SyntaxTreeData
    Public Property Type As TypeName

    Sub New(spTypeName As TypeName)
        Me.Type = spTypeName
    End Sub

End Class
