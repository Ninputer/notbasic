
Public Class TypeParameter
    Inherits SyntaxTreeData
    Public Property Name As UnifiedIdentifer
    Public Property Dimension As Nullable(Of Integer)

    Sub New(name As UnifiedIdentifer, dimension As Nullable(Of Integer))
        Me.Name = name
        Me.Dimension = dimension
    End Sub

End Class
