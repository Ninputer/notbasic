
Public Class TypeParameter
    Inherits SyntaxTreeData
    Private _name As UnifiedIdentifer
    Private _dimension As Nullable(Of Integer)

    Sub New(name As UnifiedIdentifer, dimension As Nullable(Of Integer))
        ' TODO: Complete member initialization 
        _name = name
        _dimension = dimension
    End Sub

End Class
