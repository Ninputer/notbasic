
Public Class QualifiedTypeName
    Inherits TypeName

    Private _id As UnifiedIdentifer
    Private _typeArgs As IEnumerable(Of TypeName)

    Sub New(id As UnifiedIdentifer)
        ' TODO: Complete member initialization 
        _id = id
    End Sub

    Sub New(id As UnifiedIdentifer, typeArgs As IEnumerable(Of TypeName))
        ' TODO: Complete member initialization 
        _id = id
        _typeArgs = typeArgs
    End Sub

End Class
