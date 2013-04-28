
Public Class QualifiedTypeName
    Inherits TypeName

    Private _id As UnifiedIdentifer
    Private _qualifier As QualifiedTypeName

    Sub New(id As UnifiedIdentifer)
        ' TODO: Complete member initialization 
        _id = id
    End Sub

    Sub New(qualifier As QualifiedTypeName, id As UnifiedIdentifer)
        ' TODO: Complete member initialization 
        _qualifier = qualifier
        _id = id
    End Sub

End Class
