
Public Class ParameterDeclaration

    Private _did As UnifiedIdentifer
    Private _typesp As TypeSpecifier
    Private _parameterPrefix As ParameterPrefix

    Sub New(did As UnifiedIdentifer)
        ' TODO: Complete member initialization 
        _did = did
    End Sub

    Sub New(did As UnifiedIdentifer, typesp As TypeSpecifier)
        ' TODO: Complete member initialization 
        _did = did
        _typesp = typesp
    End Sub

    Sub New(did As UnifiedIdentifer, typesp As TypeSpecifier, parameterPrefix As ParameterPrefix)
        ' TODO: Complete member initialization 
        _did = did
        _typesp = typesp
        _parameterPrefix = parameterPrefix
    End Sub

End Class
