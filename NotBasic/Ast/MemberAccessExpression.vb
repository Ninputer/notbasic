
Class MemberAccessExpression
    Inherits Expression

    Private _exp As Expression
    Private _memberName As UnifiedIdentifer
    Private _typeArgs As IEnumerable(Of TypeName)

    Sub New(exp As Expression, memberName As UnifiedIdentifer, typeArgs As IEnumerable(Of TypeName))
        ' TODO: Complete member initialization 
        _exp = exp
        _memberName = memberName
        _typeArgs = typeArgs
    End Sub

End Class
