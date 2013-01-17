
Class NewArrayExpression
    Inherits Expression

    Private _sourceSpan As Compilers.SourceSpan
    Private _sourceSpan1 As Compilers.SourceSpan
    Private _sourceSpan2 As Compilers.SourceSpan
    Private _type As TypeName
    Private _length As Expression

    Sub New(sourceSpan As Compilers.SourceSpan, sourceSpan1 As Compilers.SourceSpan, sourceSpan2 As Compilers.SourceSpan, type As TypeName, length As Expression)
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _sourceSpan1 = sourceSpan1
        _sourceSpan2 = sourceSpan2
        _type = type
        _length = length
    End Sub

End Class
