Imports VBF.Compilers

Public Class NothingExpression
    Inherits Expression

    Private _sourceSpan As SourceSpan

    Sub New(sourceSpan As SourceSpan)
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
    End Sub

End Class
