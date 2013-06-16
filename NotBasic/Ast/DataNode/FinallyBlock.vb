
Public Class FinallyBlock
    Inherits SyntaxTreeData
    Private _sourceSpan As Compilers.SourceSpan
    Private _finallyBody As IEnumerable(Of Statement)

    Sub New(sourceSpan As Compilers.SourceSpan, finallyBody As IEnumerable(Of Statement))
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _finallyBody = finallyBody
    End Sub

End Class
