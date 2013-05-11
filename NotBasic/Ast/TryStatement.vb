
Public Class TryStatement
    Inherits Statement

    Private _sourceSpan As Compilers.SourceSpan
    Private _sourceSpan1 As Compilers.SourceSpan
    Private _tryBody As IEnumerable(Of Statement)
    Private _typeCatches As IEnumerable(Of CatchBlock)
    Private _catchBlock As CatchBlock
    Private _finallyBlock As FinallyBlock

    Sub New(sourceSpan As Compilers.SourceSpan, sourceSpan1 As Compilers.SourceSpan, tryBody As IEnumerable(Of Statement), typeCatches As IEnumerable(Of CatchBlock), catchBlock As CatchBlock, finallyBlock As FinallyBlock)
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _sourceSpan1 = sourceSpan1
        _tryBody = tryBody
        _typeCatches = typeCatches
        _catchBlock = catchBlock
        _finallyBlock = finallyBlock
    End Sub

End Class
