
Class IfBlockStatement
    Inherits Statement

    Private _sourceSpan As Compilers.SourceSpan
    Private _sourceSpan1 As Compilers.SourceSpan
    Private _condition As Expression
    Private _truePart As IList(Of Statement)
    Private _elseIfBlocks As IList(Of ElseIfBlock)
    Private _elseBlockOpt As ElseBlock

    Sub New(sourceSpan As Compilers.SourceSpan, sourceSpan1 As Compilers.SourceSpan, condition As Expression, truePart As IEnumerable(Of Statement), elseIfBlocks As IEnumerable(Of ElseIfBlock), elseBlockOpt As ElseBlock)
        If truePart Is Nothing Then Exit Sub
        If elseIfBlocks Is Nothing Then Exit Sub

        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _sourceSpan1 = sourceSpan1
        _condition = condition
        _truePart = truePart.ToArray()
        _elseIfBlocks = elseIfBlocks.ToArray()
        _elseBlockOpt = elseBlockOpt
    End Sub

End Class
