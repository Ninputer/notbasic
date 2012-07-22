
Class IfBlockStatement
    Inherits Statement

    Private _sourceSpan As Compilers.SourceSpan
    Private _sourceSpan1 As Compilers.SourceSpan
    Private _condition As Expression
    Private _truePart As IEnumerable(Of Statement)
    Private _elseIfBlocks As IEnumerable(Of ElseIfBlock)
    Private _elseBlockOpt As ElseBlock

    Sub New(sourceSpan As Compilers.SourceSpan, sourceSpan1 As Compilers.SourceSpan, condition As Expression, truePart As IEnumerable(Of Statement), elseIfBlocks As IEnumerable(Of ElseIfBlock), elseBlockOpt As ElseBlock)
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _sourceSpan1 = sourceSpan1
        _condition = condition
        _truePart = truePart
        _elseIfBlocks = elseIfBlocks
        _elseBlockOpt = elseBlockOpt
    End Sub

End Class
