
Public Class ElseIfBlock
    Inherits SyntaxTreeData
    Private _sourceSpan As Compilers.SourceSpan
    Private _condition As Expression
    Private _elseIfTruePart As IEnumerable(Of Statement)

    Sub New(sourceSpan As Compilers.SourceSpan, condition As Expression, elseIfTruePart As IEnumerable(Of Statement))
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _condition = condition
        _elseIfTruePart = elseIfTruePart
    End Sub

End Class
