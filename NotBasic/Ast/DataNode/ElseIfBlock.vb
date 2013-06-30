
Public Class ElseIfBlock
    Inherits SyntaxTreeData
    Public Property ElseIfKeywordSpan As Compilers.SourceSpan
    Public Property Condition As Expression
    Public Property TruePart As IEnumerable(Of Statement)

    Sub New(sourceSpan As Compilers.SourceSpan, condition As Expression, elseIfTruePart As IEnumerable(Of Statement))
        Me.ElseIfKeywordSpan = sourceSpan
        Me.Condition = condition
        Me.TruePart = elseIfTruePart
    End Sub

End Class
