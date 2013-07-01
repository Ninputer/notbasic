
Public Class ElseBlock
    Inherits SyntaxTreeData
    Public Property ElseKeywordSpan As Compilers.SourceSpan
    Public Property ElsePart As IEnumerable(Of Statement)

    Sub New(sourceSpan As Compilers.SourceSpan, elsePart As IEnumerable(Of Statement))
        Me.ElseKeywordSpan = sourceSpan
        Me.ElsePart = elsePart
    End Sub

End Class
