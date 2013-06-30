
Public Class FinallyBlock
    Inherits SyntaxTreeData
    Public Property FinallyKeywordSpan As Compilers.SourceSpan
    Public Property FinallyBody As IEnumerable(Of Statement)

    Sub New(sourceSpan As Compilers.SourceSpan, finallyBody As IEnumerable(Of Statement))
        Me.FinallyKeywordSpan = sourceSpan
        Me.FinallyBody = finallyBody
    End Sub

End Class
