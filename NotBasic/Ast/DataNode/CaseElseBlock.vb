Imports VBF.Compilers

Public Class CaseElseBlock
    Inherits SyntaxTreeData
    Public Property CaseKeywordSpan As Compilers.SourceSpan
    Public Property ElseKeywordSpan As Compilers.SourceSpan
    Public Property CaseBody As IEnumerable(Of Statement)

    Sub New(sourceSpan As Compilers.SourceSpan, sourceSpan1 As Compilers.SourceSpan, caseBody As IEnumerable(Of Statement))
        Me.CaseKeywordSpan = sourceSpan
        Me.ElseKeywordSpan = sourceSpan1
        Me.CaseBody = caseBody
    End Sub

    Public ReadOnly Property CaseSpan As SourceSpan
        Get
            Return CaseKeywordSpan
        End Get
    End Property

End Class
