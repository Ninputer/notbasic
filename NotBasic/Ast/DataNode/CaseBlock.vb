Imports VBF.Compilers

Public Class CaseBlock
    Inherits SyntaxTreeData
    Public Property CaseKeywordSpan As SourceSpan
    Public Property ConditionExpressions As IEnumerable(Of Expression)
    Public Property CaseBody As IEnumerable(Of Statement)

    Sub New(sourceSpan As SourceSpan, expList As IEnumerable(Of Expression), caseBody As IEnumerable(Of Statement))
        Me.CaseKeywordSpan = sourceSpan
        Me.ConditionExpressions = expList
        Me.CaseBody = caseBody
    End Sub

    Public ReadOnly Property CaseSpan As SourceSpan
        Get
            Return CaseKeywordSpan
        End Get
    End Property
End Class
