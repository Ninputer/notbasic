Imports VBF.Compilers

Public Class CaseBlock
    Inherits SyntaxTreeNode
    Private _sourceSpan As SourceSpan
    Private _expList As IEnumerable(Of Expression)
    Private _caseBody As IEnumerable(Of Statement)

    Sub New(sourceSpan As SourceSpan, expList As IEnumerable(Of Expression), caseBody As IEnumerable(Of Statement))
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _expList = expList
        _caseBody = caseBody
    End Sub

    Public ReadOnly Property CaseSpan As SourceSpan
        Get
            Return _sourceSpan
        End Get
    End Property
End Class
