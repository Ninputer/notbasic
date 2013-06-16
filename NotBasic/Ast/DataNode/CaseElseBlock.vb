Imports VBF.Compilers

Public Class CaseElseBlock
    Inherits SyntaxTreeData
    Private _sourceSpan As Compilers.SourceSpan
    Private _sourceSpan1 As Compilers.SourceSpan
    Private _caseBody As IEnumerable(Of Statement)

    Sub New(sourceSpan As Compilers.SourceSpan, sourceSpan1 As Compilers.SourceSpan, caseBody As IEnumerable(Of Statement))
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _sourceSpan1 = sourceSpan1
        _caseBody = caseBody
    End Sub

    Public ReadOnly Property CaseSpan As SourceSpan
        Get
            Return _sourceSpan
        End Get
    End Property

End Class
