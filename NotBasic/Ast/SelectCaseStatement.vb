Imports VBF.Compilers

Public Class SelectCaseStatement
    Inherits Statement

    Private _sourceSpan As SourceSpan
    Private _sourceSpan1 As SourceSpan
    Private _selectExp As Expression
    Private _caseBlocks As IEnumerable(Of CaseBlock)
    Private _caseElse As CaseElseBlock

    Sub New(sourceSpan As SourceSpan, sourceSpan1 As SourceSpan, selectExp As Expression, caseBlocks As IEnumerable(Of CaseBlock), caseElse As CaseElseBlock)
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _sourceSpan1 = sourceSpan1
        _selectExp = selectExp
        _caseBlocks = caseBlocks
        _caseElse = caseElse
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
