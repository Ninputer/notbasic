Imports VBF.Compilers

Public Class SelectCaseStatement
    Inherits Statement

    Public Property SelectKeywordSpan As SourceSpan
    Public Property EndKeywordSpan As SourceSpan
    Public Property SelectExpression As Expression
    Public Property CaseBlocks As IEnumerable(Of CaseBlock)
    Public Property CaseElseBlock As CaseElseBlock

    Sub New(selectSpan As SourceSpan, endSpan As SourceSpan, selectExp As Expression, caseBlocks As IEnumerable(Of CaseBlock), caseElse As CaseElseBlock)
        Me.SelectKeywordSpan = selectSpan
        Me.EndKeywordSpan = endSpan
        Me.SelectExpression = selectExp
        Me.CaseBlocks = caseBlocks
        Me.CaseElseBlock = caseElse
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
