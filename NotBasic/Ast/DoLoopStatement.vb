Imports VBF.Compilers

Public Class DoLoopStatement
    Inherits Statement

    Shared Function DoLoopFrom(doKeywordSpan As SourceSpan, loopKeywordSpan As SourceSpan, loopBody As IEnumerable(Of Statement)) As Statement
        Return New DoLoopStatement
    End Function

    Shared Function DoWhileLoopFrom(doKeywordSpan As SourceSpan, whileKeywordSpan As SourceSpan, loopKeywordSpan As SourceSpan, condition As Expression, loopBody As IEnumerable(Of Statement)) As Statement
        Return New DoLoopStatement
    End Function

    Shared Function DoUntilLoopFrom(doKeywordSpan As SourceSpan, untilKeywordSpan As SourceSpan, loopKeywordSpan As SourceSpan, condition As Expression, loopBody As IEnumerable(Of Statement)) As Statement
        Return New DoLoopStatement
    End Function

    Shared Function DoLoopWhileFrom(doKeywordSpan As SourceSpan, whileKeywordSpan As SourceSpan, loopKeywordSpan As SourceSpan, condition As Expression, loopBody As IEnumerable(Of Statement)) As Statement
        Return New DoLoopStatement
    End Function

    Shared Function DoLoopUntilFrom(doKeywordSpan As SourceSpan, untilKeywordSpan As SourceSpan, loopKeywordSpan As SourceSpan, condition As Expression, loopBody As IEnumerable(Of Statement)) As Statement
        Return New DoLoopStatement
    End Function

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
