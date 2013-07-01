Imports VBF.Compilers

Public Enum LoopType
    None
    WhileCondition
    UntilCondition
End Enum

Public Class DoLoopStatement
    Inherits Statement

    Public Property ConditionAfterDo As Expression
    Public Property ConditionAfterLoop As Expression
    Public Property DoKeywordSpan As SourceSpan
    Public Property LoopKeywordSpan As SourceSpan
    Public Property LoopType As LoopType
    Public Property LoopBody As IEnumerable(Of Statement)

    Shared Function DoLoopFrom(doKeywordSpan As SourceSpan, loopKeywordSpan As SourceSpan, loopBody As IEnumerable(Of Statement)) As Statement
        Return New DoLoopStatement With
        {
            .LoopType = LoopType.None,
            .DoKeywordSpan = doKeywordSpan,
            .LoopKeywordSpan = loopKeywordSpan,
            .LoopBody = loopBody
        }
    End Function

    Shared Function DoWhileLoopFrom(doKeywordSpan As SourceSpan, whileKeywordSpan As SourceSpan, loopKeywordSpan As SourceSpan, condition As Expression, loopBody As IEnumerable(Of Statement)) As Statement
        Return New DoLoopStatement With
        {
            .LoopType = LoopType.WhileCondition,
            .ConditionAfterDo = condition,
            .DoKeywordSpan = doKeywordSpan,
            .LoopKeywordSpan = loopKeywordSpan,
            .LoopBody = loopBody
        }
    End Function

    Shared Function DoUntilLoopFrom(doKeywordSpan As SourceSpan, untilKeywordSpan As SourceSpan, loopKeywordSpan As SourceSpan, condition As Expression, loopBody As IEnumerable(Of Statement)) As Statement
        Return New DoLoopStatement With
        {
            .LoopType = Compiler.LoopType.UntilCondition,
            .ConditionAfterDo = condition,
            .DoKeywordSpan = doKeywordSpan,
            .LoopKeywordSpan = loopKeywordSpan,
            .LoopBody = loopBody
        }
    End Function

    Shared Function DoLoopWhileFrom(doKeywordSpan As SourceSpan, whileKeywordSpan As SourceSpan, loopKeywordSpan As SourceSpan, condition As Expression, loopBody As IEnumerable(Of Statement)) As Statement
        Return New DoLoopStatement With
        {
            .LoopType = LoopType.WhileCondition,
            .ConditionAfterLoop = condition,
            .DoKeywordSpan = doKeywordSpan,
            .LoopKeywordSpan = loopKeywordSpan,
            .LoopBody = loopBody
        }
    End Function

    Shared Function DoLoopUntilFrom(doKeywordSpan As SourceSpan, untilKeywordSpan As SourceSpan, loopKeywordSpan As SourceSpan, condition As Expression, loopBody As IEnumerable(Of Statement)) As Statement
        Return New DoLoopStatement With
        {
            .LoopType = Compiler.LoopType.UntilCondition,
            .ConditionAfterLoop = condition,
            .DoKeywordSpan = doKeywordSpan,
            .LoopKeywordSpan = loopKeywordSpan,
            .LoopBody = loopBody
        }
    End Function

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
