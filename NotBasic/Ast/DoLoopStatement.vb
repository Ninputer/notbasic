Imports VBF.Compilers

Public Class DoLoopStatement
    Inherits Statement

    Shared Function DoLoopFrom(sourceSpan As SourceSpan, sourceSpan1 As SourceSpan, loopBody As IEnumerable(Of Statement)) As Statement
        Return New DoLoopStatement
    End Function

    Shared Function DoWhileLoopFrom(sourceSpan As SourceSpan, sourceSpan1 As SourceSpan, sourceSpan2 As SourceSpan, condition As Expression, loopBody As IEnumerable(Of Statement)) As Statement
        Return New DoLoopStatement
    End Function

    Shared Function DoUntilLoopFrom(sourceSpan As SourceSpan, sourceSpan1 As SourceSpan, sourceSpan2 As SourceSpan, condition As Expression, loopBody As IEnumerable(Of Statement)) As Statement
        Return New DoLoopStatement
    End Function

    Shared Function DoLoopWhileFrom(sourceSpan As SourceSpan, sourceSpan1 As SourceSpan, sourceSpan2 As SourceSpan, condition As Expression, loopBody As IEnumerable(Of Statement)) As Statement
        Return New DoLoopStatement
    End Function

    Shared Function DoLoopUntilFrom(sourceSpan As SourceSpan, sourceSpan1 As SourceSpan, sourceSpan2 As SourceSpan, condition As Expression, loopBody As IEnumerable(Of Statement)) As Statement
        Return New DoLoopStatement
    End Function

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
