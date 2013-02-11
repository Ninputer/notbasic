Imports VBF.Compilers

Class DoLoopStatement
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

End Class
