Public Class ExitStatement
    Inherits Statement

    Public Property ExitKeywordSpan As Compilers.SourceSpan
    Public Property ExitScopeName As Compilers.Scanners.LexemeValue

    Sub New(sourceSpan As Compilers.SourceSpan, lexemeValue As Compilers.Scanners.LexemeValue)
        ExitKeywordSpan = sourceSpan
        ExitScopeName = lexemeValue
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
