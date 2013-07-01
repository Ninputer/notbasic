Imports VBF.Compilers.Scanners
Imports VBF.Compilers

Public Class ContinueStatement
    Inherits Statement

    Public Property ContinueKeywordSpan As SourceSpan
    Public Property ContinueScopeName As LexemeValue 'do, for

    Sub New(keywordSpan As SourceSpan, loopType As LexemeValue)
        Me.ContinueKeywordSpan = keywordSpan
        Me.ContinueScopeName = loopType
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
