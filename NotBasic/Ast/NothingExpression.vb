Imports VBF.Compilers

Public Class NothingExpression
    Inherits Expression

    Public Property NothingKeywordSpan As SourceSpan

    Sub New(nothingSpan As SourceSpan)
        NothingKeywordSpan = nothingSpan
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
