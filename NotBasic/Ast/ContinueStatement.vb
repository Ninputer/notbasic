Imports VBF.Compilers.Scanners
Imports VBF.Compilers

Public Class ContinueStatement
    Inherits Statement

    Private m_keywordSpan As SourceSpan
    Private m_loopType As LexemeValue 'do, for

    Sub New(keywordSpan As SourceSpan, loopType As LexemeValue)
        m_keywordSpan = keywordSpan
        m_loopType = loopType
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
