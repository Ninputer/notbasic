Imports VBF.Compilers.Scanners
Imports VBF.Compilers

Public Class UnifiedIdentifer

    Private m_identifier As String
    Private m_span As SourceSpan

    Private Sub New(value As String, span As SourceSpan)
        m_identifier = value
        m_span = span
    End Sub

    Shared Function FromIdentifier(id As Lexeme) As UnifiedIdentifer
        Return New UnifiedIdentifer(id.Value, id.Span)
    End Function

    Shared Function FromEscapedIdentifier(eid As Lexeme) As UnifiedIdentifer
        Return New UnifiedIdentifer(eid.Value.Substring(1), eid.Span)
    End Function

End Class
