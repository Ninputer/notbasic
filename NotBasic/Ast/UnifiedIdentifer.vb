Imports VBF.Compilers.Scanners
Imports VBF.Compilers

Public Class UnifiedIdentifer

    Private m_identifier As LexemeValue
    Private m_escaped As Boolean

    Public Sub New(id As LexemeValue, escaped As Boolean)
        m_identifier = id
        m_escaped = escaped
    End Sub
    
End Class
