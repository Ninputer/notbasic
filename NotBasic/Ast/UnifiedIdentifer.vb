Imports VBF.Compilers.Scanners
Imports VBF.Compilers

Public Class UnifiedIdentifer

    Private m_identifier As LexemeValue
    Private m_escaped As Boolean

    Public ReadOnly Property Identifier As String
        Get
            Return m_identifier.Content
        End Get
    End Property

    Public Sub New(id As LexemeValue, escaped As Boolean)
        m_identifier = id
        m_escaped = escaped
    End Sub
    
End Class
