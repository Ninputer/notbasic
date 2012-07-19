Imports VBF.Compilers.Scanners

Public Class KeywordManager
    Private m_lexer As Lexer
    Private m_reservedKeywords As RegularExpression

    Public Sub New(lexer As Lexer)
        m_lexer = lexer
    End Sub

    Public ReadOnly Property ReservedKeywords As RegularExpression
        Get
            Return m_reservedKeywords
        End Get
    End Property

    Public Function DefineKeyword(keyword As String) As Token
        Dim keywordLiteral = RegularExpression.Literal(keyword)
        Dim token = m_lexer.DefineToken(keywordLiteral, keyword)

        If m_reservedKeywords Is Nothing Then
            m_reservedKeywords = keywordLiteral
        Else
            m_reservedKeywords = m_reservedKeywords Or keywordLiteral
        End If

        Return token
    End Function
End Class
