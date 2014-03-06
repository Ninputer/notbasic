Imports VBF.Compilers.Scanners
Imports VBF.Compilers.Parsers

Public Class KeywordManager
    Private m_lexer As Lexer
    Private m_reservedKeywords As List(Of Token)

    Public Sub New(lexer As Lexer)
        m_lexer = lexer
        m_reservedKeywords = New List(Of Token)
    End Sub

    Public Function CreateReservedKeywordsProduction() As ProductionBase(Of Lexeme)
        Return Grammar.Union(m_reservedKeywords.ToArray())
    End Function

    Public Function DefineKeyword(keyword As String) As Token
        Dim keywordLiteral = RegularExpression.Literal(keyword)
        Dim token = m_lexer.DefineToken(keywordLiteral, keyword)

        m_reservedKeywords.Add(token)

        Return token
    End Function
End Class
