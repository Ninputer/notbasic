Public Class SyntaxTreeNode

    Public Lexemes As IEnumerable(Of Compilers.Scanners.Lexeme)

    Sub New()

    End Sub

    Sub New(c As IEnumerable(Of Compilers.Scanners.Lexeme))
        Lexemes = c
    End Sub

End Class
