

Public Class StringLiteral
    Inherits Expression

    Private _str As Compilers.Scanners.Lexeme

    Sub New(str As Compilers.Scanners.Lexeme)
        ' TODO: Complete member initialization 
        _str = str
    End Sub

End Class
