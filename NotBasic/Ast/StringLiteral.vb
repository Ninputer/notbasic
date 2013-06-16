

Public Class StringLiteral
    Inherits Expression

    Private _str As Compilers.Scanners.Lexeme

    Sub New(str As Compilers.Scanners.Lexeme)
        ' TODO: Complete member initialization 
        _str = str
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
