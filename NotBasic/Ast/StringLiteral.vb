

Public Class StringLiteral
    Inherits Expression

    Public Property Value As Compilers.Scanners.Lexeme

    Sub New(str As Compilers.Scanners.Lexeme)
        Value = str
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
