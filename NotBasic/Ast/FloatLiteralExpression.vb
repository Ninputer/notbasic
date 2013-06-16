
Public Class FloatLiteralExpression
    Inherits Expression

    Private _literal As Compilers.Scanners.Lexeme

    Sub New(literal As Compilers.Scanners.Lexeme)
        ' TODO: Complete member initialization 
        _literal = literal
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
