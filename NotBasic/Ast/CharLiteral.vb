
Public Class CharLiteral
    Inherits Expression

    Private _cl As Compilers.Scanners.Lexeme

    Sub New(cl As Compilers.Scanners.Lexeme)
        ' TODO: Complete member initialization 
        _cl = cl
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
