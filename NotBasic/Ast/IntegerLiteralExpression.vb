Imports VBF.Compilers.Scanners

Public Class IntegerLiteralExpression
    Inherits Expression

    Private _literal As LexemeValue

    Sub New(literal As LexemeValue)
        ' TODO: Complete member initialization 
        _literal = literal
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
