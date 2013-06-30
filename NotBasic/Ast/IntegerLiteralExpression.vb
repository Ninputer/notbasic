Imports VBF.Compilers.Scanners

Public Class IntegerLiteralExpression
    Inherits Expression

    Public Property Value As LexemeValue

    Sub New(literal As LexemeValue)
        Value = literal
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
