Imports VBF.Compilers.Scanners

Public Class CharLiteral
    Inherits Expression

    Public Property Value As LexemeValue

    Sub New(value As LexemeValue)
        Me.Value = value
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
