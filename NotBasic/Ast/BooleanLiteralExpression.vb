Imports VBF.Compilers.Scanners

Public Class BooleanLiteralExpression
    Inherits Expression

    Private m_value As LexemeValue

    Sub New(value As LexemeValue)
        m_value = value
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
