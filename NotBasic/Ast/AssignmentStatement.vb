
Public Class AssignmentStatement
    Inherits Statement

    Private m_identifier As UnifiedIdentifer
    Private m_value As Expression

    Sub New(identifier As UnifiedIdentifer, value As Expression)
        m_identifier = identifier
        m_value = value
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
