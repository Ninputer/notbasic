
Public Class AssignmentStatement
    Inherits Statement

    Public Property Variable As UnifiedIdentifer
    Public Property Value As Expression

    Sub New(identifier As UnifiedIdentifer, value As Expression)
        Me.Variable = identifier
        Me.Value = value
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
