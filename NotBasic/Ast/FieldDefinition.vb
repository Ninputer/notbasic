
Public Class FieldDefinition
    Inherits SyntaxTreeNode
    Public Property Name As UnifiedIdentifer
    Public Property TypeSpecifier As TypeSpecifier

    Sub New(fieldName As UnifiedIdentifer, typeSp As TypeSpecifier)
        Me.Name = fieldName
        Me.TypeSpecifier = typeSp
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
