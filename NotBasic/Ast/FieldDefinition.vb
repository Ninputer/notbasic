
Public Class FieldDefinition
    Inherits SyntaxTreeNode
    Private _fieldName As UnifiedIdentifer
    Private _typeSp As TypeSpecifier

    Sub New(fieldName As UnifiedIdentifer, typeSp As TypeSpecifier)
        ' TODO: Complete member initialization 
        _fieldName = fieldName
        _typeSp = typeSp
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
