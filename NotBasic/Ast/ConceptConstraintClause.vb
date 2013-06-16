
Public Class ConceptConstraintClause
    Inherits ConstraintClause

    Private _conceptName As UnifiedIdentifer
    Private _types As IEnumerable(Of TypeName)

    Sub New(conceptName As UnifiedIdentifer, types As IEnumerable(Of TypeName))
        ' TODO: Complete member initialization 
        _conceptName = conceptName
        _types = types
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
