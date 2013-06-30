
Public Class ConceptConstraintClause
    Inherits ConstraintClause

    Public Property ConceptName As UnifiedIdentifer
    Public Property TypeArguments As IEnumerable(Of TypeName)

    Sub New(conceptName As UnifiedIdentifer, typeArgs As IEnumerable(Of TypeName))
        Me.ConceptName = conceptName
        Me.TypeArguments = typeArgs
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
