
Public Class ConceptConstraintClause
    Inherits ConstraintClause

    Private m_conceptName As UnifiedIdentifer
    Private m_typeArgs As IEnumerable(Of TypeName)

    Sub New(conceptName As UnifiedIdentifer, typeArgs As IEnumerable(Of TypeName))
        ' TODO: Complete member initialization 
        m_conceptName = conceptName
        m_typeArgs = typeArgs
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
