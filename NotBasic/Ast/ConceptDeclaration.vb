
Public Class ConceptDeclaration
    Inherits SyntaxTreeNode

    Public Property ConceptKeywordSpan As Compilers.SourceSpan
    Public Property Name As UnifiedIdentifer
    Public Property TypeParameters As IEnumerable(Of TypeParameter)
    Public Property ConstraintClauses As IEnumerable(Of ConstraintClause)

    Sub New(keywordSpan As Compilers.SourceSpan, name As UnifiedIdentifer, typeParams As IEnumerable(Of TypeParameter), constraintClauses As IEnumerable(Of ConstraintClause))
        Me.ConceptKeywordSpan = keywordSpan
        Me.Name = name
        Me.TypeParameters = typeParams
        Me.ConstraintClauses = constraintClauses
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
