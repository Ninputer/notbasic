
Public Class ConceptDeclaration
    Inherits SyntaxTreeNode

    Private m_keywordSpan As Compilers.SourceSpan
    Private m_name As UnifiedIdentifer
    Private m_typeParams As IEnumerable(Of TypeParameter)
    Private m_constraintClauses As IEnumerable(Of ConstraintClause)

    Sub New(keywordSpan As Compilers.SourceSpan, name As UnifiedIdentifer, typeParams As IEnumerable(Of TypeParameter), constraintClauses As IEnumerable(Of ConstraintClause))
        m_keywordSpan = keywordSpan
        m_name = name
        m_typeParams = typeParams
        m_constraintClauses = constraintClauses
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
