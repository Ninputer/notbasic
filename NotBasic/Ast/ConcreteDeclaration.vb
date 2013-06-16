
Public Class ConcreteDeclaration
    Inherits SyntaxTreeNode
    Private m_keywordSpan As Compilers.SourceSpan
    Private m_typeParams As IEnumerable(Of TypeParameter)
    Private m_conceptName As UnifiedIdentifer
    Private m_typeArgs As IEnumerable(Of TypeName)
    Private m_whereClauses As IEnumerable(Of ConstraintClause)

    Sub New(keywordSpan As Compilers.SourceSpan, typeParams As IEnumerable(Of TypeParameter), conceptName As UnifiedIdentifer, typeArgs As IEnumerable(Of TypeName), whereClauses As IEnumerable(Of ConstraintClause))
        m_keywordSpan = keywordSpan
        m_typeParams = typeParams
        m_conceptName = conceptName
        m_typeArgs = typeArgs
        m_whereClauses = whereClauses
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
