
Public Class ConcreteDeclaration
    Inherits SyntaxTreeNode
    Public Property ConcreteKeywordSpan As Compilers.SourceSpan
    Public Property TypeParameters As IEnumerable(Of TypeParameter)
    Public Property ConceptName As UnifiedIdentifer
    Public Property TypeArguments As IEnumerable(Of TypeName)
    Public Property ConstraintClauses As IEnumerable(Of ConstraintClause)

    Sub New(keywordSpan As Compilers.SourceSpan, typeParams As IEnumerable(Of TypeParameter), conceptName As UnifiedIdentifer, typeArgs As IEnumerable(Of TypeName), whereClauses As IEnumerable(Of ConstraintClause))
        Me.ConcreteKeywordSpan = keywordSpan
        Me.TypeParameters = typeParams
        Me.ConceptName = conceptName
        Me.TypeArguments = typeArgs
        Me.ConstraintClauses = whereClauses
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
