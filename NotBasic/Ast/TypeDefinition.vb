
Public Class TypeDefinition
    Inherits Definition

    Public Property TypeKeywordSpan As Compilers.SourceSpan
    Public Property Name As UnifiedIdentifer
    Public Property TypeParameters As IEnumerable(Of TypeParameter)
    Public Property ConstraintClauses As IEnumerable(Of ConstraintClause)
    Public Property Fields As IEnumerable(Of FieldDefinition)
    Public Property EndKeywordSpan As Compilers.SourceSpan
    Public Property BaseTypeSpecifier As TypeSpecifier


    Sub New(typeSpan As Compilers.SourceSpan, endSpan As Compilers.SourceSpan, typeName As UnifiedIdentifer, typeParams As IEnumerable(Of TypeParameter), baseType As TypeSpecifier, whereClauses As IEnumerable(Of ConstraintClause), fields As IEnumerable(Of FieldDefinition))
        Me.TypeKeywordSpan = typeSpan
        Me.EndKeywordSpan = endSpan
        Me.Name = typeName
        Me.TypeParameters = typeParams
        Me.BaseTypeSpecifier = baseType
        Me.ConstraintClauses = whereClauses
        Me.Fields = fields
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
