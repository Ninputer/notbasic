
Public Class ConcreteDeclaration
    Inherits SyntaxTreeNode
    Private _sourceSpan As Compilers.SourceSpan
    Private _typeParams As IEnumerable(Of TypeParameter)
    Private _conceptName As UnifiedIdentifer
    Private _typeArgs As IEnumerable(Of TypeName)
    Private _whereClauses As IEnumerable(Of ConstraintClause)

    Sub New(sourceSpan As Compilers.SourceSpan, typeParams As IEnumerable(Of TypeParameter), conceptName As UnifiedIdentifer, typeArgs As IEnumerable(Of TypeName), whereClauses As IEnumerable(Of ConstraintClause))
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _typeParams = typeParams
        _conceptName = conceptName
        _typeArgs = typeArgs
        _whereClauses = whereClauses
    End Sub

End Class
