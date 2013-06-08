
Public Class ConceptDeclaration
    Inherits SyntaxTreeNode
    Private _sourceSpan As Compilers.SourceSpan
    Private _name As UnifiedIdentifer
    Private _typeParams As IEnumerable(Of TypeParameter)
    Private _constraintClauses As IEnumerable(Of ConceptConstraintClause)

    Sub New(sourceSpan As Compilers.SourceSpan, name As UnifiedIdentifer, typeParams As IEnumerable(Of TypeParameter), constraintClauses As IEnumerable(Of ConceptConstraintClause))
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _name = name
        _typeParams = typeParams
        _constraintClauses = constraintClauses
    End Sub

End Class
