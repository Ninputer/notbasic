
Public Class TypeDefinition
    Inherits Definition

    Private _sourceSpan As Compilers.SourceSpan
    Private _typeName As UnifiedIdentifer
    Private _typeParams As IEnumerable(Of TypeParameter)
    Private _whereClauses As IEnumerable(Of ConstraintClause)
    Private _fields As IEnumerable(Of FieldDefinition)
    Private _sourceSpan1 As Compilers.SourceSpan
    Private _baseType As TypeSpecifier


    Sub New(sourceSpan As Compilers.SourceSpan, sourceSpan1 As Compilers.SourceSpan, typeName As UnifiedIdentifer, typeParams As IEnumerable(Of TypeParameter), baseType As TypeSpecifier, whereClauses As IEnumerable(Of ConstraintClause), fields As IEnumerable(Of FieldDefinition))
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _sourceSpan1 = sourceSpan1
        _typeName = typeName
        _typeParams = typeParams
        _baseType = baseType
        _whereClauses = whereClauses
        _fields = fields
    End Sub

End Class
