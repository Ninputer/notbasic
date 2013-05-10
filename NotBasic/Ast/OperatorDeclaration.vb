
Public Class OperatorDeclaration
    Inherits Declaration

    Private _sourceSpan As Compilers.SourceSpan
    Private _op As Compilers.Scanners.LexemeValue
    Private _paramlist As IEnumerable(Of ParameterDeclaration)
    Private _returnTypeSp As TypeSpecifier
    Private _typeParams As IEnumerable(Of TypeParameter)
    Private _whereClauses As IEnumerable(Of ConceptConstraintClause)

    Sub New(sourceSpan As Compilers.SourceSpan, op As Compilers.Scanners.LexemeValue, paramlist As IEnumerable(Of ParameterDeclaration), returnTypeSp As TypeSpecifier, typeParams As IEnumerable(Of TypeParameter), whereClauses As IEnumerable(Of ConceptConstraintClause))
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _op = op
        _paramlist = paramlist
        _returnTypeSp = returnTypeSp
        _typeParams = typeParams
        _whereClauses = whereClauses
    End Sub

End Class
