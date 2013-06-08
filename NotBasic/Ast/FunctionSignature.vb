Imports VBF.Compilers

Public Class FunctionSignature
    Inherits ProcedureSignature

    Private m_sourceSpan As Compilers.SourceSpan
    Private m_name As UnifiedIdentifer
    Private m_paramlist As IList(Of ParameterDeclaration)
    Private _sourceSpan As Compilers.SourceSpan
    Private _name As UnifiedIdentifer
    Private _paramlist As IEnumerable(Of ParameterDeclaration)
    Private _returnTypeSp As TypeSpecifier
    Private _typeParams As IEnumerable(Of TypeParameter)
    Private _whereClauses As IEnumerable(Of ConceptConstraintClause)

  
    Sub New(sourceSpan As SourceSpan, name As UnifiedIdentifer, paramlist As IEnumerable(Of ParameterDeclaration), returnTypeSp As TypeSpecifier, typeParams As IEnumerable(Of TypeParameter), whereClauses As IEnumerable(Of ConceptConstraintClause))
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _name = name
        _paramlist = paramlist
        _returnTypeSp = returnTypeSp
        _typeParams = typeParams
        _whereClauses = whereClauses
    End Sub



End Class
