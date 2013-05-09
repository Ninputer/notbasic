Imports VBF.Compilers

Public Class FunctionDeclaration
    Inherits Declaration

    Private m_sourceSpan As Compilers.SourceSpan
    Private m_name As UnifiedIdentifer
    Private m_paramlist As IList(Of ParameterDeclaration)
    Private _sourceSpan As Compilers.SourceSpan
    Private _name As UnifiedIdentifer
    Private _paramlist As IEnumerable(Of ParameterDeclaration)
    Private _returnTypeSp As TypeSpecifier
    Private _typeParams As IEnumerable(Of TypeParameter)

    Sub New(sourceSpan As SourceSpan, name As UnifiedIdentifer, paramlist As IEnumerable(Of ParameterDeclaration))
        If paramlist Is Nothing Then Exit Sub
        m_sourceSpan = sourceSpan
        m_name = name
        m_paramlist = paramlist.ToArray()
    End Sub

    Sub New(sourceSpan As SourceSpan, name As UnifiedIdentifer, paramlist As IEnumerable(Of ParameterDeclaration), returnTypeSp As TypeSpecifier)
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _name = name
        _paramlist = paramlist
        _returnTypeSp = returnTypeSp
    End Sub

    Sub New(sourceSpan As SourceSpan, name As UnifiedIdentifer, paramlist As IEnumerable(Of ParameterDeclaration), returnTypeSp As TypeSpecifier, typeParams As IEnumerable(Of TypeParameter))
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _name = name
        _paramlist = paramlist
        _returnTypeSp = returnTypeSp
        _typeParams = typeParams
    End Sub

   

End Class
