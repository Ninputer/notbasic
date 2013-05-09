
Public Class OperatorDeclaration
    Inherits Declaration

    Private _sourceSpan As Compilers.SourceSpan
    Private _op As Compilers.Scanners.LexemeValue
    Private _paramlist As IEnumerable(Of ParameterDeclaration)
    Private _returnTypeSp As TypeSpecifier
    Private _typeParams As IEnumerable(Of TypeParameter)

    Sub New(sourceSpan As Compilers.SourceSpan, op As Compilers.Scanners.LexemeValue, paramlist As IEnumerable(Of ParameterDeclaration), returnTypeSp As TypeSpecifier, typeParams As IEnumerable(Of TypeParameter))
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _op = op
        _paramlist = paramlist
        _returnTypeSp = returnTypeSp
        _typeParams = typeParams
    End Sub

End Class
