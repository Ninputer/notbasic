
Public Class FunctionDeclaration
    Inherits SyntaxTreeNode

    Private _sourceSpan As Compilers.SourceSpan
    Private _name As UnifiedIdentifer
    Private _paramlist As IEnumerable(Of ParameterDeclaration)

    Sub New(sourceSpan As Compilers.SourceSpan, name As UnifiedIdentifer, paramlist As IEnumerable(Of ParameterDeclaration))
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _name = name
        _paramlist = paramlist
    End Sub

End Class
