
Public Class OperatorDefinition
    Inherits Definition

    Private _decl As OperatorDeclaration
    Private _statements As IEnumerable(Of Statement)
    Private _sourceSpan As Compilers.SourceSpan

    Sub New(decl As OperatorDeclaration, statements As IEnumerable(Of Statement), sourceSpan As Compilers.SourceSpan)
        ' TODO: Complete member initialization 
        _decl = decl
        _statements = statements
        _sourceSpan = sourceSpan
    End Sub

End Class
