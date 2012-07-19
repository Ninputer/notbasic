
Public Class FunctionDefinition
    Inherits SyntaxTreeNode

    Private _decl As FunctionDeclaration
    Private _sourceSpan As Compilers.SourceSpan
    Private _statements As IEnumerable(Of Statement)

    Sub New(decl As FunctionDeclaration, statements As IEnumerable(Of Statement), sourceSpan As Compilers.SourceSpan)
        ' TODO: Complete member initialization 
        _decl = decl
        _statements = statements
        _sourceSpan = sourceSpan
    End Sub

End Class
