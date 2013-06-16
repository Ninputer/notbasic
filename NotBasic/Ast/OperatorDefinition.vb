
Public Class OperatorDefinition
    Inherits Definition

    Private _decl As OperatorSignature
    Private _statements As IEnumerable(Of Statement)
    Private _sourceSpan As Compilers.SourceSpan

    Sub New(decl As OperatorSignature, statements As IEnumerable(Of Statement), sourceSpan As Compilers.SourceSpan)
        ' TODO: Complete member initialization 
        _decl = decl
        _statements = statements
        _sourceSpan = sourceSpan
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
