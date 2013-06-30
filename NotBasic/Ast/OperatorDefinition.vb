
Public Class OperatorDefinition
    Inherits Definition

    Public Property Signature As OperatorSignature
    Public Property Statements As IEnumerable(Of Statement)
    Public Property EndKeywordSpan As Compilers.SourceSpan

    Sub New(signature As OperatorSignature, statements As IEnumerable(Of Statement), sourceSpan As Compilers.SourceSpan)
        Me.Signature = signature
        Me.Statements = statements
        Me.EndKeywordSpan = sourceSpan
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
