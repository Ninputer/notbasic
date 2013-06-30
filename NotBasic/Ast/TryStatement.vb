
Public Class TryStatement
    Inherits Statement

    Public Property TryKeywordSpan As Compilers.SourceSpan
    Public Property EndKeywordSpan As Compilers.SourceSpan
    Public Property TryBody As IEnumerable(Of Statement)
    Public Property CatchBlocksWithType As IEnumerable(Of CatchBlock)
    Public Property CatchAllBlock As CatchBlock
    Public Property FinallyBlock As FinallyBlock

    Sub New(trySpan As Compilers.SourceSpan, endSpan As Compilers.SourceSpan, tryBody As IEnumerable(Of Statement), typeCatches As IEnumerable(Of CatchBlock), catchBlock As CatchBlock, finallyBlock As FinallyBlock)
        Me.TryKeywordSpan = trySpan
        Me.EndKeywordSpan = endSpan
        Me.TryBody = tryBody
        Me.CatchBlocksWithType = typeCatches
        Me.CatchAllBlock = catchBlock
        Me.FinallyBlock = finallyBlock
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
