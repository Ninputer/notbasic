
Public Class IfBlockStatement
    Inherits Statement

    Public Property IfKeywordSpan As Compilers.SourceSpan
    Public Property EndKeywordSpan As Compilers.SourceSpan
    Public Property Condition As Expression
    Public Property TruePart As IEnumerable(Of Statement)
    Public Property ElseIfBlocks As IEnumerable(Of ElseIfBlock)
    Public Property ElseBlock As ElseBlock

    Sub New(ifSpan As Compilers.SourceSpan, endSpan As Compilers.SourceSpan, condition As Expression, truePart As IEnumerable(Of Statement), elseIfBlocks As IEnumerable(Of ElseIfBlock), elseBlockOpt As ElseBlock)
        Me.IfKeywordSpan = ifSpan
        Me.EndKeywordSpan = endSpan
        Me.Condition = condition
        Me.TruePart = truePart
        Me.ElseIfBlocks = elseIfBlocks
        Me.ElseBlock = elseBlockOpt
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
