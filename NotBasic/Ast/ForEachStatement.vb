Imports VBF.Compilers

Public Class ForEachStatement
    Inherits Statement

    Public Property ForKeywordSpan As SourceSpan
    Public Property EachKeywordSpan As SourceSpan
    Public Property LoopVariable As UnifiedIdentifer
    Public Property TypeSpecifier As TypeSpecifier
    Public Property EnumerableExpression As Expression
    Public Property ForBody As IEnumerable(Of Statement)

    Sub New(forSpan As SourceSpan, eachSpan As SourceSpan, loopVar As UnifiedIdentifer, typeSp As TypeSpecifier, enumExp As Expression, forBody As IEnumerable(Of Statement))
        Me.ForKeywordSpan = forSpan
        Me.EachKeywordSpan = eachSpan
        Me.LoopVariable = loopVar
        Me.TypeSpecifier = typeSp
        Me.EnumerableExpression = enumExp
        Me.ForBody = forBody
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
