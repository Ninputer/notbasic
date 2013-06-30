Imports VBF.Compilers

Public Class ForStatement
    Inherits Statement

    Private ForKeywordSpan As SourceSpan
    Private NextKeywordSpan As SourceSpan
    Private LoopVariable As UnifiedIdentifer
    Private TypeSpecifier As TypeSpecifier
    Private FromExpression As Expression
    Private ToExpression As Expression
    Private StepExpression As Expression
    Private ForBody As IEnumerable(Of Statement)

    Sub New(forSpan As SourceSpan, nextSpan As SourceSpan, loopVar As UnifiedIdentifer, typeSp As TypeSpecifier, fromExp As Expression, toExp As Expression, stepExp As Expression, forBody As IEnumerable(Of Statement))        
        Me.ForKeywordSpan = forSpan
        Me.NextKeywordSpan = nextSpan
        Me.LoopVariable = loopVar
        Me.TypeSpecifier = typeSp
        Me.FromExpression = fromExp
        Me.ToExpression = toExp
        Me.StepExpression = stepExp
        Me.ForBody = forBody
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
