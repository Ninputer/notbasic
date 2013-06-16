Imports VBF.Compilers

Public Class ForStatement
    Inherits Statement

    Private _sourceSpan As SourceSpan
    Private _sourceSpan1 As SourceSpan
    Private _loopVar As UnifiedIdentifer
    Private _typeSp As TypeSpecifier
    Private _fromExp As Expression
    Private _toExp As Expression
    Private _stepExp As Expression
    Private _forBody As IEnumerable(Of Statement)

    Sub New(sourceSpan As SourceSpan, sourceSpan1 As SourceSpan, loopVar As UnifiedIdentifer, typeSp As TypeSpecifier, fromExp As Expression, toExp As Expression, stepExp As Expression, forBody As IEnumerable(Of Statement))
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _sourceSpan1 = sourceSpan1
        _loopVar = loopVar
        _typeSp = typeSp
        _fromExp = fromExp
        _toExp = toExp
        _stepExp = stepExp
        _forBody = forBody
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
