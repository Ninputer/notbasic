Imports VBF.Compilers

Public Class ForEachStatement
    Inherits Statement

    Private _sourceSpan As SourceSpan
    Private _sourceSpan1 As SourceSpan
    Private _loopVar As UnifiedIdentifer
    Private _typeSp As TypeSpecifier
    Private _enumExp As Expression
    Private _forBody As IEnumerable(Of Statement)

    Sub New(sourceSpan As SourceSpan, sourceSpan1 As SourceSpan, loopVar As UnifiedIdentifer, typeSp As TypeSpecifier, enumExp As Expression, forBody As IEnumerable(Of Statement))
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _sourceSpan1 = sourceSpan1
        _loopVar = loopVar
        _typeSp = typeSp
        _enumExp = enumExp
        _forBody = forBody
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
