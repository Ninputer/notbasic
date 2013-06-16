Imports VBF.Compilers.Scanners
Imports VBF.Compilers

Public Class ContinueStatement
    Inherits Statement

    Private _sourceSpan As SourceSpan
    Private _lexemeValue As LexemeValue

    Sub New(sourceSpan As SourceSpan, lexemeValue As LexemeValue)
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _lexemeValue = lexemeValue
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
