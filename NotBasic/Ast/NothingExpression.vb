Imports VBF.Compilers

Public Class NothingExpression
    Inherits Expression

    Private _sourceSpan As SourceSpan

    Sub New(sourceSpan As SourceSpan)
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
