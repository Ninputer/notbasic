
Public Class ReturnStatement
    Inherits Statement

    Public Property ReturnKeywordSpan As Compilers.SourceSpan
    Public Property ReturnValue As Expression

    Sub New(returnSpan As Compilers.SourceSpan, returnValue As Expression)
        Me.ReturnKeywordSpan = returnSpan
        Me.ReturnValue = returnValue
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
