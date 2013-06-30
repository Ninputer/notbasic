
Public Class IfThenStatement
    Inherits Statement

    Public Property IfKeywordSpan As Compilers.SourceSpan
    Public Property Condition As Expression
    Public Property TruePart As Statement
    Public Property ElsePart As Statement

    Sub New(ifSpan As Compilers.SourceSpan, condition As Expression, truePart As Statement, elsePart As Statement)
        Me.IfKeywordSpan = ifSpan
        Me.Condition = condition
        Me.TruePart = truePart
        Me.ElsePart = elsePart
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
