
Public Class NewArrayExpression
    Inherits Expression

    Public Property NewKeywordSpan As Compilers.SourceSpan
    Public Property LeftBracketSpan As Compilers.SourceSpan
    Public Property RightBracketSpan As Compilers.SourceSpan
    Public Property ArrayType As TypeName
    Public Property Length As Expression

    Sub New(newSpan As Compilers.SourceSpan, lbkSpan As Compilers.SourceSpan, rbkSpan As Compilers.SourceSpan, type As TypeName, length As Expression)
        Me.NewKeywordSpan = newSpan
        Me.LeftBracketSpan = lbkSpan
        Me.RightBracketSpan = rbkSpan
        Me.ArrayType = type
        Me.Length = length
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
