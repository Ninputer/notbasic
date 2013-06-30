
Public Class MemberAccessExpression
    Inherits Expression

    Public Property Target As Expression
    Public Property MemberName As UnifiedIdentifer
    Public Property TypeArguments As IEnumerable(Of TypeName)

    Sub New(exp As Expression, memberName As UnifiedIdentifer, typeArgs As IEnumerable(Of TypeName))
        Me.Target = exp
        Me.MemberName = memberName
        Me.TypeArguments = typeArgs
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
