
Public MustInherit Class LambdaBody
    Inherits SyntaxTreeNode
    Public Function ToLambdaBody() As LambdaBody
        Return Me
    End Function
End Class

Public Class ExpressionLambdaBody
    Inherits LambdaBody

    Public Property Expression As Expression

    Sub New(exp As Expression)
        Me.Expression = exp
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class

Public Class BlockLambdaBody
    Inherits LambdaBody

    Public Property BlockStatements As IEnumerable(Of Statement)

    Sub New(block As IEnumerable(Of Statement))
        Me.BlockStatements = block
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class