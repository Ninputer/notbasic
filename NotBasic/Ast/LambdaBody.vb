
Public MustInherit Class LambdaBody
    Inherits SyntaxTreeNode
    Public Function ToLambdaBody() As LambdaBody
        Return Me
    End Function
End Class

Public Class ExpressionLambdaBody
    Inherits LambdaBody

    Private _exp As Expression

    Sub New(exp As Expression)
        ' TODO: Complete member initialization 
        _exp = exp
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class

Public Class BlockLambdaBody
    Inherits LambdaBody

    Private _block As IEnumerable(Of Statement)

    Sub New(block As IEnumerable(Of Statement))
        ' TODO: Complete member initialization 
        _block = block
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class