
Public Class LambdaSignature
    Inherits SyntaxTreeNode
    Public Property Parameters As IEnumerable(Of ParameterDeclaration)

    Sub New(params As IEnumerable(Of ParameterDeclaration))
        Parameters = params
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
