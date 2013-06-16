
Public Class LambdaSignature
    Inherits SyntaxTreeNode
    Private _params As IEnumerable(Of ParameterDeclaration)

    Sub New(params As IEnumerable(Of ParameterDeclaration))
        ' TODO: Complete member initialization 
        _params = params
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
