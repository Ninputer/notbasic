
Public Class LambdaSignature
    Inherits SyntaxTreeNode
    Private _params As IEnumerable(Of ParameterDeclaration)

    Sub New(params As IEnumerable(Of ParameterDeclaration))
        ' TODO: Complete member initialization 
        _params = params
    End Sub

End Class
