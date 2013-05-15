Public Class LambdaExpression
    Inherits Expression

    Private _signature As LambdaSignature
    Private _body As LambdaBody

    Sub New(signature As LambdaSignature, body As LambdaBody)
        ' TODO: Complete member initialization 
        _signature = signature
        _body = body
    End Sub

End Class