Public Class LambdaExpression
    Inherits Expression

    Private _signature As LambdaSignature
    Private _body As LambdaBody

    Sub New(signature As LambdaSignature, body As LambdaBody)
        ' TODO: Complete member initialization 
        _signature = signature
        _body = body
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class