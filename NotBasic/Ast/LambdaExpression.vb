Public Class LambdaExpression
    Inherits Expression

    Public Property Signature As LambdaSignature
    Public Property Body As LambdaBody

    Sub New(signature As LambdaSignature, body As LambdaBody)
        Me.Signature = signature
        Me.Body = body
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class