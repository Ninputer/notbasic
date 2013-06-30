
Public Class ArrayTypeName
    Inherits TypeName

    Public Property ElementType As TypeName

    Sub New(baseType As TypeName)
        Me.ElementType = baseType
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
