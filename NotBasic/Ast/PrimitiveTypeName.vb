
Public Class PrimitiveTypeName
    Inherits TypeName

    Public Property TypeName As Compilers.Scanners.LexemeValue

    Sub New(typeName As Compilers.Scanners.LexemeValue)
        Me.TypeName = typeName
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
