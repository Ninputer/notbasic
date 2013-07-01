
Public Class QualifiedTypeName
    Inherits TypeName

    Public Property Identifier As UnifiedIdentifer
    Public Property TypeArguments As IEnumerable(Of TypeName)

    Sub New(id As UnifiedIdentifer, typeArgs As IEnumerable(Of TypeName))
        Me.Identifier = id
        Me.TypeArguments = typeArgs
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
