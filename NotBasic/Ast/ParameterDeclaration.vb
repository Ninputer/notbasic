
Public Class ParameterDeclaration
    Inherits SyntaxTreeNode
    Public Property Name As UnifiedIdentifer
    Public Property TypeSpecifier As TypeSpecifier
    Public Property ParameterPrefix As ParameterPrefix

    Sub New(did As UnifiedIdentifer, typesp As TypeSpecifier, parameterPrefix As ParameterPrefix)
        Me.Name = did
        Me.TypeSpecifier = typesp
        Me.ParameterPrefix = parameterPrefix
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
