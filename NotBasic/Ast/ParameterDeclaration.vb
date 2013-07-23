
Public MustInherit Class ParameterDeclaration
    Inherits SyntaxTreeNode

    Public Property TypeSpecifier As TypeSpecifier
    Public Property ParameterPrefix As ParameterPrefix

    Public Function ToBase() As ParameterDeclaration
        Return Me
    End Function

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T

    End Function
End Class

Public Class NormalParameterDeclaration
    Inherits ParameterDeclaration
    Public Property Name As UnifiedIdentifer

    Sub New(did As UnifiedIdentifer, typesp As TypeSpecifier, parameterPrefix As ParameterPrefix)
        Me.Name = did
        Me.TypeSpecifier = typesp
        Me.ParameterPrefix = parameterPrefix
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class

Public Class ExtensionParameterDeclaration
    Inherits ParameterDeclaration

    Sub New(typesp As TypeSpecifier, parameterPrefix As ParameterPrefix)
        Me.TypeSpecifier = typesp
        Me.ParameterPrefix = parameterPrefix
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class