
Public Class CompilationUnit
    Inherits SyntaxTreeNode

    Public Property Definitions As IEnumerable(Of Definition)

    Sub New(definitions As IEnumerable(Of Definition))
        Me.Definitions = definitions
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
