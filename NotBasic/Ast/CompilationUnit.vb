
Public Class CompilationUnit
    Inherits SyntaxTreeNode

    Private m_definitions As IList(Of Definition)

    Sub New(definitions As IEnumerable(Of Definition))

        If definitions Is Nothing Then Exit Sub

        m_definitions = definitions.ToArray()
    End Sub

End Class
