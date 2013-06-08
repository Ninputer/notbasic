
Public Class CompilationUnit
    Inherits SyntaxTreeNode

    Private m_definitions As IEnumerable(Of Definition)

    Sub New(definitions As IEnumerable(Of Definition))
        m_definitions = definitions
    End Sub

End Class
