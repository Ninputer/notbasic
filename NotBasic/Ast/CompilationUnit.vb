
Public Class CompilationUnit
    Inherits SyntaxTreeNode

    Private m_functions As IList(Of FunctionDefinition)

    Sub New(functions As IEnumerable(Of FunctionDefinition))

        If functions Is Nothing Then Exit Sub

        m_functions = functions.ToArray()
    End Sub

End Class
