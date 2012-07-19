
Public Class CompilationUnit
    Inherits SyntaxTreeNode

    Private _functions As IEnumerable(Of FunctionDefinition)

    Sub New(functions As IEnumerable(Of FunctionDefinition))
        ' TODO: Complete member initialization 
        _functions = functions
    End Sub

End Class
