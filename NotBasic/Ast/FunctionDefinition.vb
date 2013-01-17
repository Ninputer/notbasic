
Public Class FunctionDefinition
    Inherits SyntaxTreeNode

    Private m_decl As FunctionDeclaration
    Private m_sourceSpan As Compilers.SourceSpan
    Private m_statements As IList(Of Statement)

    Sub New(decl As FunctionDeclaration, statements As IEnumerable(Of Statement), sourceSpan As Compilers.SourceSpan)
        If statements Is Nothing Then Exit Sub

        m_decl = decl
        m_statements = statements.ToArray()
        m_sourceSpan = sourceSpan
    End Sub

End Class
