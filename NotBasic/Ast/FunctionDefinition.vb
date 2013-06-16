
Public Class FunctionDefinition
    Inherits Definition

    Private m_decl As FunctionSignature
    Private m_sourceSpan As Compilers.SourceSpan
    Private m_statements As IEnumerable(Of Statement)

    Sub New(decl As FunctionSignature, statements As IEnumerable(Of Statement), sourceSpan As Compilers.SourceSpan)
        If statements Is Nothing Then Exit Sub

        m_decl = decl
        m_statements = statements
        m_sourceSpan = sourceSpan
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
