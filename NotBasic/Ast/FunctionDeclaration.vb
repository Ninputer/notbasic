
Public Class FunctionDeclaration
    Inherits SyntaxTreeNode

    Private m_sourceSpan As Compilers.SourceSpan
    Private m_name As UnifiedIdentifer
    Private m_paramlist As IList(Of ParameterDeclaration)

    Sub New(sourceSpan As Compilers.SourceSpan, name As UnifiedIdentifer, paramlist As IEnumerable(Of ParameterDeclaration))
        If paramlist Is Nothing Then Exit Sub
        m_sourceSpan = sourceSpan
        m_name = name
        m_paramlist = paramlist.ToArray()
    End Sub

End Class
