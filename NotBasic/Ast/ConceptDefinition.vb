
Public Class ConceptDefinition
    Inherits Definition

    Private m_decl As ConceptDeclaration
    Private m_procedures As IEnumerable(Of Definition)
    Private m_endKeywordSpan As Compilers.SourceSpan

    Sub New(decl As ConceptDeclaration, procedures As IEnumerable(Of Definition), endKeywordSpan As Compilers.SourceSpan)
        m_decl = decl
        m_procedures = procedures
        m_endKeywordSpan = endKeywordSpan
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
