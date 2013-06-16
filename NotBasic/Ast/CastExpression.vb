Imports VBF.Compilers

Public Class CastExpression
    Inherits Expression

    Private m_keywordSpan As SourceSpan
    Private m_exp As Expression
    Private m_targetType As TypeSpecifier

    Sub New(keywordSpan As SourceSpan, exp As Expression, targetType As TypeSpecifier)
        m_keywordSpan = keywordSpan
        m_exp = exp
        m_targetType = targetType
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
