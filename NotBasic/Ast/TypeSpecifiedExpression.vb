Imports VBF.Compilers

Public Class TypeSpecifiedExpression
    Inherits Expression

    Private m_exp As Expression
    Private m_targetType As TypeSpecifier

    Sub New(exp As Expression, targetType As TypeSpecifier)
        m_exp = exp
        m_targetType = targetType
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
