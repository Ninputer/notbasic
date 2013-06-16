
Public Class Argument
    Inherits SyntaxTreeNode
    Private m_argexp As Expression

    Sub New(argexp As Expression)
        m_argexp = argexp
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
