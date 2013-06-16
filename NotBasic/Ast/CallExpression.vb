
Public Class CallExpression
    Inherits Expression

    Private m_object As Expression
    Private m_arguments As IEnumerable(Of Argument)

    Sub New(obj As Expression, arguments As IEnumerable(Of Argument))
        m_object = obj
        m_arguments = arguments
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
