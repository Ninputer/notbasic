
Public Class BracketExpression
    Inherits Expression

    Private m_object As Object
    Private m_arguments As IEnumerable(Of Argument)

    Sub New(indexable As Object, arguments As IEnumerable(Of Argument))
        m_object = indexable
        m_arguments = arguments
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
