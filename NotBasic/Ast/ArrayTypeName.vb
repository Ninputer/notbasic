
Public Class ArrayTypeName
    Inherits TypeName

    Private m_baseType As TypeName

    Sub New(baseType As TypeName)
        m_baseType = baseType
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
