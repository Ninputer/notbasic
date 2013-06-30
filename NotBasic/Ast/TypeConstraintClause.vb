
Public Class TypeConstraintClause
    Inherits ConstraintClause

    Public Property LeftTypes As IEnumerable(Of UnifiedIdentifer)
    Public Property RightTypes As IEnumerable(Of UnifiedIdentifer)

    Sub New(leftTypes As IEnumerable(Of UnifiedIdentifer), rightTypes As IEnumerable(Of UnifiedIdentifer))
        Me.LeftTypes = leftTypes
        Me.RightTypes = rightTypes
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
