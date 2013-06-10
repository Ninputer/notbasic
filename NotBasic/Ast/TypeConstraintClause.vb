
Public Class TypeConstraintClause
    Inherits ConstraintClause

    Private _leftTypes As IEnumerable(Of UnifiedIdentifer)
    Private _rightTypes As IEnumerable(Of UnifiedIdentifer)

    Sub New(leftTypes As IEnumerable(Of UnifiedIdentifer), rightTypes As IEnumerable(Of UnifiedIdentifer))
        ' TODO: Complete member initialization 
        _leftTypes = leftTypes
        _rightTypes = rightTypes
    End Sub

End Class
