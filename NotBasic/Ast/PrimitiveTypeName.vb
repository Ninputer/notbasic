
Public Class PrimitiveTypeName
    Inherits TypeName

    Private _lexemeValue As Compilers.Scanners.LexemeValue

    Sub New(lexemeValue As Compilers.Scanners.LexemeValue)
        ' TODO: Complete member initialization 
        _lexemeValue = lexemeValue
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
