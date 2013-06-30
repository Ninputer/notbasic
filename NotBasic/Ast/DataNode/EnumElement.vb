
Public Class EnumElement
    Inherits SyntaxTreeData
    Public Property Name As UnifiedIdentifer
    Public Property Value As Compilers.Scanners.LexemeValue

    Sub New(elementName As UnifiedIdentifer, elementValue As Compilers.Scanners.LexemeValue)
        Me.Name = elementName
        Me.Value = elementValue
    End Sub

End Class
