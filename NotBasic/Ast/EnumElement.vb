
Public Class EnumElement

    Private _elementName As UnifiedIdentifer
    Private _elementValue As Compilers.Scanners.LexemeValue

    Sub New(elementName As UnifiedIdentifer, elementValue As Compilers.Scanners.LexemeValue)
        ' TODO: Complete member initialization 
        _elementName = elementName
        _elementValue = elementValue
    End Sub

End Class
