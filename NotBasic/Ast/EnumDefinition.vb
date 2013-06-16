
Public Class EnumDefinition
    Inherits Definition

    Private _sourceSpan As Compilers.SourceSpan
    Private _sourceSpan1 As Compilers.SourceSpan
    Private _enumName As UnifiedIdentifer
    Private _elements As IEnumerable(Of EnumElement)

    Sub New(sourceSpan As Compilers.SourceSpan, sourceSpan1 As Compilers.SourceSpan, enumName As UnifiedIdentifer, elements As IEnumerable(Of EnumElement))
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _sourceSpan1 = sourceSpan1
        _enumName = enumName
        _elements = elements
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
