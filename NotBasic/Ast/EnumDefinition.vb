
Public Class EnumDefinition
    Inherits Definition

    Public Property EnumKeywordSpan As Compilers.SourceSpan
    Public Property EndKeywordSpan As Compilers.SourceSpan
    Public Property Name As UnifiedIdentifer
    Public Property EnumElements As IEnumerable(Of EnumElement)

    Sub New(sourceSpan As Compilers.SourceSpan, sourceSpan1 As Compilers.SourceSpan, enumName As UnifiedIdentifer, elements As IEnumerable(Of EnumElement))
        EnumKeywordSpan = sourceSpan
        EndKeywordSpan = sourceSpan1
        EnumName = enumName
        EnumElements = elements
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
