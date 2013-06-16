
Public Class ConcreteDefinition
    Inherits Definition

    Private _decl As ConcreteDeclaration
    Private _procedures As IEnumerable(Of Definition)
    Private _sourceSpan As Compilers.SourceSpan

    Sub New(decl As ConcreteDeclaration, procedures As IEnumerable(Of Definition), sourceSpan As Compilers.SourceSpan)
        ' TODO: Complete member initialization 
        _decl = decl
        _procedures = procedures
        _sourceSpan = sourceSpan
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
