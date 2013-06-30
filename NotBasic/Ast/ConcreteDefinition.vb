
Public Class ConcreteDefinition
    Inherits Definition

    Public Property Declaration As ConcreteDeclaration
    Public Property Procedures As IEnumerable(Of Definition)
    Public Property EndKeywordSpan As Compilers.SourceSpan

    Sub New(decl As ConcreteDeclaration, procedures As IEnumerable(Of Definition), endKeywordSpan As Compilers.SourceSpan)
        Me.Declaration = decl
        Me.Procedures = procedures
        Me.EndKeywordSpan = endKeywordSpan
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
