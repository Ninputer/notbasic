
Public Class ConceptDefinition
    Inherits Definition

    Private _decl As ConceptDeclaration
    Private _procedures As IEnumerable(Of Declaration)
    Private _sourceSpan As Compilers.SourceSpan

    Sub New(decl As ConceptDeclaration, procedures As IEnumerable(Of Declaration), sourceSpan As Compilers.SourceSpan)
        ' TODO: Complete member initialization 
        _decl = decl
        _procedures = procedures
        _sourceSpan = sourceSpan
    End Sub

End Class
