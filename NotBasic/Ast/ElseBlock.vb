
Class ElseBlock

    Private _sourceSpan As Compilers.SourceSpan
    Private _elsePart As IEnumerable(Of Statement)

    Sub New(sourceSpan As Compilers.SourceSpan, elsePart As IEnumerable(Of Statement))
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _elsePart = elsePart
    End Sub

End Class
