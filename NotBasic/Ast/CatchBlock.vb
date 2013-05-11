
Public Class CatchBlock

    Private _sourceSpan As Compilers.SourceSpan
    Private _exceptVar As UnifiedIdentifer
    Private _exceptType As TypeSpecifier
    Private _catchBody As IEnumerable(Of Statement)

    Sub New(sourceSpan As Compilers.SourceSpan, exceptVar As UnifiedIdentifer, exceptType As TypeSpecifier, catchBody As IEnumerable(Of Statement))
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _exceptVar = exceptVar
        _exceptType = exceptType
        _catchBody = catchBody
    End Sub

End Class
