
Public Class CatchBlock
    Inherits SyntaxTreeData
    Public Property CatchKeywordSpan As Compilers.SourceSpan
    Public Property ExceptionVariable As UnifiedIdentifer
    Public Property ExceptionType As TypeSpecifier
    Public Property CatchBody As IEnumerable(Of Statement)

    Sub New(sourceSpan As Compilers.SourceSpan, exceptVar As UnifiedIdentifer, exceptType As TypeSpecifier, catchBody As IEnumerable(Of Statement))
        Me.CatchKeywordSpan = sourceSpan
        Me.ExceptionVariable = exceptVar
        Me.ExceptionType = exceptType
        Me.CatchBody = catchBody
    End Sub

End Class
