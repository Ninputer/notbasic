Imports VBF.Compilers

Public Class FunctionTypeName
    Inherits TypeName

    Private _sourceSpan As SourceSpan
    Private _paramTypes As IEnumerable(Of TypeName)
    Private _returnType As TypeName

    Sub New(sourceSpan As SourceSpan, paramTypes As IEnumerable(Of TypeName), returnType As TypeName)
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _paramTypes = paramTypes
        _returnType = returnType
    End Sub

End Class
