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

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
