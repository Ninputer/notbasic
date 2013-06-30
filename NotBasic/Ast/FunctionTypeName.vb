Imports VBF.Compilers

Public Class FunctionTypeName
    Inherits TypeName

    Private FunKeywordSpan As SourceSpan
    Private ParameterTypes As IEnumerable(Of TypeName)
    Private ReturnType As TypeName

    Sub New(sourceSpan As SourceSpan, paramTypes As IEnumerable(Of TypeName), returnType As TypeName)
        ' TODO: Complete member initialization 
        Me.FunKeywordSpan = sourceSpan
        Me.ParameterTypes = paramTypes
        Me.ReturnType = returnType
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
