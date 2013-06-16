Imports System.Text
Imports System.Reflection
Imports VBF.Compilers.Scanners

Public MustInherit Class SyntaxTreeNode

    Public MustOverride Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T

    ''' <summary>
    ''' Dump the syntax tree to a string representation
    ''' </summary>
    Public Overrides Function ToString() As String
        Dim helper As New FormatHelper
        helper.ObjectToString(Me)

        Return helper.ToString()
    End Function
End Class
