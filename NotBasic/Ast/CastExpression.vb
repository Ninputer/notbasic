Imports VBF.Compilers

Public Class CastExpression
    Inherits Expression

    Private _exp As Expression
    Private _typesp As TypeSpecifier

    Sub New(opSpan As SourceSpan, exp As Expression, typesp As TypeSpecifier)
        ' TODO: Complete member initialization 
        _exp = exp
        _typesp = typesp
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
