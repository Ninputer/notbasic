Imports VBF.Compilers

Class CastExpression
    Inherits UnaryExpression

    Private _exp As Expression
    Private _typesp As TypeSpecifier

    Sub New(opSpan As SourceSpan, exp As Expression, typesp As TypeSpecifier)
        MyBase.New(opSpan, ExpressionOp.Cast, exp)
        ' TODO: Complete member initialization 
        _exp = exp
        _typesp = typesp
    End Sub

End Class
