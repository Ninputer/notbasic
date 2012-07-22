
Class IfThenStatement
    Inherits Statement

    Private _sourceSpan As Compilers.SourceSpan
    Private _condition As Expression
    Private _trueStatement As Statement
    Private _elsePart As Statement

    Sub New(sourceSpan As Compilers.SourceSpan, condition As Expression, trueStatement As Statement, elsePart As Statement)
        ' TODO: Complete member initialization 
        _sourceSpan = sourceSpan
        _condition = condition
        _trueStatement = trueStatement
        _elsePart = elsePart
    End Sub

End Class
