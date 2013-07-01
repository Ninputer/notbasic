
Public Class OperatorSignature
    Inherits ProcedureSignature

    Public Property OperatorKeywordSpan As Compilers.SourceSpan
    Public Property OperatorName As Compilers.Scanners.LexemeValue
    Public Property Parameters As IEnumerable(Of ParameterDeclaration)
    Public Property ReturnTypeSpecifier As TypeSpecifier
    Public Property TypeParameters As IEnumerable(Of TypeParameter)
    Public Property ConstraintClauses As IEnumerable(Of ConstraintClause)

    Sub New(sourceSpan As Compilers.SourceSpan, op As Compilers.Scanners.LexemeValue, paramlist As IEnumerable(Of ParameterDeclaration), returnTypeSp As TypeSpecifier, typeParams As IEnumerable(Of TypeParameter), whereClauses As IEnumerable(Of ConstraintClause))
        Me.OperatorKeywordSpan = sourceSpan
        Me.OperatorName = op
        Me.Parameters = paramlist
        Me.ReturnTypeSpecifier = returnTypeSp
        Me.TypeParameters = typeParams
        Me.ConstraintClauses = whereClauses
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
