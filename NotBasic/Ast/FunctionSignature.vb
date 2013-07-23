Imports VBF.Compilers

Public Class FunctionSignature
    Inherits ProcedureSignature

    Public Property FunKeywordSpan As Compilers.SourceSpan
    Public Property Name As UnifiedIdentifer
    Public Property Parameters As IEnumerable(Of ParameterDeclaration)
    Public Property ReturnTypeSpecifier As TypeSpecifier
    Public Property TypeParameters As IEnumerable(Of TypeParameter)
    Public Property ConstraintClauses As IEnumerable(Of ConstraintClause)

    Sub New(sourceSpan As SourceSpan, name As UnifiedIdentifer, paramlist As IEnumerable(Of ParameterDeclaration), returnTypeSp As TypeSpecifier, typeParams As IEnumerable(Of TypeParameter), whereClauses As IEnumerable(Of ConstraintClause))
        Me.FunKeywordSpan = sourceSpan
        Me.Name = name
        Me.Parameters = paramlist
        Me.ReturnTypeSpecifier = returnTypeSp
        Me.TypeParameters = typeParams
        Me.ConstraintClauses = whereClauses
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function

End Class
