Imports VBF.Compilers

Public Class ProcedureDeclaration
    Inherits Definition

    Public Property Signature As ProcedureSignature
    Public Property DeclareKeywordSpan As SourceSpan

    Public Sub New(declareSpan As SourceSpan, signature As ProcedureSignature)
        Me.DeclareKeywordSpan = declareSpan
        Me.Signature = signature
    End Sub

    Public Overrides Function Accept(Of T)(visitor As ISyntaxTreeVisitor(Of T)) As T
        Return visitor.Visit(Me)
    End Function
End Class
