Imports VBF.Compilers

Public Class ProcedureDeclaration
    Inherits Definition

    Private m_signature As ProcedureSignature
    Private m_declareSpan As SourceSpan

    Public Sub New(declareSpan As SourceSpan, signature As ProcedureSignature)
        m_declareSpan = declareSpan
        m_signature = signature
    End Sub
End Class
