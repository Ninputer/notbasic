﻿Imports VBF.Compilers.Scanners

Public Class ParameterPrefix
    Inherits SyntaxTreeNode
    Private _lexemeValue As LexemeValue

    Sub New(lexemeValue As LexemeValue)
        ' TODO: Complete member initialization 
        _lexemeValue = lexemeValue
    End Sub

End Class