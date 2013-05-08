Public Module ErrorCode
    'Scanner Errors
    Public Const InvalidToken As Integer = 1001

    'Parser Errors
    Public Const MissingToken As Integer = 2001
    Public Const UnexpectedToken As Integer = 2002
    Public Const RightShiftSymbolError As Integer = 2010
    Public Const NotEqualSymbolError As Integer = 2011

    Public Const GeneralSyntaxError As Integer = 2999

End Module
