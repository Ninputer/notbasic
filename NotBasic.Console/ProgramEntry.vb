Imports VBF.Compilers
Imports VBF.NotBasic.Compiler

Module ProgramEntry

    Sub Main()
        Dim em As New CompilationErrorManager
        Dim parser As New NotBasicParser(em)

        Dim code = <![CDATA[
fun Foo(a, b)
    return 0
end

fun Bar()
    Foo(a +* 1, 
        b - 2)

    a = 
        2 +
        3 - 4

    'if 1 then a = 0
    'if 1 then a = 0  else b = 0
    if a - a * -b =
        d > > 1
        a = 0
    elseif a xor b or c and d
        b = 0
    else
        c = 0
    end

    do while true
        a = 0
    loop
end
]]>
        Dim s = parser.Parse(code.Value)

        System.Console.WriteLine("Scanner creation time: {0}ms", parser.ScannerCreationTime)
        System.Console.WriteLine("Parser creation time: {0}ms", parser.ParserCreationTime)

        ReportErrors(em)
    End Sub

    Sub ReportErrors(errorManager As CompilationErrorManager)

        If (errorManager.Errors.Count > 0) Then

            For Each er In errorManager.Errors.OrderBy(Function(e) e.ErrorPosition.StartLocation.CharIndex)
                System.Console.WriteLine(er.ToString())
            Next
        End If
    End Sub

End Module
