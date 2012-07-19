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
end
]]>
        Dim s = parser.Parse(code.Value)

        Stop
    End Sub

End Module
