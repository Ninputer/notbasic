Imports NUnit.Framework
Imports VBF.NotBasic.Compiler
Imports VBF.Compilers

<TestFixture>
Public Class ScannerTest

    Private errorManager As New CompilationErrorManager
    Private parser As New NotBasicParser(errorManager)

    <SetUp>
    Public Sub Setup()
        errorManager.ClearErrors()
    End Sub

    <Test>
    Public Sub LineBreakerTest()
        
    End Sub
End Class
