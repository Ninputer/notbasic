Imports VBF.Compilers
Imports VBF.NotBasic.Compiler
Imports System.IO

Module ProgramEntry

    Sub Main()
        Dim em As New CompilationErrorManager
        Dim parser As New NotBasicParser(em)

        Dim code = <![CDATA[
fun Foo(a, b)
    return Test() 
end

fun Bar(f:fun(int)int, g:fun())
    x = f(2)
    return g()
end

fun Sub(s:fun()#):#
    s()
end

operator+(a:int, b:int):List<int>
    a = convert(b):int
    a = convert(b):Point<int>
    return a
end

fun Test<T, U<>>(x:myType<int>) where Functor<U>
    return 0
end

fun FooType(a:int, b:int):int
    return Foo<int>(1, 2)
end

fun Statements()
    Foo(a + 1, 
        b - 2)

    a = 
        2 +
        3 - 4

    'if 1 then a = 0
    'if 1 then a = 0  else b = 0
    if a - a * -b =
        d >> 1
        a = 0
    elseif a <> b xor b or c and d
        b = 0
    else
        c = 0
    end

    do while true
        a = 0
    loop

    for i = 0 to 10
        for j = 100 to 3 step -2
            continue for
        next
        exit for
    next

    try
        x = y - 1
    catch e:SomeException
        print(e)
    catch:AnotherException
        for each x in y
        next
    catch
        return 0
    end
end

fun Lambdas()
    f1 = x => x + 1
    f2 = x:int => x + 1
    f3 = ()=>print(a)
    f4 = (x, y)=>x + y
    f5 = (x:string)=>x
    f6 = x.bind(a=>y.bind(b=>unit(a+b)))

    f7 = x => { return x + 1 }
    f8 = x => { y = x - 1; return y + x }
    f9 = (x:int, y:int) =>
    {
        return x + y
    }
    f10 = a => {}
end

fun Strings()
    s1 = "hello world"
    s2 = "hello""world"""
    s3 = "hello
world
hahaha"

    s4 = "hello,
your name is" + name + ",
world!"

    c1 = "a"c
    c2 = "啊"c
    c3 = &U+03AF
end

fun Dispatch(select a:object)
    'dispatch method
end

fun Dispatch(case a:Point)
    'dispatch method branch
end

fun Empty()
end

fun IfAmbiguity():int
    if a then if b then return 0 else return 1
end

fun NewArrayAmbiguity()
    a = new int[5]
    b = new int[][5]

end

fun SelectCaseStatement()
    select a
    end

    select b
        case 1
            print(a)
        case 2,3,4
            print(b)
        case 3, it > 0 and it < 9
            print(c)
        case else
            print(d)
    end

    select c
        case 1
        case 2
    end

    select d
        case else
    end

    select
        case a > 0
        case else
    end
end

fun ArrayLiteral()
    a = [] 'empty array
    b = [1]
    c = ["x","y","z"]
end

concept Functor<F<>>
    decl fun fmap<T, U>(f:fun(T)U):fun(F<T>)F<U>
end

concept Monad<M<>> where Functor<M>
    decl fun unit<U>(a:U):M<U>
    decl fun bind<T, U>(m:M<T>, f:fun(T)M<U>):M<U>
end

concept Comparable<T>
    decl fun Compare(a:T, b:T):int
    decl operator<(a:T, b:T):bool
end

concept EqualityComparable<T>
    decl fun Equals(a:T, b:T):bool
    operator=(a:T, b:T):bool
        return Equals(a,b)
    end

    operator<>(a:T, b:T):bool
        return not Equals(a,b)
    end    
end

concept Addable<TA,TB,TR> where (TA,TB => TR)
    decl operator+(a:TA, b:TB):TR
end

concrete Comparable<int>
    fun Compare(a,b)
        return a - b
    end
end

concrete<T> Comparable<Point<T>>
    fun Compare(a,b)
        return a.X - b.X
    end

    operator<(a, b)
        return Compare(a,b) < 0
    end
end

type Point<T>
    X:T
    Y:T
end

type NewPoint<T> : Point<T>
    Z:T
end

enum Weekday
    Monday = 1
    Tuesday
    Wednesday
    Thursday
    Friday
    Saturday
    Sunday = 0
end
]]>

        Dim code2 = <![CDATA['dispatch method
end
]]>
        Dim el1 = em.CreateErrorList()
        Dim s = parser.Parse(code.Value, el1)

        System.Console.WriteLine("Scanner creation time: {0}ms", parser.ScannerCreationTime)
        System.Console.WriteLine("Parser creation time: {0}ms", parser.ParserCreationTime)

        ReportErrors(el1)

        If el1.Count = 0 Then
            Dim dump = s.ToString()
            'System.Console.WriteLine(dump)
        End If

        Dim el2 = em.CreateErrorList()
        Using sr As New StreamReader("StdLib\stdconcepts.nb")

            Dim ast = parser.Parse(sr, el2)
            ReportErrors(el2)

            Dim astStr = ast.ToString()

            My.Computer.FileSystem.WriteAllText("stdconcepts.ast.txt", astStr, False)
        End Using
    End Sub

    Sub ReportErrors(errorList As CompilationErrorList)

        If (errorList.Count > 0) Then

            For Each er In errorList.OrderBy(Function(e) e.ErrorPosition.StartLocation.CharIndex)
                System.Console.WriteLine(er.ToString())
            Next
        End If
    End Sub

End Module
