Imports VBF.Compilers
Imports VBF.Compilers.Scanners
Imports VBF.Compilers.Parsers
Imports VBF.Compilers.Scanners.RegularExpression
Imports System.Globalization

Public Class NotBasicParser
    Inherits ParserBase(Of CompilationUnit)

    'Lexer states
    Private m_identifierLexerIndex As Integer
    Private m_keywordLexerIndex As Integer
    Private m_declareLexerIndex As Integer
    Private m_propertyLexerIndex As Integer

    'keywords
    Private AsKeyword As Token
    Private StringKeyword As Token
    Private IntKeyword As Token
    Private ShortKeyword As Token
    Private ByteKeyword As Token
    Private CharKeyword As Token
    Private LongKeyword As Token
    Private BoolKeyword As Token
    Private SingleKeyword As Token
    Private DoubleKeyword As Token
    Private IfKeyword As Token
    Private ThenKeyword As Token
    Private ElseKeyword As Token
    Private ElseIfKeyword As Token
    Private EndKeyword As Token
    Private PrintKeyword As Token
    Private TrueKeyword As Token
    Private FalseKeyword As Token
    Private NewKeyword As Token
    Private SubKeyword As Token
    Private FunctionKeyword As Token
    Private ReturnKeyword As Token
    Private ExitKeyword As Token
    Private TryKeyword As Token
    Private CatchKeyword As Token
    Private FinallyKeyword As Token
    Private WhenKeyword As Token
    Private DoKeyword As Token
    Private WhileKeyword As Token
    Private UntilKeyword As Token
    Private LoopKeyword As Token
    Private ContinueKeyword As Token
    Private ForKeyword As Token
    Private NextKeyword As Token
    Private SelectKeyword As Token
    Private CaseKeyword As Token
    Private NothingKeyword As Token
    Private AndKeyword As Token
    Private OrKeyword As Token
    Private NotKeyword As Token
    Private XorKeyword As Token
    Private ModKeyword As Token

    'contextural keywords
    Private GetKeyword As Token
    Private SetKeyword As Token
    Private DeclareKeyword As Token

    Private Identifier As Token
    Private EscapedIdentifier As Token

    Private IntegerLiteral As Token
    Private FloatLiteral As Token
    Private RawStringLiteral As Token
    Private EmbeddedExpStringBegin As Token
    Private EmbeddedExpStringMiddle As Token
    Private EmbeddedExpStringEnd As Token
    Private CharLiteral As Token

    'punctuations
    Private Colon As Token ':
    Private Comma As Token ',
    Private LeftPth As Token '(
    Private RightPth As Token ')
    Private Semicolon As Token ';
    Private LeftBrck As Token '[
    Private RightBrck As Token ']
    Private LeftBrce As Token '{
    Private RightBrce As Token '}
    Private AtSymbol As Token '@
    Private PlusSymbol As Token '+
    Private MinusSymbol As Token '-
    Private Asterisk As Token '*
    Private Slash As Token '/
    Private CapSymbol As Token '^
    Private EqualSymbol As Token '=
    Private LessSymbol As Token '<
    Private GreaterSymbol As Token '>
    Private LessEqual As Token '<=
    Private GreaterEqual As Token '>=
    Private NotEqual As Token '<>
    Private ShiftLeft As Token '<<

    Private LineTerminator As Token

    'trivias
    Private WhiteSpace As Token
    Private Comment As Token

    Public Sub New(errorManager As CompilationErrorManager)
        MyBase.New(errorManager)
    End Sub

    Protected Overrides Sub OnDefineLexer(lexicon As Lexicon, triviaTokens As ICollection(Of Token))

        'identifier lexer without keyword
        Dim identifierLexer = lexicon.Lexer

        'lexer with reserved keywords
        Dim keywordLexer = identifierLexer.CreateSubLexer()

        'lexer for declaration modifiers
        Dim declareLexer = keywordLexer.CreateSubLexer()

        'lexer for property body (get/set proc)
        Dim propertyLexer = keywordLexer.CreateSubLexer()

        m_identifierLexerIndex = identifierLexer.Index
        m_keywordLexerIndex = keywordLexer.Index
        m_declareLexerIndex = declareLexer.Index
        m_propertyLexerIndex = propertyLexer.Index

        Dim lettersCategories As New HashSet(Of UnicodeCategory)() From
        {
            UnicodeCategory.LetterNumber,
            UnicodeCategory.LowercaseLetter,
            UnicodeCategory.ModifierLetter,
            UnicodeCategory.OtherLetter,
            UnicodeCategory.TitlecaseLetter,
            UnicodeCategory.UppercaseLetter
        }

        Dim combiningCategories As New HashSet(Of UnicodeCategory)() From
        {
            UnicodeCategory.NonSpacingMark,
            UnicodeCategory.SpacingCombiningMark
        }

        Dim lineTerminators As New HashSet(Of Char) From
        {
            ChrW(&HD), ChrW(&HA), ChrW(&H85), ChrW(&H2028), ChrW(&H2029)
        }

        Dim specialStringChar As New HashSet(Of Char) From
        {
            """"c, "<"c, "#"c, ">"c
        }

        Dim letterChar As RegularExpression
        Dim combiningChar As RegularExpression
        Dim decimalDigitChar As RegularExpression
        Dim connectingChar As RegularExpression
        Dim formattingChar As RegularExpression
        Dim spaceChar As RegularExpression
        Dim inputChar As RegularExpression
        Dim strictStringChar As RegularExpression

        Dim charSetBuilder As New CharSetExpressionBuilder()

        charSetBuilder.DefineCharSet(Function(c) lettersCategories.Contains(Char.GetUnicodeCategory(c)), Sub(re) letterChar = re)
        charSetBuilder.DefineCharSet(Function(c) combiningCategories.Contains(Char.GetUnicodeCategory(c)), Sub(re) combiningChar = re)
        charSetBuilder.DefineCharSet(Function(c) Char.GetUnicodeCategory(c) = UnicodeCategory.SpaceSeparator, Sub(re) spaceChar = re)
        charSetBuilder.DefineCharSet(Function(c) Char.GetUnicodeCategory(c) = UnicodeCategory.DecimalDigitNumber, Sub(re) decimalDigitChar = re)
        charSetBuilder.DefineCharSet(Function(c) Char.GetUnicodeCategory(c) = UnicodeCategory.ConnectorPunctuation, Sub(re) connectingChar = re)
        charSetBuilder.DefineCharSet(Function(c) Char.GetUnicodeCategory(c) = UnicodeCategory.Format, Sub(re) formattingChar = re)
        charSetBuilder.DefineCharSet(Function(c) Not lineTerminators.Contains(c), Sub(re) inputChar = re)
        charSetBuilder.DefineCharSet(Function(c) (Not specialStringChar.Contains(c)) AndAlso (Not lineTerminators.Contains(c)), Sub(re) strictStringChar = re Or Literal(""""""))

        charSetBuilder.Build()

        Dim lineTerminatorChar = CharSet(lineTerminators)
        Dim whitespaceChar = spaceChar Or Symbol(ChrW(&H9)) Or Symbol(ChrW(&HB)) Or Symbol(ChrW(&HC))

        '========== define identifiers ========== 
        With identifierLexer
            Dim identifierStartChar = letterChar Or Symbol("_"c)
            Dim identifierPartChar = letterChar Or decimalDigitChar Or connectingChar Or combiningChar Or formattingChar

            Identifier = .DefineToken(identifierStartChar >> identifierPartChar.Many(), "identifier")
            EscapedIdentifier = .DefineToken(Symbol("$"c) >> identifierStartChar >> identifierPartChar.Many(), "identifier")
        End With

        '========== define literals ========== 
        With identifierLexer

            Dim digit = CharSet("0123456789")
            Dim hexDigit = CharSet("0123456789abcdefABCDEF")
            Dim octalDigit = CharSet("01234567")

            Dim intLiteral = digit.Many1()
            Dim hexLiteral = Literal("&H") >> hexDigit.Many1()
            Dim octalLiteral = Literal("&O") >> octalDigit.Many1()

            Dim shortChar = CharSet("sS")
            Dim longChar = CharSet("lL")

            Dim integralTypeChar = shortChar Or longChar
            Dim integralLiteralValue = intLiteral Or hexLiteral Or octalLiteral
            IntegerLiteral = .DefineToken(integralLiteralValue >> integralTypeChar.Optional(), "integral literal")

            Dim singleChar = CharSet("fF")
            Dim doubleChar = CharSet("rR")

            Dim floatTypeChar = singleChar Or doubleChar

            Dim sign = CharSet("+-")
            Dim exponent = CharSet("eE") >> sign >> intLiteral
            Dim floatLiteralValue = (intLiteral >> Symbol("."c) >> intLiteral >> exponent.Optional()) Or
                (Symbol("."c) >> intLiteral >> exponent.Optional()) Or
                (intLiteral >> exponent)

            FloatLiteral = .DefineToken((floatLiteralValue >> floatTypeChar.Optional()) Or (intLiteral >> floatTypeChar), "float point literal")

            Dim embeddedExpEndSymbol = Literal("#>")
            Dim doubleQuote = Symbol(""""c)

            Dim delimitedStringSection = Symbol("#"c) Or (Symbol("<"c).Many() >> (strictStringChar Or lineTerminatorChar))

            RawStringLiteral = .DefineToken(
                doubleQuote >> delimitedStringSection.Many() >> doubleQuote,
                "string literal")

            EmbeddedExpStringBegin = .DefineToken(
                doubleQuote >> delimitedStringSection.Many() >> Symbol("<"c).Many1() >> Symbol("#"c),
                "string literal ends with '<#'")

            EmbeddedExpStringMiddle = .DefineToken(
                embeddedExpEndSymbol >> delimitedStringSection.Many() >> Symbol("<"c).Many1() >> Symbol("#"c),
                "string literal starts with '#>' and ends with '<#'")

            EmbeddedExpStringEnd = .DefineToken(
                embeddedExpEndSymbol >> delimitedStringSection.Many() >> doubleQuote,
                "string literal starts with '#>'")

            Dim charChar = CharSet("cC")
            Dim textCharLiteral = doubleQuote >> strictStringChar >> doubleQuote >> charChar
            Dim unicodeCharLiteral = Literal("&U+") >> hexDigit.Many1()

            CharLiteral = .DefineToken(textCharLiteral Or unicodeCharLiteral, "char literal")
        End With

        '========== define punctuations ========== 
        With identifierLexer
            Colon = .DefineToken(Symbol(":"c))
            Comma = .DefineToken(Symbol(","c))
            LeftPth = .DefineToken(Symbol("("c))
            RightPth = .DefineToken(Symbol(")"c))
            Semicolon = .DefineToken(Symbol(";"c))
            LeftBrck = .DefineToken(Symbol("["c))
            RightBrck = .DefineToken(Symbol("]"c))
            LeftBrce = .DefineToken(Symbol("{"c))
            RightBrce = .DefineToken(Symbol("}"c))
            AtSymbol = .DefineToken(Symbol("@"c))
            PlusSymbol = .DefineToken(Symbol("+"c))
            MinusSymbol = .DefineToken(Symbol("-"c))
            Asterisk = .DefineToken(Symbol("*"c))
            Slash = .DefineToken(Symbol("/"c))
            CapSymbol = .DefineToken(Symbol("^"c))
            EqualSymbol = .DefineToken(Symbol("="c))
            LessSymbol = .DefineToken(Symbol("<"c))
            GreaterSymbol = .DefineToken(Symbol(">"c))
            LessEqual = .DefineToken(Literal("<="))
            GreaterEqual = .DefineToken(Literal(">="))
            NotEqual = .DefineToken(Literal("<>"))
            ShiftLeft = .DefineToken(Literal("<<"))

            LineTerminator = .DefineToken(lineTerminatorChar Or Literal(vbCrLf), "line terminator")
        End With

        '========== define trivias ========== 
        With identifierLexer
            WhiteSpace = .DefineToken(whitespaceChar.Many1(), "white space")
            Comment = .DefineToken((Symbol("'"c) >> inputChar.Many()), "comment")
        End With

        '========== Define reserved keywords ========== 
        With keywordLexer
            AsKeyword = .DefineToken(Literal("as"))
            StringKeyword = .DefineToken(Literal("string"))
            IntKeyword = .DefineToken(Literal("int"))
            ShortKeyword = .DefineToken(Literal("short"))
            ByteKeyword = .DefineToken(Literal("byte"))
            CharKeyword = .DefineToken(Literal("char"))
            LongKeyword = .DefineToken(Literal("long"))
            BoolKeyword = .DefineToken(Literal("bool"))
            SingleKeyword = .DefineToken(Literal("single"))
            DoubleKeyword = .DefineToken(Literal("double"))
            IfKeyword = .DefineToken(Literal("if"))
            ThenKeyword = .DefineToken(Literal("then"))
            ElseKeyword = .DefineToken(Literal("else"))
            ElseIfKeyword = .DefineToken(Literal("elseif"))
            EndKeyword = .DefineToken(Literal("end"))
            PrintKeyword = .DefineToken(Literal("print"))
            TrueKeyword = .DefineToken(Literal("true"))
            FalseKeyword = .DefineToken(Literal("false"))
            NewKeyword = .DefineToken(Literal("new"))
            SubKeyword = .DefineToken(Literal("sub"))
            FunctionKeyword = .DefineToken(Literal("fun"))
            ReturnKeyword = .DefineToken(Literal("return"))
            ExitKeyword = .DefineToken(Literal("exit"))
            TryKeyword = .DefineToken(Literal("try"))
            CatchKeyword = .DefineToken(Literal("catch"))
            FinallyKeyword = .DefineToken(Literal("finally"))
            WhenKeyword = .DefineToken(Literal("when"))
            DoKeyword = .DefineToken(Literal("do"))
            WhileKeyword = .DefineToken(Literal("while"))
            UntilKeyword = .DefineToken(Literal("until"))
            LoopKeyword = .DefineToken(Literal("loop"))
            ContinueKeyword = .DefineToken(Literal("continue"))
            ForKeyword = .DefineToken(Literal("for"))
            NextKeyword = .DefineToken(Literal("next"))
            SelectKeyword = .DefineToken(Literal("select"))
            CaseKeyword = .DefineToken(Literal("case"))
            NothingKeyword = .DefineToken(Literal("nothing"))
            AndKeyword = .DefineToken(Literal("and"))
            OrKeyword = .DefineToken(Literal("or"))
            NotKeyword = .DefineToken(Literal("not"))
            XorKeyword = .DefineToken(Literal("xor"))
            ModKeyword = .DefineToken(Literal("mod"))
        End With

        'define contextual keywords for procedure declaration
        With declareLexer
            DeclareKeyword = .DefineToken(Literal("declare"))
        End With

        'define contextual keywords for property declaration
        With propertyLexer
            GetKeyword = .DefineToken(Literal("get"))
            SetKeyword = .DefineToken(Literal("set"))
        End With

        triviaTokens.Add(Comment)
        triviaTokens.Add(WhiteSpace)
    End Sub

    Private ReferenceIdentifier As New Production(Of UnifiedIdentifer)
    Private QualifiedIdentifier As New Production(Of UnifiedIdentifer)
    Private StatementTerminator As New Production(Of SourceSpan)
    Private DeclaringIdentifier As New Production(Of UnifiedIdentifer)
    Private TypeName As New Production(Of TypeName)
    Private ArrayTypeName As New Production(Of TypeName)
    Private PrimitiveTypeName As New Production(Of TypeName)
    Private QualifiedTypeName As New Production(Of TypeName)

    Private Program As New Production(Of CompilationUnit)
    Private ParameterList As New Production(Of IEnumerable(Of ParameterDeclaration))
    Private ParameterDeclaration As New Production(Of ParameterDeclaration)
    Private FunctionDeclaration As New Production(Of FunctionDeclaration)
    Private FunctionDefinition As New Production(Of FunctionDefinition)

    Private Statements As New Production(Of IEnumerable(Of Statement))
    Private Statement As New Production(Of Statement)
    Private SingleLineStatement As New Production(Of Statement)
    Private BlockStatement As New Production(Of Statement)

    Private ReturnStatement As New Production(Of Statement)
    Private AssignmentStatement As New Production(Of Statement)
    Private ExpressionStatement As New Production(Of Statement)
    Private CallStatement As New Production(Of Statement)
    Private IfThenStatement As New Production(Of Statement)
    Private IfBlockStatement As New Production(Of Statement)
    Private DoStatement As New Production(Of Statement)

    Private Expression As New Production(Of Expression)
    Private PrimaryExpression As New Production(Of Expression)
    Private FactorExpression As New Production(Of Expression)
    Private TermExpression As New Production(Of Expression)
    Private ComparandExpression As New Production(Of Expression)
    Private ComparisonExpression As New Production(Of Expression)
    Private EqualityExpression As New Production(Of Expression)
    Private AndExpression As New Production(Of Expression)
    Private OrExpression As New Production(Of Expression)
    Private XorExpression As New Production(Of Expression)
    Private ShiftingExpression As New Production(Of Expression)
    Private UnaryExpression As New Production(Of Expression)
    Private IntegerLiteralExpression As New Production(Of Expression)
    Private FloatLiteralExpression As New Production(Of Expression)
    Private NumericLiteralExpression As New Production(Of Expression)
    Private BooleanLiteralExpression As New Production(Of Expression)
    Private NewArrayExpression As New Production(Of Expression)
    Private ReferenceExpression As New Production(Of Expression)

    Private BracketExpression As New Production(Of Expression)
    Private CallExpression As New Production(Of Expression)

    Private ArgumentList As New Production(Of ArgumentList)

    Protected Overrides Sub OnDefineParserErrors(errorDefinition As SyntaxErrors, errorManager As CompilationErrorManager)

        With errorDefinition
            .LexicalErrorId = ErrorCode.InvalidToken
            .TokenMissingId = ErrorCode.MissingToken
            .TokenUnexpectedId = ErrorCode.UnexpectedToken
            .OtherErrorId = ErrorCode.GeneralSyntaxError
        End With

        MyBase.OnDefineParserErrors(errorDefinition, errorManager)

        errorManager.DefineError(ErrorCode.RightShiftSymbolError, 0, CompilationStage.Parsing, "Spaces between >> operator are not allowed")
    End Sub

    Protected Overrides Function OnDefineGrammar() As ProductionBase(Of CompilationUnit)
        StatementTerminator.Rule =
            From terminator In (LineTerminator.AsTerminal() Or Semicolon.AsTerminal())
            Select terminator.Span

        Dim LineContinuation = LineTerminator.Optional()

        'Statement terminator
        Dim ST = StatementTerminator.Many1()

        '=======================================================================
        ' Basic Structures
        '=======================================================================

        DeclaringIdentifier.Rule =
            (From id In Identifier
            Select UnifiedIdentifer.FromIdentifier(id)) Or
            (From eid In EscapedIdentifier
            Select UnifiedIdentifer.FromEscapedIdentifier(eid))

        ReferenceIdentifier.Rule =
            DeclaringIdentifier

        QualifiedIdentifier.Rule =
            (From id In Identifier.AsTerminal()
             Select UnifiedIdentifer.FromIdentifier(id)) Or
            (From eid In EscapedIdentifier
             Select UnifiedIdentifer.FromEscapedIdentifier(eid))

        TypeName.Rule =
            QualifiedTypeName Or
            PrimitiveTypeName
        'ArrayTypeName Or

        QualifiedTypeName.Rule =
            (From id In ReferenceIdentifier Select DirectCast(New QualifiedTypeName(id), TypeName)) Or
            (From qualifier In QualifiedTypeName
             From id In QualifiedIdentifier
             Select DirectCast(New QualifiedTypeName(DirectCast(qualifier, QualifiedTypeName), id), TypeName))

        PrimitiveTypeName.Rule =
            From inttype In IntKeyword
            Select DirectCast(New PrimitiveTypeName(), TypeName)



        '=======================================================================
        ' Program Entry
        '=======================================================================

        Program.Rule =
            From _emptylines In StatementTerminator.Many
            From functions In FunctionDefinition.Many()
            Select New CompilationUnit(functions)

        '=======================================================================
        ' Functions
        '=======================================================================

        ParameterList.Rule =
           ParameterDeclaration.Many(Comma.AsTerminal().SuffixedBy(LineContinuation))

        ParameterDeclaration.Rule =
            From did In DeclaringIdentifier
            Select New ParameterDeclaration(did)

        'FunctionDeclaration := fun name ( arglist ) <st>
        FunctionDeclaration.Rule =
            From keyword In FunctionKeyword
            From name In DeclaringIdentifier
            From _lpth In LeftPth
            From _nl1 In LineContinuation
            From paramlist In ParameterList
            From _nl2 In LineContinuation
            From _rpth In RightPth
            From _st In ST
            Select New FunctionDeclaration(keyword.Span, name, paramlist)

        FunctionDefinition.Rule =
            From decl In FunctionDeclaration
            From statements In statements
            From endfun In EndKeyword
            From _st In ST
            Select New FunctionDefinition(decl, statements, endfun.Span)

        '=======================================================================
        ' Statements
        '=======================================================================

        Statements.Rule =
            Statement.Many()

        Statement.Rule =
            SingleLineStatement.SuffixedBy(ST) Or
            BlockStatement

        SingleLineStatement.Rule =
            ReturnStatement Or
            AssignmentStatement Or
            IfThenStatement Or
            ExpressionStatement

        BlockStatement.Rule =
            IfBlockStatement Or
            DoStatement

        ReturnStatement.Rule =
            From keyword In ReturnKeyword
            From _lc In LineContinuation
            From returnValue In Expression.Optional()
            Select DirectCast(New ReturnStatement(keyword.Span, returnValue), Statement)

        AssignmentStatement.Rule =
            From id In ReferenceIdentifier
            From _eq In EqualSymbol
            From _lc In LineContinuation
            From value In Expression
            Select DirectCast(New AssignmentStatement(id, value), Statement)

        ExpressionStatement.Rule =
            From exp In NewArrayExpression Or CallExpression
            Select DirectCast(New ExpressionStatement(exp), Statement)

        IfThenStatement.Rule =
            From _if In IfKeyword
            From condition In Expression
            From _then In ThenKeyword
            From trueStatement In SingleLineStatement
            From elsePart In (
                From _else In ElseKeyword
                From elseStatement In SingleLineStatement
                Select elseStatement).Optional
            Select DirectCast(New IfThenStatement(_if.Span, condition, trueStatement, elsePart), Statement)

        Dim ElseIfBlock =
            From _elseif In ElseIfKeyword
            From condition In Expression
            From _st In ST
            From elseIfTruePart In Statements
            Select New ElseIfBlock(_elseif.Span, condition, elseIfTruePart)

        Dim ElseBlock =
            From _else In ElseKeyword
            From _st In ST
            From elsePart In Statements
            Select New ElseBlock(_else.Span, elsePart)

        IfBlockStatement.Rule =
            From _if In IfKeyword
            From condition In Expression
            From _st1 In ST
            From truePart In Statements
            From elseIfBlocks In ElseIfBlock.Many
            From elseBlockOpt In ElseBlock.Optional
            From _end In EndKeyword
            From _st2 In ST
            Select DirectCast(New IfBlockStatement(_if.Span, _end.Span, condition, truePart, elseIfBlocks, elseBlockOpt), Statement)

        Dim DoLoopForm =
            From _do In DoKeyword
            From _st In ST
            From loopBody In Statements
            From _loop In LoopKeyword
            From _st2 In ST
            Select DoLoopStatement.DoLoopFrom(_do.Span, _loop.Span, loopBody)

        Dim DoWhileLoopForm =
            From _do In DoKeyword
            From _while In WhileKeyword
            From condition In Expression
            From _st In ST
            From loopBody In Statements
            From _loop In LoopKeyword
            From _st2 In ST
            Select DoLoopStatement.DoWhileLoopFrom(_do.Span, _while.Span, _loop.Span, condition, loopBody)

        Dim DoUntilLoopForm =
            From _do In DoKeyword
            From _until In UntilKeyword
            From condition In Expression
            From _st In ST
            From loopBody In Statements
            From _loop In LoopKeyword
            From _st2 In ST
            Select DoLoopStatement.DoUntilLoopFrom(_do.Span, _until.Span, _loop.Span, condition, loopBody)

        Dim DoLoopWhileForm =
            From _do In DoKeyword
            From _st In ST
            From loopBody In Statements
            From _loop In LoopKeyword
            From _while In WhileKeyword
            From condition In Expression
            From _st2 In ST
            Select DoLoopStatement.DoLoopWhileFrom(_do.Span, _while.Span, _loop.Span, condition, loopBody)

        Dim DoLoopUntilForm =
            From _do In DoKeyword
            From _st In ST
            From loopBody In Statements
            From _loop In LoopKeyword
            From _until In UntilKeyword
            From condition In Expression
            From _st2 In ST
            Select DoLoopStatement.DoLoopUntilFrom(_do.Span, _until.Span, _loop.Span, condition, loopBody)

        DoStatement.Rule =
            DoLoopForm Or
            DoWhileLoopForm Or
            DoUntilLoopForm Or
            DoLoopWhileForm Or
            DoLoopUntilForm

        '=======================================================================
        ' Expressions
        '=======================================================================
        IntegerLiteralExpression.Rule =
            From literal In IntegerLiteral
            Select New IntegerLiteralExpression(literal).ToExpression

        FloatLiteralExpression.Rule =
            From literal In FloatLiteral
            Select New FloatLiteralExpression(literal).ToExpression

        NumericLiteralExpression.Rule =
            IntegerLiteralExpression Or FloatLiteralExpression

        BooleanLiteralExpression.Rule =
            From literal In (TrueKeyword.AsTerminal() Or FalseKeyword.AsTerminal())
            Select New BooleanLiteralExpression(literal).ToExpression

        ReferenceExpression.Rule =
            From id In ReferenceIdentifier
            Select New ReferenceExpression(id).ToExpression

        NewArrayExpression.Rule =
            From _new In NewKeyword
            From type In TypeName
            From _lbk In LeftBrck
            From _lc In LineContinuation
            From length In Expression
            From _lc2 In LineContinuation
            From _rbk In RightBrck
            Select New NewArrayExpression(_new.Span, _lbk.Span, _rbk.Span, type, length).ToExpression

        PrimaryExpression.Rule =
            NumericLiteralExpression Or
            BooleanLiteralExpression Or
            ReferenceExpression Or
            NewArrayExpression Or
            Expression.PackedBy(LeftPth, RightPth)

        CallExpression.Rule =
            From callable In PrimaryExpression
            From _lph In LeftPth
            From _lc In LineContinuation
            From arguments In ArgumentList
            From _lc2 In LineContinuation
            From _rph In RightPth
            Select New CallExpression(callable, arguments).ToExpression()

        BracketExpression.Rule =
            From indexable In PrimaryExpression
            From _lbk In LeftBrck
            From _lc In LineContinuation
            From arguments In ArgumentList
            From _lc2 In LineContinuation
            From _rbk In RightBrck
            Select New BracketExpression(indexable, arguments).ToExpression()

        Dim basicExpression = PrimaryExpression Or CallExpression Or BracketExpression

        ' unary expressions
        UnaryExpression.Rule =
            basicExpression Or
            (From op In MinusSymbol
            From exp In UnaryExpression
            Select New UnaryExpression(op.Span, ExpressionOp.Minus, exp).ToExpression) Or
            (From op In PlusSymbol
            From exp In UnaryExpression
            Select New UnaryExpression(op.Span, ExpressionOp.Plus, exp).ToExpression) Or
            (From op In NotKeyword
            From exp In UnaryExpression
            Select New UnaryExpression(op.Span, ExpressionOp.Not, exp).ToExpression)

        'binary expressions
        FactorExpression.Rule = UnaryExpression

        TermExpression.Rule =
            FactorExpression Or
            (From left In TermExpression
            From op In Asterisk
            From _lc In LineContinuation
            From right In FactorExpression
            Select New BinaryExpression(ExpressionOp.Multiplication, left, right).ToExpression()) Or
            (From left In TermExpression
            From op In Slash
            From _lc In LineContinuation
            From right In FactorExpression
            Select New BinaryExpression(ExpressionOp.Divition, left, right).ToExpression()) Or
            (From left In TermExpression
            From op In ModKeyword
            From _lc In LineContinuation
            From right In FactorExpression
            Select New BinaryExpression(ExpressionOp.Modulo, left, right).ToExpression())

        ShiftingExpression.Rule =
            TermExpression Or
            (From left In ShiftingExpression
            From op In PlusSymbol
            From _lc In LineContinuation
            From right In TermExpression
            Select New BinaryExpression(ExpressionOp.Addition, left, right).ToExpression()) Or
            (From left In ShiftingExpression
            From op In MinusSymbol
            From _lc In LineContinuation
            From right In TermExpression
            Select New BinaryExpression(ExpressionOp.Minus, left, right).ToExpression())

        ComparandExpression.Rule =
            ShiftingExpression Or
            (From left In ComparandExpression
            From op In ShiftLeft
            From _lc In LineContinuation
            From right In ShiftingExpression
            Select New BinaryExpression(ExpressionOp.ShiftLeft, left, right).ToExpression()) Or
            (From left In ComparandExpression
            From g1 In GreaterSymbol
            From g2 In GreaterSymbol Where Grammar.Check(g2.PrefixTrivia.Count = 0, ErrorCode.RightShiftSymbolError, g2.Span)
            From _lc In LineContinuation
            From right In ShiftingExpression
            Select New BinaryExpression(ExpressionOp.ShiftRight, left, right).ToExpression())

        ComparisonExpression.Rule =
            ComparandExpression Or
            (From left In ComparisonExpression
            From op In GreaterSymbol
            From _lc In LineContinuation
            From right In ComparandExpression
            Select New BinaryExpression(ExpressionOp.Greater, left, right).ToExpression()) Or
            (From left In ComparisonExpression
            From op In GreaterEqual
            From _lc In LineContinuation
            From right In ComparandExpression
            Select New BinaryExpression(ExpressionOp.GreaterEqual, left, right).ToExpression()) Or
            (From left In ComparisonExpression
            From op In LessSymbol
            From _lc In LineContinuation
            From right In ComparandExpression
            Select New BinaryExpression(ExpressionOp.Less, left, right).ToExpression()) Or
            (From left In ComparisonExpression
            From op In LessEqual
            From _lc In LineContinuation
            From right In ComparandExpression
            Select New BinaryExpression(ExpressionOp.LessEqual, left, right).ToExpression())

        EqualityExpression.Rule =
            ComparisonExpression Or
            (From left In EqualityExpression
            From op In EqualSymbol
            From _lc In LineContinuation
            From right In ComparisonExpression
            Select New BinaryExpression(ExpressionOp.Equal, left, right).ToExpression()) Or
            (From left In EqualityExpression
            From op In NotEqual
            From _lc In LineContinuation
            From right In ComparisonExpression
            Select New BinaryExpression(ExpressionOp.NotEqual, left, right).ToExpression())

        AndExpression.Rule =
            EqualityExpression Or
            From left In AndExpression
            From op In AndKeyword
            From _lc In LineContinuation
            From right In EqualityExpression
            Select New BinaryExpression(ExpressionOp.And, left, right).ToExpression()

        XorExpression.Rule =
            AndExpression Or
            From left In XorExpression
            From op In XorKeyword
            From _lc In LineContinuation
            From right In AndExpression
            Select New BinaryExpression(ExpressionOp.Xor, left, right).ToExpression()

        OrExpression.Rule =
            XorExpression Or
            From left In OrExpression
            From op In OrKeyword
            From _lc In LineContinuation
            From right In XorExpression
            Select New BinaryExpression(ExpressionOp.Or, left, right).ToExpression()

        Expression.Rule = OrExpression

        ArgumentList.Rule =
            From list In Expression.Many(Comma.Concat(LineContinuation))
            Select New ArgumentList(list.Select(Function(exp) New Argument(exp)))

        ScannerInfo.CurrentLexerIndex = m_keywordLexerIndex

        'Return From c In Parsers.Any.Many() Select New SyntaxTreeNode(c)
        Return Program
    End Function

    Private m_scannerCreationTime As Long
    Public ReadOnly Property ScannerCreationTime() As Long
        Get
            Return m_scannerCreationTime
        End Get
    End Property


    Protected Overrides Function OnCreateScannerInfo() As ScannerInfo
        Dim sw As New Stopwatch()

        sw.Start()
        Dim scannerInfo = MyBase.OnCreateScannerInfo()
        sw.Stop()

        m_scannerCreationTime = sw.ElapsedMilliseconds

        Return scannerInfo
    End Function

    Private m_parserCreationTime As Long
    Public ReadOnly Property ParserCreationTime() As Long
        Get
            Return m_parserCreationTime
        End Get
    End Property

    Protected Overrides Function OnCreateTransitionTable() As Parsers.Generator.TransitionTable
        Dim sw As New Stopwatch()

        sw.Start()
        Dim transitionTable = MyBase.OnCreateTransitionTable()
        sw.Stop()

        m_parserCreationTime = sw.ElapsedMilliseconds

        Return transitionTable
    End Function
End Class
