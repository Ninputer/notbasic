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
    Private TrueKeyword As Token
    Private FalseKeyword As Token
    Private NewKeyword As Token
    Private OperatorKeyword As Token
    Private FunctionKeyword As Token
    Private ReturnKeyword As Token
    Private ExitKeyword As Token
    Private TryKeyword As Token
    Private CatchKeyword As Token
    Private FinallyKeyword As Token
    Private ThrowKeyword As Token
    Private EachKeyword As Token
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
    Private ObjectKeyword As Token
    Private AndKeyword As Token
    Private OrKeyword As Token
    Private NotKeyword As Token
    Private XorKeyword As Token
    Private ModKeyword As Token
    Private CastKeyword As Token
    Private ConceptKeyword As Token
    Private ConcreteKeyword As Token
    Private WhereKeyword As Token
    Private TypeKeyword As Token
    Private ToKeyword As Token
    Private StepKeyword As Token
    Private InKeyword As Token
    Private EnumKeyword As Token

    'contextural keywords
    Private GetKeyword As Token
    Private SetKeyword As Token
    Private DeclareKeyword As Token

    Private Identifier As Token
    Private EscapedIdentifier As Token

    Private IntegerLiteral As Token
    Private FloatLiteral As Token
    Private RawStringLiteral As Token
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
    Private ShiftLeft As Token '<<
    Private Dot As Token '.
    Private Arrow As Token '=>

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

        'lexer for property body (get/set proc)
        Dim propertyLexer = keywordLexer.CreateSubLexer()

        m_identifierLexerIndex = identifierLexer.Index
        m_keywordLexerIndex = keywordLexer.Index
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
        charSetBuilder.DefineCharSet(Function(c) (c <> """"c) AndAlso (Not lineTerminators.Contains(c)), Sub(re) strictStringChar = re Or Literal(""""""))

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

            Dim stringChar = strictStringChar Or lineTerminatorChar

            RawStringLiteral = .DefineToken(
                doubleQuote >> stringChar.Many() >> doubleQuote,
                "string literal")


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
            ShiftLeft = .DefineToken(Literal("<<"))
            Dot = .DefineToken(Symbol("."c))
            Arrow = .DefineToken(Literal("=>"))

            LineTerminator = .DefineToken(lineTerminatorChar Or Literal(vbCrLf), "line terminator")
        End With

        '========== define trivias ========== 
        With identifierLexer
            WhiteSpace = .DefineToken(whitespaceChar.Many1(), "white space")
            Comment = .DefineToken((Symbol("'"c) >> inputChar.Many()), "comment")
        End With

        '========== Define reserved keywords ========== 
        With keywordLexer
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
            TrueKeyword = .DefineToken(Literal("true"))
            FalseKeyword = .DefineToken(Literal("false"))
            NewKeyword = .DefineToken(Literal("new"))
            OperatorKeyword = .DefineToken(Literal("operator"))
            FunctionKeyword = .DefineToken(Literal("fun"))
            ReturnKeyword = .DefineToken(Literal("return"))
            ExitKeyword = .DefineToken(Literal("exit"))
            TryKeyword = .DefineToken(Literal("try"))
            CatchKeyword = .DefineToken(Literal("catch"))
            FinallyKeyword = .DefineToken(Literal("finally"))
            ThrowKeyword = .DefineToken(Literal("throw"))
            EachKeyword = .DefineToken(Literal("each"))
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
            ObjectKeyword = .DefineToken(Literal("object"))
            AndKeyword = .DefineToken(Literal("and"))
            OrKeyword = .DefineToken(Literal("or"))
            NotKeyword = .DefineToken(Literal("not"))
            XorKeyword = .DefineToken(Literal("xor"))
            ModKeyword = .DefineToken(Literal("mod"))
            CastKeyword = .DefineToken(Literal("cast"))
            ConceptKeyword = .DefineToken(Literal("concept"))
            ConcreteKeyword = .DefineToken(Literal("concrete"))
            WhereKeyword = .DefineToken(Literal("where"))
            TypeKeyword = .DefineToken(Literal("type"))
            ToKeyword = .DefineToken(Literal("to"))
            StepKeyword = .DefineToken(Literal("step"))
            InKeyword = .DefineToken(Literal("in"))
            EnumKeyword = .DefineToken(Literal("enum"))
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
    Private TypeSpecifier As New Production(Of TypeSpecifier)
    Private TypeParameter As New Production(Of TypeParameter)
    Private TypeParameters As New Production(Of IEnumerable(Of TypeParameter))
    Private TypeArguments As New Production(Of IEnumerable(Of TypeName))
    Private FunctionTypeName As New Production(Of TypeName)

    Private Program As New Production(Of CompilationUnit)
    Private TopLevelStructure As New Production(Of Definition)

    Private TypeDefinition As New Production(Of TypeDefinition)
    Private FieldDefinition As New Production(Of FieldDefinition)

    Private EnumDefinition As New Production(Of EnumDefinition)

    Private ParameterList As New Production(Of IEnumerable(Of ParameterDeclaration))
    Private ParameterDeclaration As New Production(Of ParameterDeclaration)
    Private FunctionSignature As New Production(Of FunctionSignature)
    Private FunctionDefinition As New Production(Of FunctionDefinition)
    Private LambdaParameterList As New Production(Of IEnumerable(Of ParameterDeclaration))
    Private LambdaParameterDeclaration As New Production(Of ParameterDeclaration)

    Private OperatorSignature As New Production(Of OperatorSignature)
    Private OperatorDefinition As New Production(Of OperatorDefinition)
    Private ShiftRightOperator As New Production(Of LexemeValue)
    Private NotEqualOperator As New Production(Of LexemeValue)
    Private OverloadableOperator As New Production(Of LexemeValue)

    Private ConceptDeclaration As New Production(Of ConceptDeclaration)
    Private ConceptDefinition As New Production(Of ConceptDefinition)
    Private ConstraintClauses As New Production(Of IEnumerable(Of ConstraintClause))
    Private ConceptConstraintClause As New Production(Of ConstraintClause)
    Private TypeConstraintClause As New Production(Of ConstraintClause)
    Private ConcreteDeclaration As New Production(Of ConcreteDeclaration)
    Private ConcreteDefinition As New Production(Of ConcreteDefinition)
    Private ProcedureDeclaration As New Production(Of ProcedureDeclaration)
    Private ProcedureDefinition As New Production(Of Definition)

    Private Statements As New Production(Of IEnumerable(Of Statement))
    Private Statement As New Production(Of Statement)
    Private SingleLineStatement As New Production(Of Statement)
    Private SingleLineOpenStatement As New Production(Of Statement)
    Private SingleLineClosedStatement As New Production(Of Statement)
    Private BlockStatement As New Production(Of Statement)
    Private StatementsBlock As New Production(Of IEnumerable(Of Statement))

    Private ReturnStatement As New Production(Of Statement)
    Private AssignmentStatement As New Production(Of Statement)
    Private ExpressionStatement As New Production(Of Statement)
    Private CallStatement As New Production(Of Statement)
    Private IfThenStatement As New Production(Of Statement)
    Private IfThenElseOpenStatement As New Production(Of Statement)
    Private IfThenElseClosedStatement As New Production(Of Statement)
    Private IfBlockStatement As New Production(Of Statement)
    Private DoStatement As New Production(Of Statement)
    Private TryStatement As New Production(Of Statement)
    Private ForStatement As New Production(Of Statement)
    Private ForEachStatement As New Production(Of Statement)
    Private ExitStatement As New Production(Of Statement)
    Private ContinueStatement As New Production(Of Statement)
    Private SelectCaseStatement As New Production(Of Statement)

    Private Expression As New Production(Of Expression)
    Private PrimaryExpression As New Production(Of Expression)
    Private PrimaryExpressionNonNewArray As New Production(Of Expression)
    Private FactorExpression As New Production(Of Expression)
    Private TermExpression As New Production(Of Expression)
    Private ComparandExpression As New Production(Of Expression)
    Private ComparisonExpression As New Production(Of Expression)
    Private EqualityExpression As New Production(Of Expression)
    Private AndExpression As New Production(Of Expression)
    Private OrExpression As New Production(Of Expression)
    Private XorExpression As New Production(Of Expression)
    Private ShiftingExpression As New Production(Of Expression)
    Private CastExpression As New Production(Of Expression)
    Private UnaryExpression As New Production(Of Expression)
    Private IntegerLiteralExpression As New Production(Of Expression)
    Private FloatLiteralExpression As New Production(Of Expression)
    Private NumericLiteralExpression As New Production(Of Expression)
    Private BooleanLiteralExpression As New Production(Of Expression)
    Private StringLiteralExpression As New Production(Of Expression)
    Private CharLiteralExpression As New Production(Of Expression)
    Private NothingExpression As New Production(Of Expression)
    Private NewArrayExpression As New Production(Of Expression)
    Private ReferenceExpression As New Production(Of Expression)
    Private MemberAccessExpression As New Production(Of Expression)
    Private BracketExpression As New Production(Of Expression)
    Private CallExpression As New Production(Of Expression)
    Private LambdaExpression As New Production(Of Expression)
    Private LambdaBody As New Production(Of LambdaBody)
    Private LambdaSignature As New Production(Of LambdaSignature)
    Private ArrayLiteralExpression As New Production(Of Expression)

    Private ArgumentList As New Production(Of IEnumerable(Of Argument))

    Protected Overrides Sub OnDefineParserErrors(errorDefinition As SyntaxErrors, errorManager As CompilationErrorManager)
        With errorDefinition
            .LexicalErrorId = ErrorCode.InvalidToken
            .TokenMissingId = ErrorCode.MissingToken
            .TokenUnexpectedId = ErrorCode.UnexpectedToken
            .OtherErrorId = ErrorCode.GeneralSyntaxError
        End With

        MyBase.OnDefineParserErrors(errorDefinition, errorManager)

        errorManager.DefineError(ErrorCode.RightShiftSymbolError, 0, CompilationStage.Parsing, "Spaces between >> operator are not allowed")
        errorManager.DefineError(ErrorCode.NotEqualSymbolError, 0, CompilationStage.Parsing, "Spaces between <> operator are not allowed")
        errorManager.DefineError(ErrorCode.CaseIsNotAllowedAfterCaseElse, 0, CompilationStage.Parsing, "'case' clause is not allowed after 'case else'")
        errorManager.DefineError(ErrorCode.CaseElseCanHaveOnlyOne, 0, CompilationStage.Parsing, "Only one 'case else' clause is allowed")
        errorManager.DefineError(ErrorCode.ExpressionExpected, 0, CompilationStage.Parsing, "Expression expected")
    End Sub

    Protected Overrides Function OnDefineGrammar() As ProductionBase(Of CompilationUnit)
        'DONE: concept/concret definition
        'DONE: user defined type
        'DONE: for/foreach statement
        'DONE: try/catch statement
        'DONE: lambda expression/function type
        'DONE: select case statement
        'DONE: if then else statement ambiguity gramma
        'DONE: String literals
        'DONE: array access ambiguity gramma
        'DONE: enum
        'DONE: type inheritence
        'DONE: dispatch method
        'DONE: concept default implementation
        'DONE: array literal
        'DONE: array type specifier
        'DONE: type constraint clause
        'DONE: object type
        'DONE: nothing literal
        'TODO: new object expression?

        StatementTerminator.Rule =
            From terminator In (LineTerminator.AsTerminal() Or Semicolon.AsTerminal())
            Select terminator.Value.Span

        Dim LineContinuation = LineTerminator.Optional()

        'Statement terminator
        Dim ST = StatementTerminator.Many1()

        '=======================================================================
        ' Basic Structures
        '=======================================================================

        DeclaringIdentifier.Rule =
            (From id In Identifier
            Select New UnifiedIdentifer(id.Value, False)) Or
            (From eid In EscapedIdentifier
            Select New UnifiedIdentifer(eid.Value, True))

        ReferenceIdentifier.Rule =
            DeclaringIdentifier

        QualifiedIdentifier.Rule =
            (From id In Identifier.AsTerminal()
             Select New UnifiedIdentifer(id.Value, False)) Or
            (From eid In EscapedIdentifier
             Select New UnifiedIdentifer(eid.Value, True))

        ArrayTypeName.Rule =
            From baseType In TypeName
            From _lbk In LeftBrck
            From _rbk In RightBrck
            Select DirectCast(New ArrayTypeName(baseType), TypeName)

        TypeName.Rule =
            QualifiedTypeName Or
            PrimitiveTypeName Or
            FunctionTypeName Or
            ArrayTypeName

        QualifiedTypeName.Rule =
            From id In ReferenceIdentifier
            From typeArgs In TypeArguments.Optional
            Select DirectCast(New QualifiedTypeName(id, typeArgs), TypeName)

        '<Type, Type,...>
        TypeArguments.Rule =
            From _lt In LessSymbol
            From _lc1 In LineContinuation
            From types In TypeName.Many1(Comma.AsTerminal().SuffixedBy(LineContinuation))
            From _lc2 In LineContinuation
            From _gt In GreaterSymbol
            Select types

        Dim typeParameterDimension =
            From _lt In LessSymbol
            From commas In Comma.AsTerminal().Many()
            From _gt In GreaterSymbol
            Select New Nullable(Of Integer)(commas.Count() + 1)

        TypeParameter.Rule =
            From name In DeclaringIdentifier
            From dimension In typeParameterDimension.Optional
            Select New TypeParameter(name, dimension)

        '<identifier, identifier<>>
        TypeParameters.Rule =
            From _lt In LessSymbol
            From _lc1 In LineContinuation
            From typeParams In TypeParameter.Many1(Comma.AsTerminal().SuffixedBy(LineContinuation))
            From _lc2 In LineContinuation
            From _gt In GreaterSymbol
            Select typeParams

        PrimitiveTypeName.Rule =
            From typeKeyword In Grammar.Union(IntKeyword,
                                              BoolKeyword,
                                              SingleKeyword,
                                              DoubleKeyword,
                                              ShortKeyword,
                                              ByteKeyword,
                                              LongKeyword,
                                              CharKeyword,
                                              StringKeyword,
                                              ObjectKeyword)
            Select DirectCast(New PrimitiveTypeName(typeKeyword.Value), TypeName)

        'fun(paramType)returnType
        FunctionTypeName.Rule =
            From _fun In FunctionKeyword
            From _lpth In LeftPth
            From _lc1 In LineContinuation
            From paramTypes In TypeName.Many(Comma.AsTerminal().SuffixedBy(LineContinuation))
            From _lc2 In LineContinuation
            From _rpth In RightPth
            From returnType In TypeName.Optional
            Select DirectCast(New FunctionTypeName(_fun.Value.Span, paramTypes, returnType), TypeName)


        '=======================================================================
        ' Program Entry
        '=======================================================================

        TopLevelStructure.Rule =
            FunctionDefinition.Select(Function(d) d.ToBase()) Or
            OperatorDefinition.Select(Function(d) d.ToBase()) Or
            ConceptDefinition.Select(Function(d) d.ToBase()) Or
            ConcreteDefinition.Select(Function(d) d.ToBase()) Or
            TypeDefinition.Select(Function(d) d.ToBase()) Or
            EnumDefinition.Select(Function(d) d.ToBase())

        Program.Rule =
            From _emptylines In StatementTerminator.Many
            From definitions In TopLevelStructure.Many()
            Select New CompilationUnit(definitions)

        '=======================================================================
        ' Types
        '=======================================================================

        FieldDefinition.Rule =
            From fieldName In DeclaringIdentifier
            From typeSp In TypeSpecifier
            From _st In ST
            Select New FieldDefinition(fieldName, typeSp)

        TypeDefinition.Rule =
            From _type In TypeKeyword
            From typeName In DeclaringIdentifier
            From typeParams In TypeParameters.Optional
            From baseType In TypeSpecifier.Optional
            From whereClauses In ConstraintClauses.Optional
            From _st1 In ST
            From fields In FieldDefinition.Many
            From _end In EndKeyword
            From _st2 In ST
            Select New TypeDefinition(_type.Value.Span, _end.Value.Span, typeName, typeParams, baseType, whereClauses, fields)

        Dim EnumElementValue =
            From _eq In EqualSymbol
            From num In IntegerLiteral
            Select num.Value

        Dim EnumElement =
            From elementName In DeclaringIdentifier
            From elementValue In EnumElementValue.Optional
            Select New EnumElement(elementName, elementValue)

        EnumDefinition.Rule =
            From _enum In EnumKeyword
            From enumName In DeclaringIdentifier
            From _st1 In ST
            From elements In EnumElement.SuffixedBy(ST).Many1
            From _end In EndKeyword
            From _st2 In ST
            Select New EnumDefinition(_enum.Value.Span, _end.Value.Span, enumName, elements)

        '=======================================================================
        ' Functions
        '=======================================================================

        ParameterList.Rule =
           ParameterDeclaration.Many(Comma.AsTerminal().SuffixedBy(LineContinuation))

        Dim ParamPrefix =
            From prefix In Grammar.Union(SelectKeyword, CaseKeyword)
            Select prefix.Value

        ParameterDeclaration.Rule =
            From prefix In ParamPrefix.Optional
            From did In DeclaringIdentifier
            From typesp In TypeSpecifier.Optional()
            Select New ParameterDeclaration(did, typesp, If(prefix IsNot Nothing, New ParameterPrefix(prefix), Nothing))

        LambdaParameterList.Rule =
           LambdaParameterDeclaration.Many(Comma.AsTerminal().SuffixedBy(LineContinuation))

        LambdaParameterDeclaration.Rule =
            From did In DeclaringIdentifier
            From typesp In TypeSpecifier.Optional()
            Select New ParameterDeclaration(did, typesp)

        TypeSpecifier.Rule =
            From _colon In Colon
            From _nl In LineContinuation
            From spTypeName In TypeName
            Select New TypeSpecifier(spTypeName)

        'FunctionDeclaration := fun name ( arglist ) <st>
        FunctionSignature.Rule =
            From keyword In FunctionKeyword
            From name In DeclaringIdentifier
            From typeParams In TypeParameters.Optional
            From _lpth In LeftPth
            From _nl1 In LineContinuation
            From paramlist In ParameterList
            From _nl2 In LineContinuation
            From _rpth In RightPth
            From returnTypeSp In TypeSpecifier.Optional()
            From whereClauses In ConstraintClauses.Optional()
            From _st In ST
            Select New FunctionSignature(keyword.Value.Span, name, paramlist, returnTypeSp, typeParams, whereClauses)

        FunctionDefinition.Rule =
            From decl In FunctionSignature
            From ss In Statements
            From endfun In EndKeyword
            From _st2 In ST
            Select New FunctionDefinition(decl, ss, endfun.Value.Span)

        '=======================================================================
        ' Operators
        '=======================================================================

        OverloadableOperator.Rule =
            NotEqualOperator Or ShiftRightOperator Or
            From op In Grammar.Union(MinusSymbol, PlusSymbol, NotKeyword, CastKeyword,
                                     Asterisk, Slash, ModKeyword, ShiftLeft,
                                     GreaterSymbol, GreaterEqual, LessSymbol, LessEqual, EqualSymbol,
                                     AndKeyword, XorKeyword, OrKeyword)
            Select op.Value

        'OperatorDeclaration := operator op ( arglist ) <st>
        OperatorSignature.Rule =
            From _operator In OperatorKeyword
            From op In OverloadableOperator
            From typeParams In TypeParameters.Optional
            From _lpth In LeftPth
            From _nl1 In LineContinuation
            From paramlist In ParameterList
            From _nl2 In LineContinuation
            From _rpth In RightPth
            From returnTypeSp In TypeSpecifier.Optional()
            From whereClauses In ConstraintClauses.Optional()
            From _st In ST
            Select New OperatorSignature(_operator.Value.Span, op, paramlist, returnTypeSp, typeParams, whereClauses)

        OperatorDefinition.Rule =
            From decl In OperatorSignature
            From statements In statements
            From endfun In EndKeyword
            From _st In ST
            Select New OperatorDefinition(decl, statements, endfun.Value.Span)

        '=======================================================================
        ' Concepts
        '=======================================================================

        ConceptDeclaration.Rule =
            From _concept In ConceptKeyword
            From name In DeclaringIdentifier
            From typeParams In TypeParameters
            From whereClauses In ConstraintClauses.Optional
            From _st In ST
            Select New ConceptDeclaration(_concept.Value.Span, name, typeParams, whereClauses)

        Dim ConstraintClause = ConceptConstraintClause Or TypeConstraintClause

        ConstraintClauses.Rule =
            From _where In WhereKeyword
            From _lc In LineContinuation
            From constraints In ConstraintClause.Many1(Comma.AsTerminal().SuffixedBy(LineContinuation))
            Select constraints

        ConceptConstraintClause.Rule =
            From conceptName In ReferenceIdentifier
            From typeArgs In TypeArguments
            Select New ConceptConstraintClause(conceptName, typeArgs).ToBase

        '(A,B => C,D)
        TypeConstraintClause.Rule =
            From _lph In LeftPth
            From _lc1 In LineContinuation
            From leftTypes In ReferenceIdentifier.Many1(Comma.Concat(LineContinuation))
            From _arrow In Arrow
            From _lc2 In LineContinuation
            From rightTypes In ReferenceIdentifier.Many1(Comma.Concat(LineContinuation))
            From _rph In RightPth
            Select New TypeConstraintClause(leftTypes, rightTypes).ToBase

        ConcreteDeclaration.Rule =
            From _concrete In ConcreteKeyword
            From typeParams In TypeParameters.Optional
            From conceptName In ReferenceIdentifier
            From typeArgs In TypeArguments
            From whereClauses In ConstraintClauses.Optional
            From _st In ST
            Select New ConcreteDeclaration(_concrete.Value.Span, typeParams, conceptName, typeArgs, whereClauses)

        Dim ProcedureDeclarationOrDefinition =
            ProcedureDeclaration.Select(Function(d) d.ToBase) Or
            ProcedureDefinition

        ConceptDefinition.Rule =
            From decl In ConceptDeclaration
            From procedures In ProcedureDeclarationOrDefinition.Many()
            From _end In EndKeyword
            From _st In ST
            Select New ConceptDefinition(decl, procedures, _end.Value.Span)

        Dim ProcedureSignature =
            FunctionSignature.Select(Function(s) s.ToBase) Or
            OperatorSignature.Select(Function(s) s.ToBase)

        ProcedureDeclaration.Rule =
            From _declare In DeclareKeyword
            From signature In ProcedureSignature
            Select New ProcedureDeclaration(_declare.Value.Span, signature)

        ProcedureDefinition.Rule =
            FunctionDefinition.Select(Function(d) d.ToBase()) Or
            OperatorDefinition.Select(Function(d) d.ToBase())

        ConcreteDefinition.Rule =
            From decl In ConcreteDeclaration
            From procedures In ProcedureDefinition.Many()
            From _end In EndKeyword
            From _st In ST
            Select New ConcreteDefinition(decl, procedures, _end.Value.Span)

        '=======================================================================
        ' Statements
        '=======================================================================

        Statements.Rule =
            Statement.Many1(ST).SuffixedBy(ST) Or
            Grammar.Empty(Of IEnumerable(Of Statement))(Nothing)

        StatementsBlock.Rule =
            From _lbr In LeftBrce
            From _st1 In ST.Optional()
            From s In Statement.Many(ST)
            From _st2 In ST.Optional()
            From _rbr In RightBrce
            Select s

        Statement.Rule =
            SingleLineStatement Or
            BlockStatement

        SingleLineStatement.Rule =
            SingleLineOpenStatement Or
            SingleLineClosedStatement

        SingleLineClosedStatement.Rule =
            ReturnStatement Or
            AssignmentStatement Or
            IfThenElseClosedStatement Or
            ExpressionStatement Or
            ContinueStatement Or
            ExitStatement

        SingleLineOpenStatement.Rule =
            IfThenStatement Or
            IfThenElseOpenStatement

        BlockStatement.Rule =
            IfBlockStatement Or
            DoStatement Or
            ForStatement Or
            ForEachStatement Or
            TryStatement Or
            SelectCaseStatement

        ReturnStatement.Rule =
            From keyword In ReturnKeyword
            From _lc In LineContinuation
            From returnValue In Expression.Optional()
            Select New ReturnStatement(keyword.Value.Span, returnValue).ToBase

        AssignmentStatement.Rule =
            From id In ReferenceIdentifier
            From _eq In EqualSymbol
            From _lc In LineContinuation
            From value In Expression
            Select New AssignmentStatement(id, value).ToBase

        ExpressionStatement.Rule =
            From exp In CallExpression
            Select New ExpressionStatement(exp).ToBase

        IfThenStatement.Rule =
            From _if In IfKeyword
            From condition In Expression
            From _then In ThenKeyword
            From trueStatement In SingleLineStatement
            Select New IfThenStatement(_if.Value.Span, condition, trueStatement, Nothing).ToBase

        IfThenElseOpenStatement.Rule =
            From _if In IfKeyword
            From condition In Expression
            From _then In ThenKeyword
            From trueStatement In SingleLineClosedStatement
            From elsePart In (
                From _else In ElseKeyword
                From elseStatement In SingleLineOpenStatement
                Select elseStatement)
            Select New IfThenStatement(_if.Value.Span, condition, trueStatement, elsePart).ToBase

        IfThenElseClosedStatement.Rule =
            From _if In IfKeyword
            From condition In Expression
            From _then In ThenKeyword
            From trueStatement In SingleLineClosedStatement
            From elsePart In (
                From _else In ElseKeyword
                From elseStatement In SingleLineClosedStatement
                Select elseStatement)
            Select New IfThenStatement(_if.Value.Span, condition, trueStatement, elsePart).ToBase

        Dim ElseIfBlock =
            From _elseif In ElseIfKeyword
            From condition In Expression
            From _st In ST
            From elseIfTruePart In Statements
            Select New ElseIfBlock(_elseif.Value.Span, condition, elseIfTruePart)

        Dim ElseBlock =
            From _else In ElseKeyword
            From _st In ST
            From elsePart In Statements
            Select New ElseBlock(_else.Value.Span, elsePart)

        IfBlockStatement.Rule =
            From _if In IfKeyword
            From condition In Expression
            From _st1 In ST
            From truePart In Statements
            From elseIfBlocks In ElseIfBlock.Many
            From elseBlockOpt In ElseBlock.Optional
            From _end In EndKeyword
            Select New IfBlockStatement(_if.Value.Span, _end.Value.Span, condition, truePart, elseIfBlocks, elseBlockOpt).ToBase

        Dim DoLoopForm =
            From _do In DoKeyword
            From _st In ST
            From loopBody In Statements
            From _loop In LoopKeyword
            Select DoLoopStatement.DoLoopFrom(_do.Value.Span, _loop.Value.Span, loopBody)

        Dim DoWhileLoopForm =
            From _do In DoKeyword
            From _while In WhileKeyword
            From condition In Expression
            From _st In ST
            From loopBody In Statements
            From _loop In LoopKeyword
            Select DoLoopStatement.DoWhileLoopFrom(_do.Value.Span, _while.Value.Span, _loop.Value.Span, condition, loopBody)

        Dim DoUntilLoopForm =
            From _do In DoKeyword
            From _until In UntilKeyword
            From condition In Expression
            From _st In ST
            From loopBody In Statements
            From _loop In LoopKeyword
            Select DoLoopStatement.DoUntilLoopFrom(_do.Value.Span, _until.Value.Span, _loop.Value.Span, condition, loopBody)

        Dim DoLoopWhileForm =
            From _do In DoKeyword
            From _st In ST
            From loopBody In Statements
            From _loop In LoopKeyword
            From _while In WhileKeyword
            From condition In Expression
            Select DoLoopStatement.DoLoopWhileFrom(_do.Value.Span, _while.Value.Span, _loop.Value.Span, condition, loopBody)

        Dim DoLoopUntilForm =
            From _do In DoKeyword
            From _st In ST
            From loopBody In Statements
            From _loop In LoopKeyword
            From _until In UntilKeyword
            From condition In Expression
            Select DoLoopStatement.DoLoopUntilFrom(_do.Value.Span, _until.Value.Span, _loop.Value.Span, condition, loopBody)

        DoStatement.Rule =
            DoLoopForm Or
            DoWhileLoopForm Or
            DoUntilLoopForm Or
            DoLoopWhileForm Or
            DoLoopUntilForm

        ContinueStatement.Rule =
            From _continue In ContinueKeyword
            From loopStruct In Grammar.Union(ForKeyword, DoKeyword)
            Select New ContinueStatement(_continue.Value.Span, loopStruct.Value).ToBase

        ExitStatement.Rule =
            From _exit In ExitKeyword
            From exitStruct In Grammar.Union(ForKeyword, DoKeyword, TryKeyword, SelectKeyword, FunctionKeyword)
            Select New ExitStatement(_exit.Value.Span, exitStruct.Value).ToBase

        Dim CatchWithTypeBlock =
            From _catch In CatchKeyword
            From exceptVar In DeclaringIdentifier.Optional
            From exceptType In TypeSpecifier
            From _st In ST
            From catchBody In Statements
            Select New CatchBlock(_catch.Value.Span, exceptVar, exceptType, catchBody)

        Dim CatchBlock =
            From _catch In CatchKeyword
            From _st In ST
            From catchBody In Statements
            Select New CatchBlock(_catch.Value.Span, Nothing, Nothing, catchBody)

        Dim FinallyBlock =
            From _finally In FinallyKeyword
            From _st In ST
            From finallyBody In Statements
            Select New FinallyBlock(_finally.Value.Span, finallyBody)

        TryStatement.Rule =
            From _try In TryKeyword
            From _st1 In ST
            From tryBody In Statements
            From typeCatches In CatchWithTypeBlock.Many
            From _catchBlock In CatchBlock.Optional
            From _finallyBlock In FinallyBlock.Optional
            From _end In EndKeyword
            Select New TryStatement(_try.Value.Span, _end.Value.Span, tryBody, typeCatches, _catchBlock, _finallyBlock).ToBase

        ForStatement.Rule =
            From _for In ForKeyword
            From loopVar In DeclaringIdentifier
            From typeSp In TypeSpecifier.Optional
            From _eq In EqualSymbol
            From _lc In LineContinuation
            From fromExp In Expression
            From _to In ToKeyword
            From _lc2 In LineContinuation
            From toExp In Expression
            From stepExp In Expression.PrefixedBy(StepKeyword).Optional
            From _st1 In ST
            From forBody In Statements
            From _next In NextKeyword
            Select New ForStatement(_for.Value.Span, _next.Value.Span, loopVar, typeSp, fromExp, toExp, stepExp, forBody).ToBase

        ForEachStatement.Rule =
            From _for In ForKeyword
            From _each In EachKeyword
            From loopVar In DeclaringIdentifier
            From typeSp In TypeSpecifier.Optional
            From _in In InKeyword
            From _lc In LineContinuation
            From enumExp In Expression
            From _st1 In ST
            From forBody In Statements
            From _next In NextKeyword
            Select New ForEachStatement(_for.Value.Span, _each.Value.Span, loopVar, typeSp, enumExp, forBody).ToBase

        Dim CaseExpressionList =
            Expression.Many(Comma.Concat(LineContinuation))

        Dim CaseBlock =
            From _case In CaseKeyword
            From expList In CaseExpressionList Where Grammar.Check(expList.Count() > 0, ErrorCode.ExpressionExpected, _case.Value.Span)
            From _st In ST
            From caseBody In Statements
            Select New CaseBlock(_case.Value.Span, expList, caseBody)

        Dim CaseElseBlock =
            From _case In CaseKeyword
            From _else In ElseKeyword
            From _st In ST
            From caseBody In Statements
            Select New CaseElseBlock(_case.Value.Span, _else.Value.Span, caseBody)

        Dim ErrorCase =
            (From errCaseBlock In CaseBlock Where Grammar.Check(False, ErrorCode.CaseIsNotAllowedAfterCaseElse, errCaseBlock.CaseSpan) Select Nothing) Or
            From errCaseElseBlock In CaseElseBlock Where Grammar.Check(False, ErrorCode.CaseElseCanHaveOnlyOne, errCaseElseBlock.CaseSpan) Select Nothing


        SelectCaseStatement.Rule =
            From _select In SelectKeyword
            From selectExp In Expression.Optional
            From _st In ST
            From caseBlocks In CaseBlock.Many()
            From caseElse In CaseElseBlock.Optional()
            From errors In ErrorCase.Many()
            From _end In EndKeyword
            Select New SelectCaseStatement(_select.Value.Span, _end.Value.Span, selectExp, caseBlocks, caseElse).ToBase()

        '=======================================================================
        ' Expressions
        '=======================================================================
        IntegerLiteralExpression.Rule =
            From literal In IntegerLiteral
            Select New IntegerLiteralExpression(literal.Value).ToBase

        FloatLiteralExpression.Rule =
            From literal In FloatLiteral
            Select New FloatLiteralExpression(literal).ToBase

        NumericLiteralExpression.Rule =
            IntegerLiteralExpression Or FloatLiteralExpression

        BooleanLiteralExpression.Rule =
            From literal In (TrueKeyword.AsTerminal() Or FalseKeyword.AsTerminal())
            Select New BooleanLiteralExpression(literal.Value).ToBase

        StringLiteralExpression.Rule =
            From str In RawStringLiteral
            Select New StringLiteral(str).ToBase

        CharLiteralExpression.Rule =
            From cl In CharLiteral
            Select New CharLiteral(cl.Value).ToBase

        NothingExpression.Rule =
            From _nothing In NothingKeyword
            Select New NothingExpression(_nothing.Value.Span).ToBase

        ArrayLiteralExpression.Rule =
            From _lbk In LeftBrck
            From _lc1 In LineContinuation
            From expList In Expression.Many(Comma.Concat(LineContinuation))
            From _lc2 In LineContinuation
            From _rbk In RightBrck
            Select New ArrayLiteralExpression(expList).ToBase

        ReferenceExpression.Rule =
            From id In ReferenceIdentifier
            From typeArgs In TypeArguments.Optional
            Select New ReferenceExpression(id, typeArgs).ToBase

        MemberAccessExpression.Rule =
            From exp In PrimaryExpression
            From _dot In Dot
            From memberName In ReferenceIdentifier
            From typeArgs In TypeArguments.Optional
            Select New MemberAccessExpression(exp, memberName, typeArgs).ToBase()

        NewArrayExpression.Rule =
            From _new In NewKeyword
            From type In TypeName
            From _lbk In LeftBrck
            From _lc In LineContinuation
            From length In Expression
            From _lc2 In LineContinuation
            From _rbk In RightBrck
            Select New NewArrayExpression(_new.Value.Span, _lbk.Value.Span, _rbk.Value.Span, type, length).ToBase

        PrimaryExpression.Rule =
            NewArrayExpression Or
            PrimaryExpressionNonNewArray

        PrimaryExpressionNonNewArray.Rule =
            NumericLiteralExpression Or
            BooleanLiteralExpression Or
            StringLiteralExpression Or
            CharLiteralExpression Or
            NothingExpression Or
            ArrayLiteralExpression Or
            ReferenceExpression Or
            MemberAccessExpression Or
            Expression.PackedBy(LeftPth, RightPth)

        CallExpression.Rule =
            From callable In PrimaryExpression
            From _lph In LeftPth
            From _lc In LineContinuation
            From arguments In ArgumentList
            From _lc2 In LineContinuation
            From _rph In RightPth
            Select New CallExpression(callable, arguments).ToBase()

        BracketExpression.Rule =
            From indexable In PrimaryExpressionNonNewArray
            From _lbk In LeftBrck
            From _lc In LineContinuation
            From arguments In ArgumentList
            From _lc2 In LineContinuation
            From _rbk In RightBrck
            Select New BracketExpression(indexable, arguments).ToBase()

        Dim basicExpression = PrimaryExpression Or CallExpression Or BracketExpression

        ' unary expressions
        UnaryExpression.Rule =
            basicExpression Or
            (From op In MinusSymbol
            From exp In UnaryExpression
            Select New UnaryExpression(op.Value.Span, ExpressionOp.Minus, exp).ToBase) Or
            (From op In PlusSymbol
            From exp In UnaryExpression
            Select New UnaryExpression(op.Value.Span, ExpressionOp.Plus, exp).ToBase) Or
            (From op In NotKeyword
            From exp In UnaryExpression
            Select New UnaryExpression(op.Value.Span, ExpressionOp.Not, exp).ToBase) Or
            CastExpression

        CastExpression.Rule =
            From op In CastKeyword
            From typesp In TypeSpecifier.Optional
            From exp In UnaryExpression
            Select New CastExpression(op.Value.Span, exp, typesp).ToBase

        'binary expressions
        FactorExpression.Rule = UnaryExpression

        TermExpression.Rule =
            FactorExpression Or
            (From left In TermExpression
            From op In Asterisk
            From _lc In LineContinuation
            From right In FactorExpression
            Select New BinaryExpression(ExpressionOp.Multiplication, left, right).ToBase()) Or
            (From left In TermExpression
            From op In Slash
            From _lc In LineContinuation
            From right In FactorExpression
            Select New BinaryExpression(ExpressionOp.Divition, left, right).ToBase()) Or
            (From left In TermExpression
            From op In ModKeyword
            From _lc In LineContinuation
            From right In FactorExpression
            Select New BinaryExpression(ExpressionOp.Modulo, left, right).ToBase())

        ShiftingExpression.Rule =
            TermExpression Or
            (From left In ShiftingExpression
            From op In PlusSymbol
            From _lc In LineContinuation
            From right In TermExpression
            Select New BinaryExpression(ExpressionOp.Addition, left, right).ToBase()) Or
            (From left In ShiftingExpression
            From op In MinusSymbol
            From _lc In LineContinuation
            From right In TermExpression
            Select New BinaryExpression(ExpressionOp.Minus, left, right).ToBase())

        ShiftRightOperator.Rule =
            From g1 In GreaterSymbol
            From g2 In GreaterSymbol
            Let op = New LexemeValue(g1.Value.Content & g2.Value.Content, New SourceSpan(g1.Value.Span.StartLocation, g2.Value.Span.EndLocation))
            Where Grammar.Check(g2.PrefixTrivia.Count = 0, ErrorCode.RightShiftSymbolError, op.Span)
            Select op

        ComparandExpression.Rule =
            ShiftingExpression Or
            (From left In ComparandExpression
            From op In ShiftLeft
            From _lc In LineContinuation
            From right In ShiftingExpression
            Select New BinaryExpression(ExpressionOp.ShiftLeft, left, right).ToBase()) Or
            (From left In ComparandExpression
            From _shr In ShiftRightOperator
            From _lc In LineContinuation
            From right In ShiftingExpression
            Select New BinaryExpression(ExpressionOp.ShiftRight, left, right).ToBase())

        ComparisonExpression.Rule =
            ComparandExpression Or
            (From left In ComparisonExpression
            From op In GreaterSymbol
            From _lc In LineContinuation
            From right In ComparandExpression
            Select New BinaryExpression(ExpressionOp.Greater, left, right).ToBase()) Or
            (From left In ComparisonExpression
            From op In GreaterEqual
            From _lc In LineContinuation
            From right In ComparandExpression
            Select New BinaryExpression(ExpressionOp.GreaterEqual, left, right).ToBase()) Or
            (From left In ComparisonExpression
            From op In LessSymbol
            From _lc In LineContinuation
            From right In ComparandExpression
            Select New BinaryExpression(ExpressionOp.Less, left, right).ToBase()) Or
            (From left In ComparisonExpression
            From op In LessEqual
            From _lc In LineContinuation
            From right In ComparandExpression
            Select New BinaryExpression(ExpressionOp.LessEqual, left, right).ToBase())

        NotEqualOperator.Rule =
            From lt In LessSymbol
            From gt In GreaterSymbol
            Let op = New LexemeValue(lt.Value.Content & gt.Value.Content, New SourceSpan(lt.Value.Span.StartLocation, gt.Value.Span.EndLocation))
            Where Grammar.Check(gt.PrefixTrivia.Count = 0, ErrorCode.NotEqualSymbolError, op.Span)
            Select op

        EqualityExpression.Rule =
            ComparisonExpression Or
            (From left In EqualityExpression
            From op In EqualSymbol
            From _lc In LineContinuation
            From right In ComparisonExpression
            Select New BinaryExpression(ExpressionOp.Equal, left, right).ToBase()) Or
            (From left In EqualityExpression
            From _neq In NotEqualOperator
            From _lc In LineContinuation
            From right In ComparisonExpression
            Select New BinaryExpression(ExpressionOp.NotEqual, left, right).ToBase())

        AndExpression.Rule =
            EqualityExpression Or
            From left In AndExpression
            From op In AndKeyword
            From _lc In LineContinuation
            From right In EqualityExpression
            Select New BinaryExpression(ExpressionOp.And, left, right).ToBase()

        XorExpression.Rule =
            AndExpression Or
            From left In XorExpression
            From op In XorKeyword
            From _lc In LineContinuation
            From right In AndExpression
            Select New BinaryExpression(ExpressionOp.Xor, left, right).ToBase()

        OrExpression.Rule =
            XorExpression Or
            From left In OrExpression
            From op In OrKeyword
            From _lc In LineContinuation
            From right In XorExpression
            Select New BinaryExpression(ExpressionOp.Or, left, right).ToBase()

        Dim FreeLambdaParameter = ParameterDeclaration
        Dim LambdaParameterListDecl =
            From _lph In LeftPth
            From _lc1 In LineContinuation
            From params In LambdaParameterList
            From _lc2 In LineContinuation
            From _rph In RightPth
            Select params

        Dim LambdaSignatureParams =
            LambdaParameterListDecl Or
            From p In FreeLambdaParameter
            Select DirectCast({p}, IEnumerable(Of ParameterDeclaration))

        LambdaSignature.Rule =
            From params In LambdaSignatureParams
            Select New LambdaSignature(params)

        LambdaExpression.Rule =
            From signature In LambdaSignature
            From _arrow In Arrow
            From _lc In LineContinuation
            From body In LambdaBody
            Select New LambdaExpression(signature, body).ToBase

        LambdaBody.Rule =
            (From exp In Expression
             Select New ExpressionLambdaBody(exp).ToLambdaBody) Or
            (From block In StatementsBlock
             Select New BlockLambdaBody(block).ToLambdaBody)

        Expression.Rule = OrExpression Or LambdaExpression

        ArgumentList.Rule =
             Expression.Select(Function(exp) New Argument(exp)).Many(Comma.Concat(LineContinuation))

        ScannerInfo.CurrentLexerIndex = m_keywordLexerIndex

        'Return From c In Parsers.Any.Many() Select New SyntaxTreeNode(c)
        Return Program

        'Return ST.SuffixedBy(EndKeyword.AsTerminal).SuffixedBy(ST).Select(Function(s) CType(Nothing, CompilationUnit))
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

    Private m_sw As New Stopwatch()

    Protected Overrides Function OnCreateTransitionTable(automaton As Parsers.Generator.LR0Model, scannerInfo As ScannerInfo) As Parsers.Generator.TransitionTable
        'Dim lr0 = automaton.ToString()

        Dim transitionTable = MyBase.OnCreateTransitionTable(automaton, scannerInfo)
        m_sw.Stop()

        m_parserCreationTime = m_sw.ElapsedMilliseconds

        Return transitionTable
    End Function

    Protected Overrides Function OnCreateAutomaton(productionInfoManager As Parsers.Generator.ProductionInfoManager) As Parsers.Generator.LR0Model
        m_sw.Restart()
        Return MyBase.OnCreateAutomaton(productionInfoManager)
    End Function
End Class
