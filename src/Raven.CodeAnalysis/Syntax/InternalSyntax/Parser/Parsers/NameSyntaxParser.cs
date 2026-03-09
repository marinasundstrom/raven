namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;
using System.Collections.Generic;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

internal class NameSyntaxParser : SyntaxParser
{
    private readonly bool _allowOmittedTypeArguments;

    public NameSyntaxParser(ParseContext parent, bool allowOmittedTypeArguments = false) : base(parent)
    {
        _allowOmittedTypeArguments = allowOmittedTypeArguments;
    }


    public NameSyntax ParseName()
    {
        NameSyntax left = ParseUnqualifiedName();

        while (ConsumeToken(SyntaxKind.DotToken, out var dotToken))
        {
            left = QualifiedName(left, dotToken, ParseUnqualifiedName());
        }

        return left;
    }

    public TypeSyntax ParseTypeName()
    {
        // Type union syntax (`A | B`) and literal type syntax (`"x"`, `42`, `true`) are intentionally disabled.
        return ParseTypeNameElement(allowImplicitFunctionTypeRecovery: true);
    }

    public TypeSyntax ParseTypeNameWithoutFunctionRecovery()
    {
        return ParseTypeNameElement(allowImplicitFunctionTypeRecovery: false);
    }

    private TypeSyntax ParseTypeNameElement(bool allowImplicitFunctionTypeRecovery)
    {
        if (ConsumeToken(SyntaxKind.FuncKeyword, out var funcKeyword))
        {
            return ParseFunctionType(funcKeyword);
        }

        if (ConsumeToken(SyntaxKind.AmpersandToken, out var ampToken))
        {
            var elementType = ParseTypeName();
            return ByRefType(ampToken, elementType);
        }

        if (ConsumeToken(SyntaxKind.StarToken, out var starToken))
        {
            var elementType = ParseTypeName();
            return PointerType(starToken, elementType);
        }

        TypeSyntax name;

        if (PeekToken().IsKind(SyntaxKind.OpenParenToken))
        {
            if (PeekToken(1).IsKind(SyntaxKind.CloseParenToken))
            {
                var open = ReadToken();
                var close = ReadToken();
                name = UnitType(open, close);
            }
            else
            {
                name = ParseTupleType();
            }
        }
        else
        {
            name = ParseNameCore();
        }

        name = ParseArrayTypeSuffix(name);

        if (ConsumeToken(SyntaxKind.QuestionToken, out var questionToken))
        {
            name = NullableType(name, questionToken);
        }

        if (allowImplicitFunctionTypeRecovery && ConsumeToken(SyntaxKind.ArrowToken, out var arrowToken))
        {
            var returnType = new NameSyntaxParser(this).ParseTypeName();
            name = CreateImplicitFunctionType(name, arrowToken, returnType);
        }

        return name;
    }

    private FunctionTypeSyntax ParseFunctionType(SyntaxToken funcKeyword)
    {
        if (PeekToken().IsKind(SyntaxKind.OpenParenToken))
        {
            var openParenToken = ReadToken();

            List<GreenNode> parameters = new List<GreenNode>();

            while (true)
            {
                var next = PeekToken();

                if (next.IsKind(SyntaxKind.CloseParenToken) || next.IsKind(SyntaxKind.EndOfFileToken))
                    break;

                var parameterType = new NameSyntaxParser(this).ParseTypeName();
                if (parameterType is null)
                    break;

                parameters.Add(parameterType);

                if (ConsumeToken(SyntaxKind.CommaToken, out var commaToken))
                {
                    parameters.Add(commaToken);
                    continue;
                }

                break;
            }

            ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);
            ConsumeTokenOrMissing(SyntaxKind.ArrowToken, out var arrowToken);

            var returnType = new NameSyntaxParser(this).ParseTypeName();
            var parameterList = FunctionTypeParameterList(openParenToken, List(parameters.ToArray()), closeParenToken);

            return FunctionType(funcKeyword, null, parameterList, arrowToken, returnType);
        }

        var parameter = new NameSyntaxParser(this).ParseTypeNameWithoutFunctionRecovery();
        ConsumeTokenOrMissing(SyntaxKind.ArrowToken, out var singleParameterArrowToken);
        var singleParameterReturnType = new NameSyntaxParser(this).ParseTypeName();

        return FunctionType(funcKeyword, parameter, null, singleParameterArrowToken, singleParameterReturnType);
    }

    private FunctionTypeSyntax CreateImplicitFunctionType(TypeSyntax candidateParameter, SyntaxToken arrowToken, TypeSyntax returnType)
    {
        if (candidateParameter is UnitTypeSyntax unitType)
        {
            var parameterList = FunctionTypeParameterList(unitType.OpenParenToken, SyntaxList.Empty, unitType.CloseParenToken);
            return FunctionType(Token(SyntaxKind.None), null, parameterList, arrowToken, returnType);
        }

        if (candidateParameter is TupleTypeSyntax tupleType)
        {
            var parameters = new List<GreenNode>();

            for (var i = 0; i < tupleType.Elements.SlotCount; i++)
            {
                var element = tupleType.Elements[i];
                switch (element)
                {
                    case TupleElementSyntax tupleElement:
                        parameters.Add(tupleElement.Type);
                        break;
                    case SyntaxToken commaToken when commaToken.Kind == SyntaxKind.CommaToken:
                        parameters.Add(commaToken);
                        break;
                }
            }

            var parameterList = FunctionTypeParameterList(tupleType.OpenParenToken, List(parameters.ToArray()), tupleType.CloseParenToken);
            return FunctionType(Token(SyntaxKind.None), null, parameterList, arrowToken, returnType);
        }

        return FunctionType(Token(SyntaxKind.None), candidateParameter, null, arrowToken, returnType);
    }

    private TypeSyntax ParseNameCore()
    {
        var peek = PeekToken();
        if (peek.IsKind(SyntaxKind.NullKeyword))
        {
            ReadToken();
            return NullType(peek);
        }

        if (IsPredefinedTypeKeyword(peek))
        {
            ReadToken();

            TypeSyntax type = PredefinedType(peek);

            if (ConsumeToken(SyntaxKind.QuestionToken, out var qToken))
            {
                type = NullableType(type, qToken);
            }

            return type;
        }

        TypeSyntax left = ParseUnqualifiedName();

        while (ConsumeToken(SyntaxKind.DotToken, out var dotToken))
        {
            left = QualifiedName((NameSyntax)left, dotToken, ParseUnqualifiedName());
        }

        return left;
    }

    private TypeSyntax ParseArrayTypeSuffix(TypeSyntax elementType)
    {
        if (!PeekToken().IsKind(SyntaxKind.OpenBracketToken))
        {
            return elementType;
        }

        var rankSpecifiers = new List<GreenNode>();

        while (PeekToken().IsKind(SyntaxKind.OpenBracketToken))
        {
            var rankSpecifier = ParseArrayRankSpecifier();

            rankSpecifiers.Add(rankSpecifier);

            if (rankSpecifier.CloseBracketToken.IsMissing)
            {
                // Recovery: keep the consumed '[' and missing ']' so the parser
                // makes forward progress on malformed inputs.
                break;
            }
        }

        if (rankSpecifiers.Count == 0)
            return elementType;

        return ArrayType(elementType, List(rankSpecifiers.ToArray()));
    }

    private ArrayRankSpecifierSyntax ParseArrayRankSpecifier()
    {
        var openBracket = ReadToken();
        SyntaxList commas = SyntaxList.Empty;

        while (ConsumeToken(SyntaxKind.CommaToken, out var commaToken))
        {
            commas = commas.Add(commaToken);
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseBracketToken, out var closeBracket);

        return ArrayRankSpecifier(openBracket, commas, closeBracket);
    }

    public UnqualifiedNameSyntax ParseUnqualifiedName()
    {
        var name = PeekToken();

        if (name.IsKind(SyntaxKind.StarToken))
        {
            ReadToken();
            return WildcardName(name);
        }

        if (!IsIdentifierToken(name)
            && HasLeadingEndOfLineTrivia(name))
        {
            AddDiagnostic(DiagnosticInfo.Create(CompilerDiagnostics.IdentifierExpected, GetSpanOfLastToken()));
            return IdentifierName(
                MissingToken(SyntaxKind.IdentifierToken));
        }

        if (CanTokenBeIdentifier(name))
        {
            name = ReadToken();
            if (name.Kind != SyntaxKind.IdentifierToken)
            {
                name = ToIdentifierToken(name);
                UpdateLastToken(name);
            }

            if (
                PeekToken().IsKind(SyntaxKind.LessThanToken) &&
                LooksLikeTypeArgumentList())
            {
                var typeArgList = ParseTypeArgumentList();
                return GenericName(name, typeArgList);
            }
        }
        else
        {
            name = MissingToken(SyntaxKind.IdentifierToken);
        }

        return IdentifierName(name);
    }

    public SimpleNameSyntax ParseSimpleName()
    {
        var name = ParseUnqualifiedName();
        if (name is WildcardNameSyntax)
        {
            return IdentifierName(MissingToken(SyntaxKind.IdentifierToken));
        }

        return (SimpleNameSyntax)name;
    }

    private TypeArgumentListSyntax ParseTypeArgumentList()
    {
        var lessThanToken = ReadToken();

        List<GreenNode> argumentList = new List<GreenNode>();

        var parsedArguments = 0;
        var restoreNewlinesAsTokens = TreatNewlinesAsTokens;
        SetTreatNewlinesAsTokens(false);

        SyntaxToken greaterThanToken;

        try
        {
            while (true)
            {
                var argumentStart = Position;
                var t = PeekToken();

                while (IsNewLineLike(t))
                {
                    ReadToken();
                    t = PeekToken();
                }

                if (t.IsKind(SyntaxKind.EndOfFileToken) ||
                    t.IsKind(SyntaxKind.GreaterThanToken) ||
                    t.IsKind(SyntaxKind.GreaterThanGreaterThanToken))
                {
                    break;
                }

                if (_allowOmittedTypeArguments &&
                    parsedArguments == 0 &&
                    t.IsKind(SyntaxKind.CommaToken))
                {
                    argumentList.Add(TypeArgument(IdentifierName(MissingToken(SyntaxKind.IdentifierToken))));
                    parsedArguments++;
                    continue;
                }

                if (parsedArguments > 0)
                {
                    if (t.IsKind(SyntaxKind.CommaToken))
                    {
                        var commaToken = ReadToken();
                        argumentList.Add(commaToken);
                        t = PeekToken();

                        while (IsNewLineLike(t))
                        {
                            ReadToken();
                            t = PeekToken();
                        }

                        if (_allowOmittedTypeArguments &&
                            (t.IsKind(SyntaxKind.CommaToken) ||
                             t.IsKind(SyntaxKind.GreaterThanToken) ||
                             t.IsKind(SyntaxKind.GreaterThanGreaterThanToken) ||
                             t.IsKind(SyntaxKind.EndOfFileToken)))
                        {
                            argumentList.Add(TypeArgument(IdentifierName(MissingToken(SyntaxKind.IdentifierToken))));
                            parsedArguments++;
                            continue;
                        }
                    }
                    else
                    {
                        argumentList.Add(MissingToken(SyntaxKind.CommaToken));
                        AddDiagnostic(
                            DiagnosticInfo.Create(
                                CompilerDiagnostics.CharacterExpected,
                                GetSpanOfPeekedToken(),
                                ","));
                    }
                }

                var typeName = new NameSyntaxParser(this).ParseTypeName();
                if (typeName is null or { IsMissing: true })
                {
                    AddDiagnostic(
                        DiagnosticInfo.Create(
                            CompilerDiagnostics.IdentifierExpected,
                            GetSpanOfPeekedToken()));
                    typeName = IdentifierName(MissingToken(SyntaxKind.IdentifierToken));
                }

                if (Position == argumentStart)
                {
                    var token = PeekToken();
                    var tokenText = string.IsNullOrEmpty(token.Text)
                        ? token.Kind.ToString()
                        : token.Text;

                    AddDiagnostic(
                        DiagnosticInfo.Create(
                            CompilerDiagnostics.UnexpectedTokenInIncompleteSyntax,
                            GetSpanOfPeekedToken(),
                            tokenText));

                    if (token.IsKind(SyntaxKind.GreaterThanToken) ||
                        token.IsKind(SyntaxKind.GreaterThanGreaterThanToken) ||
                        token.IsKind(SyntaxKind.EndOfFileToken))
                        break;

                    ReadToken();
                    continue;
                }

                argumentList.Add(TypeArgument(typeName));
                parsedArguments++;
            }

            if (!ConsumeToken(SyntaxKind.GreaterThanToken, out greaterThanToken))
            {
                if (TrySplitLeadingLookaheadToken(
                    SyntaxKind.GreaterThanGreaterThanToken,
                    SyntaxKind.GreaterThanToken,
                    ">",
                    SyntaxKind.GreaterThanToken,
                    ">"))
                {
                    ConsumeTokenOrMissing(SyntaxKind.GreaterThanToken, out greaterThanToken);
                }
                else
                {
                    greaterThanToken = MissingToken(SyntaxKind.GreaterThanToken);
                }
            }
        }
        finally
        {
            SetTreatNewlinesAsTokens(restoreNewlinesAsTokens);
        }

        if (greaterThanToken.IsMissing)
        {
            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.CharacterExpected,
                    GetSpanOfLastToken(),
                    ">"));
        }

        return TypeArgumentList(lessThanToken, List(argumentList.ToArray()), greaterThanToken);
    }

    private static bool IsNewLineLike(SyntaxToken token)
    {
        return token.Kind is SyntaxKind.NewLineToken or SyntaxKind.LineFeedToken or SyntaxKind.CarriageReturnToken or SyntaxKind.CarriageReturnLineFeedToken;
    }

    private TypeSyntax ParseTupleType()
    {
        var openParenToken = ReadToken();

        List<GreenNode> elements = new List<GreenNode>();
        var tupleElementCount = 0;
        var hasNamedElement = false;
        var hasTrailingComma = false;
        TypeSyntax? singleElementType = null;

        while (true)
        {
            var t = PeekToken();

            if (t.IsKind(SyntaxKind.CloseParenToken))
                break;

            NameColonSyntax? nameColon = null;

            if (PeekToken(1).IsKind(SyntaxKind.ColonToken) && CanTokenBeIdentifier(PeekToken()))
            {
                hasNamedElement = true;
                var name = ReadToken();
                if (name.Kind != SyntaxKind.IdentifierToken)
                {
                    name = ToIdentifierToken(name);
                    UpdateLastToken(name);
                }
                var colon = ReadToken();
                nameColon = NameColon(IdentifierName(name), colon);
            }

            var startPosition = Position;
            var type = ParseTypeName();
            if (type is null || type.IsMissing || Position == startPosition)
                break;

            elements.Add(TupleElement(nameColon, type));
            tupleElementCount++;
            if (tupleElementCount == 1)
            {
                singleElementType = type;
            }

            var commaToken = PeekToken();
            if (commaToken.IsKind(SyntaxKind.CommaToken))
            {
                ReadToken();
                elements.Add(commaToken);
                hasTrailingComma = PeekToken().IsKind(SyntaxKind.CloseParenToken);
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

        if (tupleElementCount == 1 && !hasNamedElement && !hasTrailingComma && singleElementType is not null)
        {
            // Single parenthesized types are grouping, e.g. (() -> ())?
            return singleElementType;
        }

        if (tupleElementCount == 1)
        {
            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.SingleElementTupleTypeNotAllowed,
                    GetSpanOfLastToken()));
        }

        return TupleType(openParenToken, List(elements.ToArray()), closeParenToken);
    }

    private bool LooksLikeTypeArgumentList()
    {
        int depth = 0;
        int i = 0;

        while (true)
        {
            var token = PeekToken(i++);

            if (token.IsKind(SyntaxKind.LessThanToken))
            {
                depth++;
            }
            else if (token.IsKind(SyntaxKind.GreaterThanToken))
            {
                depth--;
                if (depth == 0)
                    return true;
            }
            else if (token.IsKind(SyntaxKind.GreaterThanGreaterThanToken))
            {
                depth -= 2;
                if (depth <= 0)
                    return true;
            }
            else if (token.IsKind(SyntaxKind.EndOfFileToken) ||
                     token.IsKind(SyntaxKind.SemicolonToken))
            {
                return false;
            }
            else if (token.IsMissing)
            {
                return false;
            }
            else if (depth == 0)
            {
                return false; // Not inside <...>
            }

            if (i > 20)
                return false; // bail out
        }
    }

    private bool IsPredefinedTypeKeyword(SyntaxToken token)
    {
        switch (token.Kind)
        {
            case SyntaxKind.StringKeyword:
            case SyntaxKind.BoolKeyword:
            case SyntaxKind.CharKeyword:
            case SyntaxKind.SByteKeyword:
            case SyntaxKind.ShortKeyword:
            case SyntaxKind.UShortKeyword:
            case SyntaxKind.IntKeyword:
            case SyntaxKind.UIntKeyword:
            case SyntaxKind.LongKeyword:
            case SyntaxKind.ULongKeyword:
            case SyntaxKind.ByteKeyword:
            case SyntaxKind.FloatKeyword:
            case SyntaxKind.DoubleKeyword:
            case SyntaxKind.DecimalKeyword:
            case SyntaxKind.NIntKeyword:
            case SyntaxKind.NUIntKeyword:
            case SyntaxKind.ObjectKeyword:
            case SyntaxKind.UnitKeyword:
                return true;
        }

        return false;
    }
}
