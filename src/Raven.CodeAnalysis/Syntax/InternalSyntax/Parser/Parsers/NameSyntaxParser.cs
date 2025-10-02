namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;
using System.Collections.Generic;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

internal class NameSyntaxParser : SyntaxParser
{
    public NameSyntaxParser(ParseContext parent) : base(parent)
    {

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
        if (ConsumeToken(SyntaxKind.AmpersandToken, out var ampToken))
        {
            var elementType = ParseTypeName();
            return ByRefType(ampToken, elementType);
        }

        var signedLiteral = TryParseSignedNumericLiteralType();
        if (signedLiteral is not null)
        {
            return signedLiteral;
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

        SyntaxList types = SyntaxList.Empty;

        bool isUnion = false;

        if (IsNextToken(SyntaxKind.BarToken))
        {
            isUnion = true;
            types = types.Add(name);
        }

        while (ConsumeToken(SyntaxKind.BarToken, out var barToken))
        {
            types = types.Add(barToken);
            types = types.Add(ParseTypeName());
        }

        if (isUnion)
        {
            return UnionType(types);
        }

        return name;
    }

    private TypeSyntax? TryParseSignedNumericLiteralType()
    {
        var signToken = PeekToken();

        if (!signToken.IsKind(SyntaxKind.PlusToken) && !signToken.IsKind(SyntaxKind.MinusToken))
        {
            return null;
        }

        var checkpoint = CreateCheckpoint("signed-numeric-literal-type");
        signToken = ReadToken();

        var numericToken = PeekToken();

        if (!numericToken.IsKind(SyntaxKind.NumericLiteralToken) ||
            signToken.TrailingTrivia.Count > 0 ||
            numericToken.LeadingTrivia.Count > 0)
        {
            checkpoint.Dispose();
            return null;
        }

        numericToken = ReadToken();

        var combinedToken = CreateSignedNumericLiteralToken(signToken, numericToken);
        return LiteralType(SyntaxKind.NumericLiteralType, combinedToken);
    }

    private static InternalSyntax.SyntaxToken CreateSignedNumericLiteralToken(InternalSyntax.SyntaxToken signToken, InternalSyntax.SyntaxToken numericToken)
    {
        var sign = signToken.Kind == SyntaxKind.MinusToken ? -1 : 1;
        var tokenText = string.Concat(signToken.Text, numericToken.Text);
        object? signedValue = numericToken.GetValue() switch
        {
            int value => sign * value,
            long value => sign * value,
            float value => sign * value,
            double value => sign * value,
            _ => numericToken.GetValue(),
        };

        var diagnostics = CombineDiagnostics(signToken, numericToken);

        return new InternalSyntax.SyntaxToken(
            SyntaxKind.NumericLiteralToken,
            tokenText,
            signedValue,
            tokenText.Length,
            signToken.LeadingTrivia,
            numericToken.TrailingTrivia,
            diagnostics,
            annotations: null);
    }

    private static DiagnosticInfo[]? CombineDiagnostics(InternalSyntax.SyntaxToken first, InternalSyntax.SyntaxToken second)
    {
        List<DiagnosticInfo>? diagnostics = null;

        foreach (var diagnostic in first.GetDiagnostics())
        {
            diagnostics ??= new List<DiagnosticInfo>();
            diagnostics.Add(diagnostic);
        }

        foreach (var diagnostic in second.GetDiagnostics())
        {
            diagnostics ??= new List<DiagnosticInfo>();
            diagnostics.Add(diagnostic);
        }

        return diagnostics?.ToArray();
    }

    private TypeSyntax ParseNameCore()
    {
        var peek = PeekToken();
        if (peek.IsKind(SyntaxKind.NullKeyword))
        {
            ReadToken();
            return NullType(peek);
        }

        if (peek.Kind is SyntaxKind.TrueKeyword or SyntaxKind.FalseKeyword or
            SyntaxKind.NumericLiteralToken or SyntaxKind.StringLiteralToken or
            SyntaxKind.CharacterLiteralToken)
        {
            ReadToken();
            var kind = peek.Kind switch
            {
                SyntaxKind.TrueKeyword => SyntaxKind.TrueLiteralType,
                SyntaxKind.FalseKeyword => SyntaxKind.FalseLiteralType,
                SyntaxKind.NumericLiteralToken => SyntaxKind.NumericLiteralType,
                SyntaxKind.StringLiteralToken => SyntaxKind.StringLiteralType,
                SyntaxKind.CharacterLiteralToken => SyntaxKind.CharacterLiteralType,
                _ => SyntaxKind.LiteralType,
            };

            return LiteralType(kind, peek);
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
            rankSpecifiers.Add(ParseArrayRankSpecifier());
        }

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

        if (!IsIdentifierToken(name)
            && HasLeadingEndOfLineTrivia(name))
        {
            AddDiagnostic(DiagnosticInfo.Create(CompilerDiagnostics.IdentifierExpected, GetSpanOfLastToken()));
            return IdentifierName(
                MissingToken(SyntaxKind.IdentifierToken), Diagnostics);
        }

        if (name.IsKind(SyntaxKind.StarToken))
        {
            ReadToken();
            return WildcardName(name);
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

        while (true)
        {
            var t = PeekToken();

            if (t.IsKind(SyntaxKind.GreaterThanToken))
                break;

            var typeName = new NameSyntaxParser(this).ParseTypeName();
            if (typeName is null)
                break;

            argumentList.Add(TypeArgument(typeName));

            var commaToken = PeekToken();
            if (commaToken.IsKind(SyntaxKind.CommaToken))
            {
                ReadToken();
                argumentList.Add(commaToken);
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.GreaterThanToken, out var greaterThanToken);

        return TypeArgumentList(lessThanToken, List(argumentList.ToArray()), greaterThanToken);
    }

    private TupleTypeSyntax ParseTupleType()
    {
        var openParenToken = ReadToken();

        List<GreenNode> elements = new List<GreenNode>();

        while (true)
        {
            var t = PeekToken();

            if (t.IsKind(SyntaxKind.CloseParenToken))
                break;

            NameColonSyntax? nameColon = null;

            if (PeekToken(1).IsKind(SyntaxKind.ColonToken) && CanTokenBeIdentifier(PeekToken()))
            {
                var name = ReadToken();
                if (name.Kind != SyntaxKind.IdentifierToken)
                {
                    name = ToIdentifierToken(name);
                    UpdateLastToken(name);
                }
                var colon = ReadToken();
                nameColon = NameColon(IdentifierName(name), colon);
            }

            var type = ParseTypeName();
            if (type is null)
                break;

            elements.Add(TupleElement(nameColon, type));

            var commaToken = PeekToken();
            if (commaToken.IsKind(SyntaxKind.CommaToken))
            {
                ReadToken();
                elements.Add(commaToken);
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

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
            case SyntaxKind.IntKeyword:
            case SyntaxKind.DoubleKeyword:
            case SyntaxKind.ObjectKeyword:
            case SyntaxKind.UnitKeyword:
                return true;
        }

        return false;
    }
}
