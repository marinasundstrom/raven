namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System.Collections.Generic;

using Raven.CodeAnalysis.Syntax.InternalSyntax;

using static SyntaxFactory;

internal class UnionDeclarationParser : SyntaxParser
{
    public UnionDeclarationParser(ParseContext context) : base(context)
    {
    }

    internal UnionDeclarationSyntax Parse()
    {
        var attributeLists = AttributeDeclarationParser.ParseAttributeLists(this);
        var modifiers = ParseModifiers();

        var unionKeyword = ExpectToken(SyntaxKind.UnionKeyword);

        SyntaxToken identifier;
        if (CanTokenBeIdentifier(PeekToken()))
        {
            identifier = ReadIdentifierToken();
        }
        else
        {
            identifier = ExpectToken(SyntaxKind.IdentifierToken);
        }

        TypeParameterListSyntax? typeParameterList = null;
        if (PeekToken().IsKind(SyntaxKind.LessThanToken))
        {
            typeParameterList = new TypeDeclarationParser(this).ParseTypeParameterList();
        }

        ConsumeTokenOrMissing(SyntaxKind.OpenBraceToken, out var openBraceToken);

        List<GreenNode> cases = new();

        while (true)
        {
            var next = PeekToken();

            if (next.IsKind(SyntaxKind.CloseBraceToken))
            {
                break;
            }

            var unionCase = ParseCase();
            cases.Add(unionCase);
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseBraceToken, out var closeBraceToken);

        var terminatorToken = ConsumeOptionalTypeTerminator();

        return UnionDeclaration(
            attributeLists,
            modifiers,
            unionKeyword,
            identifier,
            typeParameterList,
            openBraceToken,
            List(cases),
            closeBraceToken,
            terminatorToken);
    }

    private UnionCaseClauseSyntax ParseCase()
    {
        SyntaxToken identifier;
        if (CanTokenBeIdentifier(PeekToken()))
        {
            identifier = ReadIdentifierToken();
        }
        else
        {
            identifier = ExpectToken(SyntaxKind.IdentifierToken);
        }

        ParameterListSyntax? parameterList = null;
        if (PeekToken().IsKind(SyntaxKind.OpenParenToken))
        {
            parameterList = new TypeDeclarationParser(this).ParseParameterList();
        }

        return UnionCaseClause(identifier, parameterList);
    }

    private SyntaxList ParseModifiers()
    {
        SyntaxList modifiers = SyntaxList.Empty;

        while (true)
        {
            var kind = PeekToken().Kind;

            if (kind is SyntaxKind.PublicKeyword or
                    SyntaxKind.PrivateKeyword or
                    SyntaxKind.InternalKeyword or
                    SyntaxKind.ProtectedKeyword or
                    SyntaxKind.StaticKeyword or
                    SyntaxKind.AbstractKeyword or
                    SyntaxKind.SealedKeyword or
                    SyntaxKind.PartialKeyword or
                    SyntaxKind.VirtualKeyword or
                    SyntaxKind.AsyncKeyword or
                    SyntaxKind.OpenKeyword or
                    SyntaxKind.OverrideKeyword)
            {
                modifiers = modifiers.Add(ReadToken());
            }
            else
            {
                break;
            }
        }

        return modifiers;
    }

    private SyntaxToken ConsumeOptionalTypeTerminator()
    {
        var previous = TreatNewlinesAsTokens;
        SetTreatNewlinesAsTokens(true);

        try
        {
            var current = PeekToken();

            if (IsNewLineLike(current) || current.Kind == SyntaxKind.SemicolonToken)
            {
                return ReadToken();
            }

            if (current.Kind is SyntaxKind.EndOfFileToken or SyntaxKind.CloseBraceToken)
            {
                return Token(SyntaxKind.None);
            }

            return Token(SyntaxKind.None);
        }
        finally
        {
            SetTreatNewlinesAsTokens(previous);
        }
    }

    private static bool IsNewLineLike(SyntaxToken token)
    {
        return token.Kind is SyntaxKind.NewLineToken or
            SyntaxKind.LineFeedToken or
            SyntaxKind.CarriageReturnToken or
            SyntaxKind.CarriageReturnLineFeedToken;
    }
}
