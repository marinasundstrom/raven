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

        var constraintClauses = new ConstrainClauseListParser(this).ParseConstraintClauseList();

        ConsumeTokenOrMissing(SyntaxKind.OpenBraceToken, out var openBraceToken);

        List<GreenNode> cases = new();

        while (true)
        {
            var next = PeekToken();

            if (next.IsKind(SyntaxKind.CloseBraceToken))
            {
                break;
            }

            if (next.IsKind(SyntaxKind.CommaToken))
            {
                ReadToken();
                continue;
            }

            var caseStart = Position;
            var unionCase = ParseCase();
            cases.Add(unionCase);

            // Ensure parser progress when recovery produced only missing tokens.
            if (Position == caseStart && !PeekToken().IsKind(SyntaxKind.EndOfFileToken))
            {
                ReadToken();
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseBraceToken, out var closeBraceToken);

        var terminatorToken = ConsumeOptionalTypeTerminator();

        return UnionDeclaration(
            attributeLists,
            modifiers,
            unionKeyword,
            identifier,
            typeParameterList,
            constraintClauses,
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

        var terminatorToken = ConsumeOptionalCaseTerminator();

        return UnionCaseClause(identifier, parameterList, terminatorToken);
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
                    SyntaxKind.FilescopeKeyword or
                    SyntaxKind.StaticKeyword or
                    SyntaxKind.AbstractKeyword or
                    SyntaxKind.FinalKeyword or
                    SyntaxKind.SealedKeyword or
                    SyntaxKind.PartialKeyword or
                    SyntaxKind.VirtualKeyword or
                    SyntaxKind.AsyncKeyword or
                    SyntaxKind.OpenKeyword or
                    SyntaxKind.RecordKeyword or
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
        var current = PeekToken();

        if (current.Kind == SyntaxKind.SemicolonToken)
            return ReadToken();

        if (HasLineBreakBeforePeekToken() || current.Kind is SyntaxKind.EndOfFileToken or SyntaxKind.CloseBraceToken)
            return Token(SyntaxKind.None);

        return Token(SyntaxKind.None);
    }

    private SyntaxToken ConsumeOptionalCaseTerminator()
    {
        var current = PeekToken();

        if (current.Kind is SyntaxKind.CommaToken or SyntaxKind.SemicolonToken)
            return ReadToken();

        if (HasLineBreakBeforePeekToken())
            return Token(SyntaxKind.None);

        return Token(SyntaxKind.None);
    }
}
