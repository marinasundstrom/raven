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
        var classOrStructKeyword = ConsumeClassOrStructKeyword();

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

        UnionMemberTypeListSyntax? memberTypes = null;
        if (PeekToken().IsKind(SyntaxKind.OpenParenToken))
        {
            memberTypes = ParseUnionMemberTypeList();
        }

        var constraintClauses = new ConstrainClauseListParser(this).ParseConstraintClauseList();

        SyntaxToken openBraceToken;
        SyntaxToken closeBraceToken;
        List<GreenNode> cases = new();

        if (memberTypes is null)
        {
            ConsumeTokenOrMissing(SyntaxKind.OpenBraceToken, out openBraceToken);

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

            ConsumeTokenOrMissing(SyntaxKind.CloseBraceToken, out closeBraceToken);
        }
        else
        {
            openBraceToken = Token(SyntaxKind.None);
            closeBraceToken = Token(SyntaxKind.None);
        }

        var terminatorToken = ConsumeOptionalTypeTerminator();

        return UnionDeclaration(
            attributeLists,
            modifiers,
            unionKeyword,
            classOrStructKeyword,
            identifier,
            typeParameterList,
            memberTypes,
            constraintClauses,
            openBraceToken,
            List(cases),
            closeBraceToken,
            terminatorToken);
    }

    private SyntaxToken ConsumeClassOrStructKeyword()
    {
        if (PeekToken().Kind is SyntaxKind.ClassKeyword or SyntaxKind.StructKeyword)
            return ReadToken();

        return Token(SyntaxKind.None);
    }

    private UnionMemberTypeListSyntax ParseUnionMemberTypeList()
    {
        var openParenToken = ExpectToken(SyntaxKind.OpenParenToken);
        var types = new List<GreenNode>();
        var restoreNewlinesAsTokens = TreatNewlinesAsTokens;
        SetTreatNewlinesAsTokens(false);

        SyntaxToken closeParenToken;

        try
        {
            var parsedTypes = 0;
            while (true)
            {
                var current = PeekToken();

                if (parsedTypes > 0)
                {
                    if (current.IsKind(SyntaxKind.CommaToken))
                    {
                        types.Add(ReadToken());
                        current = PeekToken();
                    }
                    else if (!current.IsKind(SyntaxKind.CloseParenToken) &&
                             !current.IsKind(SyntaxKind.EndOfFileToken))
                    {
                        types.Add(MissingToken(SyntaxKind.CommaToken));
                    }
                }

                while (current.Kind is SyntaxKind.LineFeedToken or SyntaxKind.CarriageReturnToken or SyntaxKind.CarriageReturnLineFeedToken)
                {
                    ReadToken();
                    current = PeekToken();
                }

                if (current.IsKind(SyntaxKind.CloseParenToken) || current.IsKind(SyntaxKind.EndOfFileToken))
                    break;

                var typeName = new NameSyntaxParser(this).ParseTypeName();
                if (typeName is null or { IsMissing: true })
                {
                    AddDiagnostic(
                        DiagnosticInfo.Create(
                            CompilerDiagnostics.IdentifierExpected,
                            GetSpanOfPeekedToken()));
                    typeName = IdentifierName(MissingToken(SyntaxKind.IdentifierToken));
                }

                types.Add(typeName);
                parsedTypes++;
            }

            ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out closeParenToken);
        }
        finally
        {
            SetTreatNewlinesAsTokens(restoreNewlinesAsTokens);
        }

        return UnionMemberTypeList(openParenToken, List(types), closeParenToken);
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
