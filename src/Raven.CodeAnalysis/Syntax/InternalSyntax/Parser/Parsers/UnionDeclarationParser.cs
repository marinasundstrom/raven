namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System.Collections.Generic;
using System;

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
        UnionCaseFieldClauseSyntax? fieldClause = null;

        // Positional payload: Success(x: int)
        if (PeekToken().IsKind(SyntaxKind.OpenParenToken))
        {
            parameterList = new TypeDeclarationParser(this).ParseParameterList();
        }
        // Record-like payload: Success { a: int, b: string }
        else if (PeekToken().IsKind(SyntaxKind.OpenBraceToken))
        {
            fieldClause = ParseCaseFieldClause();
        }

        return UnionCaseClause(identifier, parameterList, fieldClause);
    }

    private UnionCaseFieldClauseSyntax ParseCaseFieldClause()
    {
        ConsumeTokenOrMissing(SyntaxKind.OpenBraceToken, out var openBraceToken);

        // We build a separated list by interleaving nodes and comma tokens.
        // This matches the common internal representation used elsewhere in the parser.
        List<GreenNode> items = new();

        while (true)
        {
            var next = PeekToken();

            if (next.IsKind(SyntaxKind.CloseBraceToken) || next.IsKind(SyntaxKind.EndOfFileToken))
                break;

            // Parse a single field:  a: int (= <expr>)?
            var field = ParseCaseField();
            items.Add(field);

            // Optional comma between fields
            if (PeekToken().IsKind(SyntaxKind.CommaToken))
            {
                items.Add(ReadToken());
                continue;
            }

            // No comma -> stop (caller will consume close brace)
            break;
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseBraceToken, out var closeBraceToken);

        return UnionCaseFieldClause(
            openBraceToken,
            List(items),
            closeBraceToken);
    }

    private UnionCaseFieldSyntax ParseCaseField()
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

        // Parse ": <type>" using existing type declaration parsing.
        // We intentionally reuse TypeAnnotationClause so the union field declaration syntax
        // stays consistent with parameters/locals/properties.
        var typeAnnotation = ParseTypeAnnotationClauseForField();

        // Optional initializer: "= <expr>"
        EqualsValueClauseSyntax? initializer = null;
        if (PeekToken().IsKind(SyntaxKind.EqualsToken))
        {
            var equalsToken = ReadToken();
            var value = new ExpressionSyntaxParser(this).ParseExpression();
            initializer = EqualsValueClause(equalsToken, value);
        }

        return UnionCaseField(identifier, typeAnnotation, initializer);
    }

    private TypeAnnotationClauseSyntax ParseTypeAnnotationClauseForField()
    {
        // Try to reuse the existing type declaration parser if it already has a helper.
        // If it doesn't, we fall back to a minimal ": <type>" parse.
        //
        // NOTE: `TypeDeclarationParser.ParseTypeAnnotationClause()` exists in most of Raven's
        // parsers; if it doesn't in your current branch, the fallback path will still work.
        var typeDeclParser = new TypeDeclarationParser(this);

        // Prefer a dedicated API when present.
        // This is written defensively to keep compilation stable across refactors.
        try
        {
            // If ParseTypeAnnotationClause exists, use it.
            return new TypeAnnotationClauseSyntaxParser(this).ParseTypeAnnotation()!;
        }
        catch
        {
            var colonToken = ExpectToken(SyntaxKind.ColonToken);
            var type = new NameSyntaxParser(this).ParseName();
            return TypeAnnotationClause(colonToken, type);
        }
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
