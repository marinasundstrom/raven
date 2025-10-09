namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System.Collections.Generic;

using static SyntaxFactory;

internal sealed class ExtensionDeclarationParser : SyntaxParser
{
    public ExtensionDeclarationParser(ParseContext context) : base(context)
    {
    }

    internal ExtensionDeclarationSyntax Parse(SyntaxList attributeLists, SyntaxList modifiers)
    {
        var extensionKeyword = ExpectToken(SyntaxKind.ExtensionKeyword);

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
            typeParameterList = ParseTypeParameterList();

        var forKeyword = ExpectToken(SyntaxKind.ForKeyword);
        var receiverType = new NameSyntaxParser(this).ParseTypeName();

        ConsumeTokenOrMissing(SyntaxKind.OpenBraceToken, out var openBraceToken);

        List<GreenNode> members = new();
        while (true)
        {
            var current = PeekToken();
            if (current.IsKind(SyntaxKind.CloseBraceToken))
                break;

            if (current.IsKind(SyntaxKind.EndOfFileToken))
                break;

            var member = ParseMember();
            members.Add(member);

            SetTreatNewlinesAsTokens(false);

            var separator = PeekToken();
            if (separator.IsKind(SyntaxKind.CommaToken))
            {
                ReadToken();
                members.Add(separator);
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseBraceToken, out var closeBraceToken);

        var terminatorToken = ConsumeOptionalTypeTerminator();

        return ExtensionDeclaration(
            attributeLists,
            modifiers,
            extensionKeyword,
            identifier,
            typeParameterList,
            forKeyword,
            receiverType,
            openBraceToken,
            List(members),
            closeBraceToken,
            terminatorToken,
            Diagnostics);
    }

    private MemberDeclarationSyntax ParseMember()
    {
        var attributeLists = AttributeDeclarationParser.ParseAttributeLists(this);
        var modifiers = ParseMemberModifiers();

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
            typeParameterList = ParseTypeParameterList();

        var parameterList = new StatementSyntaxParser(this).ParseParameterList();
        var returnType = new TypeAnnotationClauseSyntaxParser(this).ParseReturnTypeAnnotation();

        BlockStatementSyntax? body = null;
        ArrowExpressionClauseSyntax? expressionBody = null;
        var next = PeekToken();
        if (next.IsKind(SyntaxKind.OpenBraceToken))
        {
            body = new StatementSyntaxParser(this).ParseBlockStatementSyntax();
        }
        else if (next.IsKind(SyntaxKind.FatArrowToken))
        {
            expressionBody = new ExpressionSyntaxParser(this).ParseArrowExpressionClause();
        }

        TryConsumeTerminator(out var terminatorToken);

        return MethodDeclaration(
            attributeLists,
            modifiers,
            explicitInterfaceSpecifier: null,
            identifier,
            typeParameterList,
            parameterList,
            returnType,
            body,
            expressionBody,
            terminatorToken);
    }

    private TypeParameterListSyntax ParseTypeParameterList()
    {
        var lessThanToken = ReadToken();
        List<GreenNode> parameters = new();

        while (true)
        {
            var token = PeekToken();
            if (token.IsKind(SyntaxKind.GreaterThanToken))
                break;

            SyntaxToken? varianceKeyword = null;
            if (token.IsKind(SyntaxKind.InKeyword) || token.IsKind(SyntaxKind.OutKeyword))
            {
                varianceKeyword = ReadToken();
                token = PeekToken();
            }

            SyntaxToken identifier;
            if (CanTokenBeIdentifier(token))
            {
                identifier = ReadIdentifierToken();
            }
            else
            {
                identifier = ExpectToken(SyntaxKind.IdentifierToken);
            }

            SyntaxToken? colonToken = null;
            SyntaxList constraints = SyntaxList.Empty;

            if (PeekToken().IsKind(SyntaxKind.ColonToken))
            {
                colonToken = ReadToken();
                var constraintNodes = new List<GreenNode>();
                while (true)
                {
                    var constraint = ParseTypeParameterConstraint();
                    constraintNodes.Add(constraint);

                    var separator = PeekToken();
                    if (!separator.IsKind(SyntaxKind.CommaToken))
                        break;

                    ReadToken();
                    constraintNodes.Add(separator);
                }

                constraints = List(constraintNodes);
            }

            parameters.Add(TypeParameter(varianceKeyword, identifier, colonToken, constraints));

            var commaToken = PeekToken();
            if (commaToken.IsKind(SyntaxKind.CommaToken))
            {
                ReadToken();
                parameters.Add(commaToken);
            }
            else
            {
                if (!commaToken.IsKind(SyntaxKind.GreaterThanToken))
                    break;
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.GreaterThanToken, out var greaterThanToken);
        return TypeParameterList(lessThanToken, List(parameters), greaterThanToken);
    }

    private TypeParameterConstraintSyntax ParseTypeParameterConstraint()
    {
        var token = PeekToken();
        if (token.IsKind(SyntaxKind.ClassKeyword))
        {
            var classKeyword = ReadToken();
            return ClassConstraint(classKeyword);
        }

        if (token.IsKind(SyntaxKind.StructKeyword))
        {
            var structKeyword = ReadToken();
            return StructConstraint(structKeyword);
        }

        var type = new NameSyntaxParser(this).ParseTypeName();
        return TypeConstraint(type);
    }

    private SyntaxList ParseMemberModifiers()
    {
        SyntaxList modifiers = SyntaxList.Empty;

        while (true)
        {
            var kind = PeekToken().Kind;
            if (kind is SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword)
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
                return ReadToken();

            if (current.Kind is SyntaxKind.EndOfFileToken or SyntaxKind.CloseBraceToken)
                return Token(SyntaxKind.None);

            return Token(SyntaxKind.None);
        }
        finally
        {
            SetTreatNewlinesAsTokens(previous);
        }
    }

    private static bool IsNewLineLike(SyntaxToken token)
        => token.Kind is SyntaxKind.NewLineToken or SyntaxKind.LineFeedToken or SyntaxKind.CarriageReturnToken or SyntaxKind.CarriageReturnLineFeedToken;
}
