namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;

using static SyntaxFactory;

internal class TypeDeclarationParser : SyntaxParser
{
    public TypeDeclarationParser(ParseContext context) : base(context)
    {

    }

    internal BaseTypeDeclarationSyntax Parse()
    {
        var modifiers = ParseModifiers();

        var structOrClassKeyword = ReadToken();

        if (!ConsumeTokenOrMissing(SyntaxKind.IdentifierToken, out var identifier))
        {

        }

        List<GreenNode> memberList = new List<GreenNode>();

        ConsumeTokenOrMissing(SyntaxKind.OpenBraceToken, out var openBraceToken);

        while (true)
        {
            var t = PeekToken();

            if (t.IsKind(SyntaxKind.CloseBraceToken))
                break;


            var member = ParseMember();

            memberList.Add(member);

            SetTreatNewlinesAsTokens(false);

            var commaToken = PeekToken();
            if (commaToken.IsKind(SyntaxKind.CommaToken))
            {
                ReadToken();
                memberList.Add(commaToken);
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseBraceToken, out var closeBraceToken);

        TryConsumeTerminator(out var terminatorToken);

        return ClassDeclaration(modifiers, structOrClassKeyword, identifier, null, openBraceToken, List(memberList), closeBraceToken, terminatorToken);
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

    private MemberDeclarationSyntax ParseMember()
    {
        var modifiers = ParseModifiers();

        var keywordOrIdentifier = PeekToken();

        if (keywordOrIdentifier.IsKind(SyntaxKind.LetKeyword) || keywordOrIdentifier.IsKind(SyntaxKind.VarKeyword))
        {
            return ParseFieldDeclarationSyntax(modifiers);
        }
        else
        {
            if (keywordOrIdentifier.IsKind(SyntaxKind.InitKeyword))
            {
                return ParseConstructorDeclaration(modifiers, keywordOrIdentifier);
            }
            else if (PeekToken(1).Kind == SyntaxKind.OpenParenToken)
            {
                return ParseMethodOrConstructorDeclarationBase(modifiers);
            }
            else if (PeekToken(1).Kind == SyntaxKind.OpenBracketToken)
            {
                return ParseIndexerDeclaration(modifiers, keywordOrIdentifier);
            }
            else
            {
                return ParsePropertyDeclaration(modifiers, keywordOrIdentifier);
            }
        }
    }

    private MemberDeclarationSyntax ParseConstructorDeclaration(SyntaxList modifiers, SyntaxToken initKeyword)
    {
        ReadToken();

        ConsumeTokenOrNull(SyntaxKind.IdentifierToken, out var identifier);

        // Check is open paren

        var parameterList = ParseParameterList();

        var token = PeekToken();

        BlockSyntax? body = null;
        ArrowExpressionClauseSyntax? expressionBody = null;

        if (token.IsKind(SyntaxKind.OpenBraceToken))
        {
            body = new ExpressionSyntaxParser(this).ParseBlockSyntax();
        }
        else if (token.IsKind(SyntaxKind.FatArrowToken))
        {
            expressionBody = new ExpressionSyntaxParser(this).ParseArrowExpressionClause();
        }

        TryConsumeTerminator(out var terminatorToken);

        if (expressionBody is not null)
        {
            return ConstructorDeclaration(modifiers, initKeyword, identifier, parameterList, null, expressionBody, terminatorToken);
        }
        else if (body is not null)
        {
            return ConstructorDeclaration(modifiers, initKeyword, identifier, parameterList, body, null, terminatorToken);
        }

        throw new Exception();
    }

    private MemberDeclarationSyntax ParseMethodOrConstructorDeclarationBase(SyntaxList modifiers)
    {
        if (!ConsumeTokenOrMissing(SyntaxKind.IdentifierToken, out var identifier))
        {
            if (!ConsumeTokenOrMissing(SyntaxKind.SelfKeyword, out identifier))
            {
                // Init should be a constructordeclarationbtw BTW
                // Invalid name
            }
        }

        var potentialOpenParenToken = PeekToken();

        if (potentialOpenParenToken.IsKind(SyntaxKind.OpenParenToken))
        {
            return ParseMethodOrConstructorDeclaration(modifiers, identifier);
        }

        // Remove below

        throw new Exception();
    }

    private MemberDeclarationSyntax ParseMethodOrConstructorDeclaration(SyntaxList modifiers, SyntaxToken identifier)
    {
        var parameterList = ParseParameterList();

        var returnParameterAnnotation = new TypeAnnotationClauseSyntaxParser(this).ParseReturnTypeAnnotation();

        if (identifier.IsKind(SyntaxKind.InitKeyword))
        {
            // Report type annotation
            // ConstructorDeclaration
        }

        var token = PeekToken();

        BlockSyntax? body = null;
        ArrowExpressionClauseSyntax? expressionBody = null;

        if (token.IsKind(SyntaxKind.OpenBraceToken))
        {
            body = new ExpressionSyntaxParser(this).ParseBlockSyntax();
        }
        else if (token.IsKind(SyntaxKind.FatArrowToken))
        {
            expressionBody = new ExpressionSyntaxParser(this).ParseArrowExpressionClause();
        }

        TryConsumeTerminator(out var terminatorToken);

        if (expressionBody is not null)
        {
            return MethodDeclaration(modifiers, identifier, parameterList, returnParameterAnnotation, null, expressionBody, terminatorToken);
        }
        else if (body is not null)
        {
            return MethodDeclaration(modifiers, identifier, parameterList, returnParameterAnnotation, body, null, terminatorToken);
        }

        throw new Exception();
    }

    private PropertyDeclarationSyntax ParsePropertyDeclaration(SyntaxList modifiers, SyntaxToken identifier)
    {
        ReadToken();

        var typeAnnotation = new TypeAnnotationClauseSyntaxParser(this).ParseTypeAnnotation();

        var token = PeekToken();

        AccessorListSyntax? accessorList = null;
        if (token.IsKind(SyntaxKind.OpenBraceToken))
        {
            accessorList = ParseAccessorList();
        }
        else
        {
            // Handle skipped trivia

            //var lastToken = typeAnnotation.GetLastToken();
            //var newToken = token.WithTrailingTrivia();
            //typeAnnotation = (ArrowTypeClauseSyntax)typeAnnotation.ReplaceNode(lastToken, newToken);
        }

        TryConsumeTerminator(out var terminatorToken);

        return PropertyDeclaration(modifiers, identifier, typeAnnotation, accessorList, null, terminatorToken);
    }

    private IndexerDeclarationSyntax ParseIndexerDeclaration(SyntaxList modifiers, SyntaxToken identifier)
    {
        ReadToken();

        var parameterList = ParseBracketedParameterList();

        var typeAnnotation = new TypeAnnotationClauseSyntaxParser(this).ParseTypeAnnotation();

        var token = PeekToken();

        AccessorListSyntax? accessorList = null;
        if (token.IsKind(SyntaxKind.OpenBraceToken))
        {
            accessorList = ParseAccessorList();
        }
        else
        {
            // Handle skipped trivia

            //var lastToken = typeAnnotation.GetLastToken();
            //var newToken = token.WithTrailingTrivia();
            //typeAnnotation = (ArrowTypeClauseSyntax)typeAnnotation.ReplaceNode(lastToken, newToken);
        }

        TryConsumeTerminator(out var terminatorToken);

        return IndexerDeclaration(modifiers, identifier, parameterList, typeAnnotation, accessorList, null, terminatorToken);
    }

    private AccessorListSyntax ParseAccessorList()
    {
        var openBraceToken = ReadToken();

        List<GreenNode> accessorList = new List<GreenNode>();

        SetTreatNewlinesAsTokens(false);

        while (true)
        {
            var t = PeekToken();

            if (t.IsKind(SyntaxKind.CloseBraceToken))
                break;

            SyntaxList modifiers = SyntaxList.Empty;

            SyntaxToken modifier;
            if (ConsumeToken(SyntaxKind.RefKeyword, out modifier) || ConsumeToken(SyntaxKind.OutKeyword, out modifier) || ConsumeToken(SyntaxKind.InKeyword, out modifier))
            {
                modifiers = modifiers.Add(modifier);
            }

            SyntaxToken name;

            if (!ConsumeToken(SyntaxKind.GetKeyword, out name) && !ConsumeToken(SyntaxKind.SetKeyword, out name))
            {

            }

            var token = PeekToken();

            BlockSyntax? body = null;
            ArrowExpressionClauseSyntax? expressionBody = null;

            if (token.IsKind(SyntaxKind.OpenBraceToken))
            {
                body = new ExpressionSyntaxParser(this).ParseBlockSyntax();
            }
            else if (token.IsKind(SyntaxKind.FatArrowToken))
            {
                expressionBody = new ExpressionSyntaxParser(this).ParseArrowExpressionClause();
            }

            SetTreatNewlinesAsTokens(true);

            TryConsumeTerminator(out var terminatorToken);

            SetTreatNewlinesAsTokens(false);

            if (expressionBody is not null)
            {
                accessorList.Add(AccessorDeclaration(
                    name.IsKind(SyntaxKind.GetKeyword) ? SyntaxKind.GetAccessorDeclaration
                    : SyntaxKind.SetAccessorDeclaration,
                    modifiers, name, null, expressionBody, terminatorToken));
            }
            else if (body is not null)
            {
                accessorList.Add(AccessorDeclaration(
                    name.IsKind(SyntaxKind.GetKeyword) ? SyntaxKind.GetAccessorDeclaration
                    : SyntaxKind.SetAccessorDeclaration,
                    modifiers, name, body, null, terminatorToken));
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseBraceToken, out var closeBraceToken);

        SetTreatNewlinesAsTokens(false);

        return AccessorList(openBraceToken, List(accessorList.ToArray()), closeBraceToken);
    }

    public ParameterListSyntax ParseParameterList()
    {
        var openParenToken = ReadToken();

        List<GreenNode> parameterList = new List<GreenNode>();

        while (true)
        {
            var t = PeekToken();

            if (t.IsKind(SyntaxKind.CloseParenToken))
                break;

            SyntaxList modifiers = SyntaxList.Empty;

            SyntaxToken modifier;
            if (ConsumeToken(SyntaxKind.RefKeyword, out modifier) || ConsumeToken(SyntaxKind.OutKeyword, out modifier) || ConsumeToken(SyntaxKind.InKeyword, out modifier))
            {
                modifiers = modifiers.Add(modifier);
            }

            if (!ConsumeToken(SyntaxKind.IdentifierToken, out var name))
            {

            }

            var typeAnnotation = new TypeAnnotationClauseSyntaxParser(this).ParseTypeAnnotation();

            parameterList.Add(Parameter(modifiers, name, typeAnnotation));

            var commaToken = PeekToken();
            if (commaToken.IsKind(SyntaxKind.CommaToken))
            {
                ReadToken();
                parameterList.Add(commaToken);
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

        return ParameterList(openParenToken, List(parameterList.ToArray()), closeParenToken);
    }

    private FieldDeclarationSyntax ParseFieldDeclarationSyntax(SyntaxList modifiers)
    {
        var declaration = ParseVariableDeclarationSyntax();

        SetTreatNewlinesAsTokens(true);

        if (!TryConsumeTerminator(out var terminatorToken))
        {
            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    GetEndOfLastToken()));
        }

        return FieldDeclaration(modifiers, declaration, terminatorToken, Diagnostics);
    }

    private VariableDeclarationSyntax? ParseVariableDeclarationSyntax()
    {
        var letOrVarKeyword = ReadToken();

        if (!ConsumeToken(SyntaxKind.IdentifierToken, out var identifier))
        {

        }

        EqualsValueClauseSyntax? initializer = null;

        var typeAnnotation = new TypeAnnotationClauseSyntaxParser(this).ParseTypeAnnotation();

        if (IsNextToken(SyntaxKind.EqualsToken, out var _))
        {
            initializer = new EqualsValueClauseSyntaxParser(this).Parse();
        }

        var declarators = new SyntaxList(
            [VariableDeclarator(identifier, typeAnnotation, initializer)]);

        return new VariableDeclarationSyntax(letOrVarKeyword, declarators);
    }

    private TypeAnnotationClauseSyntax? ParseTypeAnnotationClauseSyntax()
    {
        if (ConsumeToken(SyntaxKind.ColonToken, out var colonToken))
        {
            TypeSyntax type = new NameSyntaxParser(this).ParseTypeName();

            return TypeAnnotationClause(colonToken, type);
        }

        return null;
    }

    public BracketedParameterListSyntax ParseBracketedParameterList()
    {
        var openBracketToken = ReadToken();

        List<GreenNode> parameterList = new List<GreenNode>();

        while (true)
        {
            var t = PeekToken();

            if (t.IsKind(SyntaxKind.CloseBracketToken))
                break;

            SyntaxList modifiers = SyntaxList.Empty;

            SyntaxToken modifier;
            if (ConsumeToken(SyntaxKind.RefKeyword, out modifier) || ConsumeToken(SyntaxKind.OutKeyword, out modifier) || ConsumeToken(SyntaxKind.InKeyword, out modifier))
            {
                modifiers = modifiers.Add(modifier);
            }

            if (!ConsumeToken(SyntaxKind.IdentifierToken, out var name))
            {

            }

            var typeAnnotation = new TypeAnnotationClauseSyntaxParser(this).ParseTypeAnnotation();

            parameterList.Add(Parameter(modifiers, name, typeAnnotation));

            var commaToken = PeekToken();
            if (commaToken.IsKind(SyntaxKind.CommaToken))
            {
                ReadToken();
                parameterList.Add(commaToken);
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseBracketToken, out var closeBracketToken);

        return BracketedParameterList(openBracketToken, List(parameterList.ToArray()), closeBracketToken);
    }
}