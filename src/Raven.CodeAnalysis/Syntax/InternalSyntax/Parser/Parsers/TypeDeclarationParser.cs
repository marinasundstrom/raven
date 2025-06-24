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

        return ClassDeclaration(modifiers, structOrClassKeyword, identifier, SyntaxList.Empty, openBraceToken, List(memberList), closeBraceToken, terminatorToken);
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
            if (PeekToken(1).Kind == SyntaxKind.OpenParenToken)
            {
                return ParseMethodDeclarationOrPropertyDeclaration(modifiers);
            }
            else
            {
                return ParsePropertyDeclaration(modifiers, keywordOrIdentifier);
            }
        }
    }

    private MemberDeclarationSyntax ParseMethodDeclarationOrPropertyDeclaration(SyntaxList modifiers)
    {
        if (!ConsumeTokenOrMissing(SyntaxKind.IdentifierToken, out var identifier))
        {
            if (!ConsumeTokenOrMissing(SyntaxKind.InitKeyword, out identifier))
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

        return ParsePropertyDeclaration(modifiers, identifier);
    }

    private MemberDeclarationSyntax ParseMethodOrConstructorDeclaration(SyntaxList modifiers, SyntaxToken identifier)
    {
        var parameterList = ParseParameterList();

        var returnParameterAnnotation = new TypeAnnotationSyntaxParser(this).ParseReturnTypeAnnotation();

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
        else if (token.IsKind(SyntaxKind.ArrowToken))
        {
            expressionBody = new ExpressionSyntaxParser(this).ParseArrowExpressionClause();
        }

        TryConsumeTerminator(out var terminatorToken);

        if (expressionBody is not null)
        {
            return MethodDeclaration(modifiers, identifier, parameterList, returnParameterAnnotation, expressionBody, terminatorToken);
        }
        else if (body is not null)
        {
            return MethodDeclaration(modifiers, identifier, parameterList, returnParameterAnnotation, body, terminatorToken);
        }

        throw new Exception();
    }

    private PropertyDeclarationSyntax ParsePropertyDeclaration(SyntaxList modifiers, SyntaxToken identifier)
    {
        ReadToken();

        var typeAnnotation = new TypeAnnotationSyntaxParser(this).ParseReturnTypeAnnotation();

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
            //typeAnnotation = (ReturnTypeAnnotationSyntax)typeAnnotation.ReplaceNode(lastToken, newToken);
        }
        
        TryConsumeTerminator(out var terminatorToken);

        return PropertyDeclaration(modifiers, identifier, typeAnnotation, accessorList, terminatorToken);
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
            else if (token.IsKind(SyntaxKind.ArrowToken))
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
                    modifiers, name, expressionBody, terminatorToken));
            }
            else if (body is not null)
            {
                accessorList.Add(AccessorDeclaration(
                    name.IsKind(SyntaxKind.GetKeyword) ? SyntaxKind.GetAccessorDeclaration
                    : SyntaxKind.SetAccessorDeclaration,
                    modifiers, name, body, terminatorToken));
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

            var typeAnnotation = new TypeAnnotationSyntaxParser(this).ParseTypeAnnotation();

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

        var typeAnnotation = new TypeAnnotationSyntaxParser(this).ParseTypeAnnotation();

        if (IsNextToken(SyntaxKind.EqualsToken, out var _))
        {
            initializer = new EqualsValueClauseSyntaxParser(this).Parse();
        }

        var declarators = new SyntaxList(
            [VariableDeclarator(identifier, typeAnnotation, initializer)]);

        return new VariableDeclarationSyntax(letOrVarKeyword, declarators);
    }

    private TypeAnnotationSyntax? ParseTypeAnnotationSyntax()
    {
        if (ConsumeToken(SyntaxKind.ColonToken, out var colonToken))
        {
            TypeSyntax type = new NameSyntaxParser(this).ParseTypeName();

            return TypeAnnotation(colonToken, type);
        }

        return null;
    }
}