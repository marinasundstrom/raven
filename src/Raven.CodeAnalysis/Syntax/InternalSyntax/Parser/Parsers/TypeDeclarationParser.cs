namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;
using System.Collections.Generic;

using Raven.CodeAnalysis.Syntax.InternalSyntax;

using static SyntaxFactory;

internal class TypeDeclarationParser : SyntaxParser
{
    public TypeDeclarationParser(ParseContext context) : base(context)
    {

    }

    internal BaseTypeDeclarationSyntax Parse()
    {
        var modifiers = ParseModifiers();

        var typeKeyword = ReadToken();

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
        if (typeKeyword.IsKind(SyntaxKind.ClassKeyword) && PeekToken().IsKind(SyntaxKind.OpenParenToken))
        {
            parameterList = ParseParameterList();
        }

        TypeParameterListSyntax? typeParameterList = null;
        if (PeekToken().IsKind(SyntaxKind.LessThanToken))
        {
            typeParameterList = ParseTypeParameterList();
        }

        BaseListSyntax? baseList = ParseBaseList();

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

        var terminatorToken = ConsumeOptionalTypeTerminator();

        if (typeKeyword.IsKind(SyntaxKind.InterfaceKeyword))
        {
            return InterfaceDeclaration(modifiers, typeKeyword, identifier, typeParameterList, baseList, null, openBraceToken, List(memberList), closeBraceToken, terminatorToken);
        }

        return ClassDeclaration(modifiers, typeKeyword, identifier, typeParameterList, baseList, parameterList, openBraceToken, List(memberList), closeBraceToken, terminatorToken);
    }

    private TypeParameterListSyntax ParseTypeParameterList()
    {
        var lessThanToken = ReadToken();

        List<GreenNode> parameters = new List<GreenNode>();

        while (true)
        {
            var token = PeekToken();

            if (token.IsKind(SyntaxKind.GreaterThanToken))
                break;

            SyntaxToken identifier;
            if (CanTokenBeIdentifier(token))
            {
                identifier = ReadIdentifierToken();
            }
            else
            {
                identifier = ExpectToken(SyntaxKind.IdentifierToken);
            }

            parameters.Add(TypeParameter(identifier));

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

    private BaseListSyntax? ParseBaseList()
    {
        if (ConsumeToken(SyntaxKind.ColonToken, out var colonToken))
        {
            var types = new List<GreenNode>();
            while (true)
            {
                var type = new NameSyntaxParser(this).ParseTypeName();
                types.Add(type);

                var commaToken = PeekToken();
                if (commaToken.IsKind(SyntaxKind.CommaToken))
                {
                    ReadToken();
                    types.Add(commaToken);
                }
                else
                {
                    break;
                }
            }

            return BaseList(colonToken, List(types));
        }

        return null;
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
                     SyntaxKind.VirtualKeyword or
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

    private MemberDeclarationSyntax ParseMember()
    {
        var typeDeclarationCheckpoint = CreateCheckpoint();
        var modifiers = ParseModifiers();

        var keywordOrIdentifier = PeekToken();

        if (keywordOrIdentifier.IsKind(SyntaxKind.ClassKeyword) || keywordOrIdentifier.IsKind(SyntaxKind.InterfaceKeyword))
        {
            typeDeclarationCheckpoint.Dispose();
            return new TypeDeclarationParser(this).Parse();
        }

        if (keywordOrIdentifier.IsKind(SyntaxKind.LetKeyword) || keywordOrIdentifier.IsKind(SyntaxKind.VarKeyword))
        {
            return ParseFieldDeclarationSyntax(modifiers);
        }

        if (keywordOrIdentifier.IsKind(SyntaxKind.IdentifierToken) && PeekToken(1).Kind == SyntaxKind.ColonToken)
        {
            var checkpoint = CreateCheckpoint();
            ReadToken();
            _ = new TypeAnnotationClauseSyntaxParser(this).ParseTypeAnnotation();

            var nextToken = PeekToken();

            bool looksLikeField = nextToken.Kind is SyntaxKind.EqualsToken
                or SyntaxKind.CommaToken
                or SyntaxKind.SemicolonToken
                or SyntaxKind.CloseBraceToken
                or SyntaxKind.EndOfFileToken
                or SyntaxKind.LineFeedToken
                or SyntaxKind.CarriageReturnToken
                or SyntaxKind.CarriageReturnLineFeedToken
                or SyntaxKind.NewLineToken;

            checkpoint.Dispose();

            if (looksLikeField)
            {
                return ParseFieldDeclarationSyntax(modifiers);
            }
        }

        if (keywordOrIdentifier.IsKind(SyntaxKind.InitKeyword))
        {
            return ParseConstructorDeclaration(modifiers, keywordOrIdentifier);
        }

        var nameCheckpoint = CreateCheckpoint();
        _ = ParseMemberNameWithExplicitInterface();
        var tokenAfterName = PeekToken();
        nameCheckpoint.Dispose();

        if (tokenAfterName.IsKind(SyntaxKind.OpenParenToken))
        {
            return ParseMethodOrConstructorDeclarationBase(modifiers);
        }

        if (tokenAfterName.IsKind(SyntaxKind.OpenBracketToken))
        {
            return ParseIndexerDeclaration(modifiers);
        }

        return ParsePropertyDeclaration(modifiers);
    }

    private MemberDeclarationSyntax ParseConstructorDeclaration(SyntaxList modifiers, SyntaxToken initKeyword)
    {
        ReadToken();

        SyntaxToken? identifier = null;
        if (CanTokenBeIdentifier(PeekToken()))
        {
            identifier = ReadIdentifierToken();
        }
        else
        {
            ConsumeTokenOrNull(SyntaxKind.IdentifierToken, out identifier);
        }

        // Check is open paren

        var parameterList = ParseParameterList();

        ConstructorInitializerSyntax? initializer = null;
        if (PeekToken().IsKind(SyntaxKind.ColonToken))
        {
            var colonToken = ReadToken();
            ConsumeTokenOrMissing(SyntaxKind.BaseKeyword, out var baseKeyword);
            var argumentList = new ExpressionSyntaxParser(this).ParseArgumentListSyntax();
            initializer = BaseConstructorInitializer(colonToken, baseKeyword, argumentList);
        }

        var token = PeekToken();

        BlockStatementSyntax? body = null;
        ArrowExpressionClauseSyntax? expressionBody = null;

        if (token.IsKind(SyntaxKind.OpenBraceToken))
        {
            body = new StatementSyntaxParser(this).ParseBlockStatementSyntax();
        }
        else if (token.IsKind(SyntaxKind.FatArrowToken))
        {
            expressionBody = new ExpressionSyntaxParser(this).ParseArrowExpressionClause();
        }

        TryConsumeTerminator(out var terminatorToken);

        if (identifier is null)
        {
            if (expressionBody is not null)
            {
                return ConstructorDeclaration(modifiers, initKeyword, parameterList, initializer, null, expressionBody, terminatorToken);
            }
            else if (body is not null)
            {
                return ConstructorDeclaration(modifiers, initKeyword, parameterList, initializer, body, null, terminatorToken);
            }
        }
        else
        {
            if (expressionBody is not null)
            {
                return NamedConstructorDeclaration(modifiers, initKeyword, identifier, parameterList, null, expressionBody, terminatorToken);
            }
            else if (body is not null)
            {
                return NamedConstructorDeclaration(modifiers, initKeyword, identifier, parameterList, body, null, terminatorToken);
            }
        }

        throw new Exception();
    }

    private MemberDeclarationSyntax ParseMethodOrConstructorDeclarationBase(SyntaxList modifiers)
    {
        var (explicitInterfaceSpecifier, identifier) = ParseMemberNameWithExplicitInterface();

        var potentialOpenParenToken = PeekToken();

        if (potentialOpenParenToken.IsKind(SyntaxKind.OpenParenToken))
        {
            return ParseMethodOrConstructorDeclaration(modifiers, explicitInterfaceSpecifier, identifier);
        }

        // Remove below

        throw new Exception();
    }

    private MemberDeclarationSyntax ParseMethodOrConstructorDeclaration(
        SyntaxList modifiers,
        ExplicitInterfaceSpecifierSyntax? explicitInterfaceSpecifier,
        SyntaxToken identifier)
    {
        var parameterList = ParseParameterList();

        var returnParameterAnnotation = new TypeAnnotationClauseSyntaxParser(this).ParseReturnTypeAnnotation();

        if (identifier.IsKind(SyntaxKind.InitKeyword))
        {
            // Report type annotation
            // ConstructorDeclaration
        }

        var token = PeekToken();

        BlockStatementSyntax? body = null;
        ArrowExpressionClauseSyntax? expressionBody = null;

        if (token.IsKind(SyntaxKind.OpenBraceToken))
        {
            body = new StatementSyntaxParser(this).ParseBlockStatementSyntax();
        }
        else if (token.IsKind(SyntaxKind.FatArrowToken))
        {
            expressionBody = new ExpressionSyntaxParser(this).ParseArrowExpressionClause();
        }

        TryConsumeTerminator(out var terminatorToken);

        if (expressionBody is not null)
        {
            return MethodDeclaration(modifiers, explicitInterfaceSpecifier, identifier, parameterList, returnParameterAnnotation, null, expressionBody, terminatorToken);
        }
        else if (body is not null)
        {
            return MethodDeclaration(modifiers, explicitInterfaceSpecifier, identifier, parameterList, returnParameterAnnotation, body, null, terminatorToken);
        }

        return MethodDeclaration(modifiers, explicitInterfaceSpecifier, identifier, parameterList, returnParameterAnnotation, null, null, terminatorToken);
    }

    private (ExplicitInterfaceSpecifierSyntax? ExplicitInterfaceSpecifier, SyntaxToken Identifier) ParseMemberNameWithExplicitInterface()
    {
        if (ConsumeToken(SyntaxKind.SelfKeyword, out var selfToken))
            return (null, selfToken);

        var checkpoint = CreateCheckpoint();
        var typeName = new NameSyntaxParser(this).ParseTypeName();

        if (typeName is QualifiedNameSyntax qualifiedName)
        {
            SyntaxToken identifierToken = qualifiedName.Right switch
            {
                IdentifierNameSyntax identifierName => identifierName.Identifier,
                GenericNameSyntax genericName => genericName.Identifier,
                _ => default
            };

            if (identifierToken != default)
            {
                identifierToken = ToIdentifierToken(identifierToken);
                UpdateLastToken(identifierToken);

                var explicitInterfaceName = (TypeSyntax)qualifiedName.Left;
                var explicitInterfaceSpecifier = ExplicitInterfaceSpecifier(explicitInterfaceName, qualifiedName.DotToken, identifierToken);
                return (explicitInterfaceSpecifier, Token(SyntaxKind.None));
            }
        }

        checkpoint.Dispose();

        SyntaxToken identifier;
        if (CanTokenBeIdentifier(PeekToken()))
        {
            identifier = ReadIdentifierToken();
        }
        else if (!ConsumeToken(SyntaxKind.SelfKeyword, out identifier))
        {
            identifier = MissingToken(SyntaxKind.IdentifierToken);
        }

        return (null, identifier);
    }

    private PropertyDeclarationSyntax ParsePropertyDeclaration(SyntaxList modifiers)
    {
        var (explicitInterfaceSpecifier, identifier) = ParseMemberNameWithExplicitInterface();

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

        return PropertyDeclaration(modifiers, explicitInterfaceSpecifier, identifier, typeAnnotation, accessorList, null, terminatorToken);
    }

    private IndexerDeclarationSyntax ParseIndexerDeclaration(SyntaxList modifiers)
    {
        var (explicitInterfaceSpecifier, identifier) = ParseMemberNameWithExplicitInterface();

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

        return IndexerDeclaration(modifiers, explicitInterfaceSpecifier, identifier, parameterList, typeAnnotation, accessorList, null, terminatorToken);
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
                name = MissingToken(SyntaxKind.GetKeyword);
            }

            var token = PeekToken();

            BlockStatementSyntax? body = null;
            ArrowExpressionClauseSyntax? expressionBody = null;

            if (token.IsKind(SyntaxKind.OpenBraceToken))
            {
                body = new StatementSyntaxParser(this).ParseBlockStatementSyntax();
            }
            else if (token.IsKind(SyntaxKind.FatArrowToken))
            {
                expressionBody = new ExpressionSyntaxParser(this).ParseArrowExpressionClause();
            }

            SetTreatNewlinesAsTokens(true);

            TryConsumeTerminator(out var terminatorToken);

            SetTreatNewlinesAsTokens(false);

            var accessorKind = name.IsKind(SyntaxKind.GetKeyword)
                ? SyntaxKind.GetAccessorDeclaration
                : SyntaxKind.SetAccessorDeclaration;

            if (expressionBody is not null)
            {
                accessorList.Add(AccessorDeclaration(accessorKind, modifiers, name, null, expressionBody, terminatorToken));
            }
            else if (body is not null)
            {
                accessorList.Add(AccessorDeclaration(accessorKind, modifiers, name, body, null, terminatorToken));
            }
            else
            {
                accessorList.Add(AccessorDeclaration(accessorKind, modifiers, name, null, null, terminatorToken));
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

            SyntaxToken name;
            if (CanTokenBeIdentifier(PeekToken()))
            {
                name = ReadIdentifierToken();
            }
            else
            {
                name = ExpectToken(SyntaxKind.IdentifierToken);
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

        TryConsumeTerminator(out var terminatorToken);

        return FieldDeclaration(modifiers, declaration, terminatorToken, Diagnostics);
    }

    private VariableDeclarationSyntax? ParseVariableDeclarationSyntax()
    {
        var firstToken = ReadToken();

        SyntaxToken letOrVarKeyword;
        SyntaxToken identifier;

        if (firstToken.Kind is SyntaxKind.LetKeyword or SyntaxKind.VarKeyword)
        {
            letOrVarKeyword = firstToken;

            if (CanTokenBeIdentifier(PeekToken()))
            {
                identifier = ReadIdentifierToken();
            }
            else
            {
                identifier = ExpectToken(SyntaxKind.IdentifierToken);
            }
        }
        else
        {
            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.FieldDeclarationRequiresLetOrVar,
                    GetSpanOfLastToken()));

            letOrVarKeyword = MissingToken(SyntaxKind.LetKeyword);

            if (CanTokenBeIdentifier(firstToken))
            {
                identifier = ToIdentifierToken(firstToken);
                UpdateLastToken(identifier);
            }
            else if (CanTokenBeIdentifier(PeekToken()))
            {
                identifier = ReadIdentifierToken();
            }
            else
            {
                identifier = ExpectToken(SyntaxKind.IdentifierToken);
            }
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

            SyntaxToken name;
            if (CanTokenBeIdentifier(PeekToken()))
            {
                name = ReadIdentifierToken();
            }
            else
            {
                name = ExpectToken(SyntaxKind.IdentifierToken);
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
        return token.Kind is SyntaxKind.NewLineToken or SyntaxKind.LineFeedToken or SyntaxKind.CarriageReturnToken or SyntaxKind.CarriageReturnLineFeedToken;
    }
}
