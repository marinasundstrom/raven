namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;
using System.Collections.Generic;

using Raven.CodeAnalysis.Syntax.InternalSyntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using static SyntaxFactory;

internal class TypeDeclarationParser : SyntaxParser
{
    internal static SyntaxKind PeekTypeKeyword(ParseContext context)
    {
        using var checkpoint = context.CreateCheckpoint("TypeDeclarationPeek");

        var parser = new TypeDeclarationParser(context);
        _ = AttributeDeclarationParser.ParseAttributeLists(parser);
        _ = parser.ParseModifiers();

        return context.PeekToken().Kind;
    }

    public TypeDeclarationParser(ParseContext context) : base(context)
    {

    }

    internal BaseTypeDeclarationSyntax Parse()
    {
        var attributeLists = AttributeDeclarationParser.ParseAttributeLists(this);

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

        var constraintClauses = new ConstrainClauseListParser(this).ParseConstraintClauseList();

        List<GreenNode> memberList = new List<GreenNode>();

        ConsumeTokenOrMissing(SyntaxKind.OpenBraceToken, out var openBraceToken);

        SyntaxToken closeBraceToken;
        if (openBraceToken.IsMissing && typeKeyword.IsKind(SyntaxKind.ClassKeyword) && modifiers.GetChildren().Any(x => x.IsKind(SyntaxKind.RecordKeyword)))
        {
            closeBraceToken = MissingToken(SyntaxKind.CloseBraceToken);
        }
        else
        {
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

            ConsumeTokenOrMissing(SyntaxKind.CloseBraceToken, out closeBraceToken);
        }

        var terminatorToken = ConsumeOptionalTypeTerminator();

        if (typeKeyword.IsKind(SyntaxKind.InterfaceKeyword))
        {
            return InterfaceDeclaration(attributeLists, modifiers, typeKeyword, identifier, typeParameterList, baseList, null, constraintClauses, openBraceToken, List(memberList), closeBraceToken, terminatorToken);
        }

        return ClassDeclaration(attributeLists, modifiers, typeKeyword, identifier, typeParameterList, baseList, parameterList, constraintClauses, openBraceToken, List(memberList), closeBraceToken, terminatorToken);
    }

    internal TypeParameterListSyntax ParseTypeParameterList()
    {
        var lessThanToken = ReadToken();

        List<GreenNode> parameters = new List<GreenNode>();

        var parsedParameters = 0;
        var restoreNewlinesAsTokens = TreatNewlinesAsTokens;
        SetTreatNewlinesAsTokens(false);

        SyntaxToken greaterThanToken;

        try
        {
            while (true)
            {
                var token = PeekToken();

                while (IsNewLineLike(token))
                {
                    ReadToken();
                    token = PeekToken();
                }

                if (token.IsKind(SyntaxKind.EndOfFileToken) ||
                    token.IsKind(SyntaxKind.GreaterThanToken))
                {
                    break;
                }

                if (parsedParameters > 0)
                {
                    if (token.IsKind(SyntaxKind.CommaToken))
                    {
                        var commaToken = ReadToken();
                        parameters.Add(commaToken);
                        token = PeekToken();
                    }
                    else
                    {
                        AddDiagnostic(
                            DiagnosticInfo.Create(
                                CompilerDiagnostics.CharacterExpected,
                                GetSpanOfLastToken(),
                                ","));
                    }
                }

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
                        var constraint = new ConstrainClauseListParser(this).ParseTypeParameterConstraint();
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
                parsedParameters++;
            }

            ConsumeTokenOrMissing(SyntaxKind.GreaterThanToken, out greaterThanToken);
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

        return TypeParameterList(lessThanToken, List(parameters), greaterThanToken, Diagnostics);
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
        return ParseTypeMemberModifiers();
    }

    private MemberDeclarationSyntax ParseMember()
    {
        var memberDeclarationCheckpoint = CreateCheckpoint();

        var attributeLists = AttributeDeclarationParser.ParseAttributeLists(this);
        var modifiers = ParseModifiers();

        var keywordOrIdentifier = PeekToken();

        if (keywordOrIdentifier.IsKind(SyntaxKind.OpenBracketToken))
        {
            var typeKeywordKind = PeekTypeKeyword(this);

            if (typeKeywordKind is SyntaxKind.ClassKeyword or SyntaxKind.InterfaceKeyword)
            {
                memberDeclarationCheckpoint.Rewind();
                return new TypeDeclarationParser(this).Parse();
            }

            if (typeKeywordKind == SyntaxKind.EnumKeyword)
            {
                memberDeclarationCheckpoint.Rewind();
                return new EnumDeclarationParser(this).Parse();
            }

            if (typeKeywordKind == SyntaxKind.UnionKeyword)
            {
                memberDeclarationCheckpoint.Rewind();
                return new UnionDeclarationParser(this).Parse();
            }
        }

        if (keywordOrIdentifier.IsKind(SyntaxKind.ClassKeyword) || keywordOrIdentifier.IsKind(SyntaxKind.InterfaceKeyword))
        {
            memberDeclarationCheckpoint.Rewind();
            return new TypeDeclarationParser(this).Parse();
        }

        if (keywordOrIdentifier.IsKind(SyntaxKind.EnumKeyword))
        {
            memberDeclarationCheckpoint.Rewind();
            return new EnumDeclarationParser(this).Parse();
        }

        if (keywordOrIdentifier.IsKind(SyntaxKind.UnionKeyword))
        {
            memberDeclarationCheckpoint.Rewind();
            return new UnionDeclarationParser(this).Parse();
        }

        if ((keywordOrIdentifier.IsKind(SyntaxKind.ExplicitKeyword) || keywordOrIdentifier.IsKind(SyntaxKind.ImplicitKeyword))
            && PeekToken(1).IsKind(SyntaxKind.OperatorKeyword))
        {
            return ParseConversionOperatorDeclaration(attributeLists, modifiers);
        }

        if (keywordOrIdentifier.IsKind(SyntaxKind.OperatorKeyword))
        {
            return ParseOperatorDeclaration(attributeLists, modifiers);
        }

        if (keywordOrIdentifier.IsKind(SyntaxKind.EventKeyword))
        {
            return ParseEventDeclaration(attributeLists, modifiers);
        }

        if (keywordOrIdentifier.IsKind(SyntaxKind.LetKeyword) || keywordOrIdentifier.IsKind(SyntaxKind.ValKeyword) || keywordOrIdentifier.IsKind(SyntaxKind.VarKeyword) || keywordOrIdentifier.IsKind(SyntaxKind.ConstKeyword))
        {
            return ParseFieldDeclarationSyntax(attributeLists, modifiers);
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

            checkpoint.Rewind();

            if (looksLikeField)
            {
                return ParseFieldDeclarationSyntax(attributeLists, modifiers);
            }
        }

        if (keywordOrIdentifier.IsKind(SyntaxKind.InitKeyword))
        {
            return ParseConstructorDeclaration(attributeLists, modifiers, keywordOrIdentifier);
        }

        var nameCheckpoint = CreateCheckpoint();
        _ = ParseMemberNameWithExplicitInterface();
        var tokenAfterName = PeekToken();

        if (tokenAfterName.IsKind(SyntaxKind.LessThanToken))
        {
            var typeParameterCheckpoint = CreateCheckpoint();
            _ = ParseTypeParameterList();
            var tokenAfterTypeParameters = PeekToken();
            typeParameterCheckpoint.Rewind();

            if (tokenAfterTypeParameters.IsKind(SyntaxKind.OpenParenToken))
            {
                nameCheckpoint.Rewind();
                return ParseMethodOrConstructorDeclarationBase(attributeLists, modifiers);
            }
        }

        nameCheckpoint.Rewind();

        if (tokenAfterName.IsKind(SyntaxKind.OpenParenToken))
        {
            return ParseMethodOrConstructorDeclarationBase(attributeLists, modifiers);
        }

        if (tokenAfterName.IsKind(SyntaxKind.OpenBracketToken))
        {
            return ParseIndexerDeclaration(attributeLists, modifiers);
        }

        return ParsePropertyDeclaration(attributeLists, modifiers);
    }

    private MemberDeclarationSyntax ParseOperatorDeclaration(SyntaxList attributeLists, SyntaxList modifiers)
    {
        var operatorKeyword = ReadToken();
        var operatorToken = ParseOverloadableOperatorToken();

        ConsumeTokenOrMissing(SyntaxKind.OpenParenToken, out var openParenToken);
        var parameterList = ParseParameterList(openParenToken);

        var returnType = new TypeAnnotationClauseSyntaxParser(this).ParseReturnTypeAnnotation();

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

        return OperatorDeclaration(
            attributeLists,
            modifiers,
            operatorKeyword,
            operatorToken,
            parameterList,
            returnType,
            body,
            expressionBody,
            terminatorToken);
    }

    private MemberDeclarationSyntax ParseConversionOperatorDeclaration(SyntaxList attributeLists, SyntaxList modifiers)
    {
        var conversionKindKeyword = ReadToken();
        var operatorKeyword = ExpectToken(SyntaxKind.OperatorKeyword);

        ConsumeTokenOrMissing(SyntaxKind.OpenParenToken, out var openParenToken);
        var parameterList = ParseParameterList(openParenToken);

        var returnType = new TypeAnnotationClauseSyntaxParser(this).ParseReturnTypeAnnotation();

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

        return ConversionOperatorDeclaration(
            attributeLists,
            modifiers,
            conversionKindKeyword,
            operatorKeyword,
            parameterList,
            returnType,
            body,
            expressionBody,
            terminatorToken);
    }

    private MemberDeclarationSyntax ParseConstructorDeclaration(SyntaxList attributeLists, SyntaxList modifiers, SyntaxToken initKeyword)
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
                return ConstructorDeclaration(attributeLists, modifiers, initKeyword, parameterList, initializer, null, expressionBody, terminatorToken);
            }
            else if (body is not null)
            {
                return ConstructorDeclaration(attributeLists, modifiers, initKeyword, parameterList, initializer, body, null, terminatorToken);
            }
        }
        else
        {
            if (expressionBody is not null)
            {
                return NamedConstructorDeclaration(attributeLists, modifiers, initKeyword, identifier, parameterList, null, expressionBody, terminatorToken);
            }
            else if (body is not null)
            {
                return NamedConstructorDeclaration(attributeLists, modifiers, initKeyword, identifier, parameterList, body, null, terminatorToken);
            }
        }

        throw new Exception();
    }

    private MemberDeclarationSyntax ParseMethodOrConstructorDeclarationBase(SyntaxList attributeLists, SyntaxList modifiers)
    {
        var (explicitInterfaceSpecifier, identifier) = ParseMemberNameWithExplicitInterface();

        var potentialOpenParenToken = PeekToken();

        if (potentialOpenParenToken.IsKind(SyntaxKind.OpenParenToken))
        {
            return ParseMethodOrConstructorDeclaration(attributeLists, modifiers, explicitInterfaceSpecifier, identifier);
        }

        if (potentialOpenParenToken.IsKind(SyntaxKind.LessThanToken))
        {
            var typeParameterCheckpoint = CreateCheckpoint();
            _ = ParseTypeParameterList();
            var tokenAfterTypeParameters = PeekToken();
            typeParameterCheckpoint.Rewind();

            if (tokenAfterTypeParameters.IsKind(SyntaxKind.OpenParenToken))
            {
                return ParseMethodOrConstructorDeclaration(attributeLists, modifiers, explicitInterfaceSpecifier, identifier);
            }
        }

        // Remove below

        throw new Exception();
    }

    private MemberDeclarationSyntax ParseMethodOrConstructorDeclaration(
        SyntaxList attributeLists,
        SyntaxList modifiers,
        ExplicitInterfaceSpecifierSyntax? explicitInterfaceSpecifier,
        SyntaxToken identifier)
    {
        TypeParameterListSyntax? typeParameterList = null;
        if (PeekToken().IsKind(SyntaxKind.LessThanToken))
        {
            typeParameterList = ParseTypeParameterList();
        }

        var parameterList = ParseParameterList();

        var returnParameterAnnotation = new TypeAnnotationClauseSyntaxParser(this).ParseReturnTypeAnnotation();

        if (identifier.IsKind(SyntaxKind.InitKeyword))
        {
            // Report type annotation
            // ConstructorDeclaration
        }

        var constraintClauses = new ConstrainClauseListParser(this).ParseConstraintClauseList();

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
            return MethodDeclaration(attributeLists, modifiers, explicitInterfaceSpecifier, identifier, typeParameterList, parameterList, returnParameterAnnotation, constraintClauses, null, expressionBody, terminatorToken);
        }
        else if (body is not null)
        {
            return MethodDeclaration(attributeLists, modifiers, explicitInterfaceSpecifier, identifier, typeParameterList, parameterList, returnParameterAnnotation, constraintClauses, body, null, terminatorToken);
        }

        return MethodDeclaration(attributeLists, modifiers, explicitInterfaceSpecifier, identifier, typeParameterList, parameterList, returnParameterAnnotation, constraintClauses, null, null, terminatorToken);
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

        checkpoint.Rewind();

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

    private PropertyDeclarationSyntax ParsePropertyDeclaration(SyntaxList attributeLists, SyntaxList modifiers)
    {
        var (explicitInterfaceSpecifier, identifier) = ParseMemberNameWithExplicitInterface();

        var typeAnnotation = new TypeAnnotationClauseSyntaxParser(this).ParseTypeAnnotation()
            ?? CreateMissingTypeAnnotationClause();

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

        ArrowExpressionClauseSyntax? expressionBody = null;
        if (PeekToken().IsKind(SyntaxKind.FatArrowToken))
            expressionBody = new ExpressionSyntaxParser(this).ParseArrowExpressionClause();

        EqualsValueClauseSyntax? initializer = null;
        if (IsNextToken(SyntaxKind.EqualsToken, out _))
        {
            initializer = new EqualsValueClauseSyntaxParser(this).Parse();
        }

        TryConsumeTerminator(out var terminatorToken);

        return PropertyDeclaration(attributeLists, modifiers, explicitInterfaceSpecifier, identifier, typeAnnotation, accessorList, expressionBody, initializer, terminatorToken);
    }

    private EventDeclarationSyntax ParseEventDeclaration(SyntaxList attributeLists, SyntaxList modifiers)
    {
        var eventKeyword = ReadToken();
        var (explicitInterfaceSpecifier, identifier) = ParseMemberNameWithExplicitInterface();

        var typeAnnotation = new TypeAnnotationClauseSyntaxParser(this).ParseTypeAnnotation()
            ?? CreateMissingTypeAnnotationClause();

        AccessorListSyntax? accessorList = null;
        if (PeekToken().IsKind(SyntaxKind.OpenBraceToken))
        {
            accessorList = ParseAccessorList();
        }

        TryConsumeTerminator(out var terminatorToken);

        return EventDeclaration(
            attributeLists,
            modifiers,
            eventKeyword,
            explicitInterfaceSpecifier,
            identifier,
            typeAnnotation,
            accessorList,
            terminatorToken);
    }

    private TypeAnnotationClauseSyntax CreateMissingTypeAnnotationClause()
    {
        var colonToken = MissingToken(SyntaxKind.ColonToken);
        var missingType = IdentifierName(MissingToken(SyntaxKind.IdentifierToken));
        return TypeAnnotationClause(colonToken, missingType);
    }

    private IndexerDeclarationSyntax ParseIndexerDeclaration(SyntaxList attributeLists, SyntaxList modifiers)
    {
        var (explicitInterfaceSpecifier, identifier) = ParseMemberNameWithExplicitInterface();

        var parameterList = ParseBracketedParameterList();

        var typeAnnotation = new TypeAnnotationClauseSyntaxParser(this).ParseTypeAnnotation()
            ?? CreateMissingTypeAnnotationClause();

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

        ArrowExpressionClauseSyntax? expressionBody = null;
        if (PeekToken().IsKind(SyntaxKind.FatArrowToken))
            expressionBody = new ExpressionSyntaxParser(this).ParseArrowExpressionClause();

        TryConsumeTerminator(out var terminatorToken);

        return IndexerDeclaration(attributeLists, modifiers, explicitInterfaceSpecifier, identifier, parameterList, typeAnnotation, accessorList, expressionBody, null, terminatorToken);
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

            var attributeLists = AttributeDeclarationParser.ParseAttributeLists(this);
            SyntaxList modifiers = SyntaxList.Empty;
            SyntaxToken modifier;

            while (true)
            {
                if (ConsumeToken(SyntaxKind.AsyncKeyword, out modifier) ||
                    ConsumeToken(SyntaxKind.PublicKeyword, out modifier) ||
                    ConsumeToken(SyntaxKind.PrivateKeyword, out modifier) ||
                    ConsumeToken(SyntaxKind.ProtectedKeyword, out modifier) ||
                    ConsumeToken(SyntaxKind.InternalKeyword, out modifier) ||
                    ConsumeToken(SyntaxKind.RefKeyword, out modifier) ||
                    ConsumeToken(SyntaxKind.OutKeyword, out modifier) ||
                    ConsumeToken(SyntaxKind.InKeyword, out modifier))
                {
                    modifiers = modifiers.Add(modifier);
                    continue;
                }

                break;
            }

            SyntaxToken name;

            if (!ConsumeToken(SyntaxKind.GetKeyword, out name) &&
                !ConsumeToken(SyntaxKind.SetKeyword, out name) &&
                !ConsumeToken(SyntaxKind.AddKeyword, out name) &&
                !ConsumeToken(SyntaxKind.RemoveKeyword, out name))
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

            var accessorKind = name.Kind switch
            {
                SyntaxKind.GetKeyword => SyntaxKind.GetAccessorDeclaration,
                SyntaxKind.SetKeyword => SyntaxKind.SetAccessorDeclaration,
                SyntaxKind.AddKeyword => SyntaxKind.AddAccessorDeclaration,
                SyntaxKind.RemoveKeyword => SyntaxKind.RemoveAccessorDeclaration,
                _ => SyntaxKind.GetAccessorDeclaration,
            };

            if (expressionBody is not null)
            {
                accessorList.Add(AccessorDeclaration(accessorKind, attributeLists, modifiers, name, null, expressionBody, terminatorToken));
            }
            else if (body is not null)
            {
                accessorList.Add(AccessorDeclaration(accessorKind, attributeLists, modifiers, name, body, null, terminatorToken));
            }
            else
            {
                accessorList.Add(AccessorDeclaration(accessorKind, attributeLists, modifiers, name, null, null, terminatorToken));
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseBraceToken, out var closeBraceToken);

        SetTreatNewlinesAsTokens(false);

        return AccessorList(openBraceToken, List(accessorList.ToArray()), closeBraceToken);
    }

    public ParameterListSyntax ParseParameterList(SyntaxToken? openParenToken = null)
    {
        var openParenTokenValue = openParenToken ?? ReadToken();

        List<GreenNode> parameterList = new List<GreenNode>();

        var parsedParameters = 0;
        var restoreNewlinesAsTokens = TreatNewlinesAsTokens;
        SetTreatNewlinesAsTokens(false);

        SyntaxToken closeParenToken;

        try
        {
            while (true)
            {
                var t = PeekToken();

                while (IsNewLineLike(t))
                {
                    ReadToken();
                    t = PeekToken();
                }

                if (t.IsKind(SyntaxKind.EndOfFileToken) ||
                    t.IsKind(SyntaxKind.CloseParenToken))
                {
                    break;
                }

                if (parsedParameters > 0)
                {
                    if (t.IsKind(SyntaxKind.CommaToken))
                    {
                        var commaToken = ReadToken();
                        parameterList.Add(commaToken);
                    }
                    else
                    {
                        AddDiagnostic(
                            DiagnosticInfo.Create(
                                CompilerDiagnostics.CharacterExpected,
                                GetSpanOfLastToken(),
                                ","));
                    }
                }

                var attributeLists = AttributeDeclarationParser.ParseAttributeLists(this);

                SyntaxToken? refKindKeyword = null;
                if (PeekToken().Kind is SyntaxKind.RefKeyword or SyntaxKind.OutKeyword or SyntaxKind.InKeyword)
                    refKindKeyword = ReadToken();

                SyntaxToken? bindingKeyword = null;
                if (PeekToken().Kind is SyntaxKind.LetKeyword or SyntaxKind.ValKeyword or SyntaxKind.VarKeyword or SyntaxKind.ConstKeyword)
                    bindingKeyword = ReadToken();

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

                EqualsValueClauseSyntax? defaultValue = null;
                if (IsNextToken(SyntaxKind.EqualsToken, out _))
                {
                    defaultValue = new EqualsValueClauseSyntaxParser(this).Parse();
                }

                parameterList.Add(Parameter(attributeLists, refKindKeyword, bindingKeyword, name, typeAnnotation, defaultValue));
                parsedParameters++;
            }

            ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out closeParenToken);
        }
        finally
        {
            SetTreatNewlinesAsTokens(restoreNewlinesAsTokens);
        }

        if (closeParenToken.IsMissing)
        {
            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.CharacterExpected,
                    GetSpanOfLastToken(),
                    ")"));
        }

        return ParameterList(openParenTokenValue, List(parameterList.ToArray()), closeParenToken, Diagnostics);
    }

    private FieldDeclarationSyntax ParseFieldDeclarationSyntax(SyntaxList attributeLists, SyntaxList modifiers)
    {
        var declaration = ParseVariableDeclarationSyntax();

        SetTreatNewlinesAsTokens(true);

        TryConsumeTerminator(out var terminatorToken);

        return FieldDeclaration(attributeLists, modifiers, declaration, terminatorToken, Diagnostics);
    }

    private VariableDeclarationSyntax? ParseVariableDeclarationSyntax()
    {
        var firstToken = ReadToken();

        SyntaxToken bindingKeyword;
        SyntaxToken identifier;

        if (firstToken.Kind is SyntaxKind.LetKeyword or SyntaxKind.ValKeyword or SyntaxKind.VarKeyword or SyntaxKind.ConstKeyword)
        {
            bindingKeyword = firstToken;

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
                    CompilerDiagnostics.FieldDeclarationRequiresBindingKeyword,
                    GetSpanOfLastToken()));

            bindingKeyword = MissingToken(SyntaxKind.LetKeyword);

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

        return new VariableDeclarationSyntax(bindingKeyword, declarators);
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

        var parsedParameters = 0;
        var restoreNewlinesAsTokens = TreatNewlinesAsTokens;
        SetTreatNewlinesAsTokens(false);

        SyntaxToken closeBracketToken;

        try
        {
            while (true)
            {
                var t = PeekToken();

                while (IsNewLineLike(t))
                {
                    ReadToken();
                    t = PeekToken();
                }

                if (t.IsKind(SyntaxKind.EndOfFileToken) ||
                    t.IsKind(SyntaxKind.CloseBracketToken))
                {
                    break;
                }

                if (parsedParameters > 0)
                {
                    if (t.IsKind(SyntaxKind.CommaToken))
                    {
                        var commaToken = ReadToken();
                        parameterList.Add(commaToken);
                    }
                    else
                    {
                        AddDiagnostic(
                            DiagnosticInfo.Create(
                                CompilerDiagnostics.CharacterExpected,
                                GetSpanOfLastToken(),
                                ","));
                    }
                }

                var attributeLists = AttributeDeclarationParser.ParseAttributeLists(this);

                SyntaxToken? refKindKeyword = null;
                if (PeekToken().Kind is SyntaxKind.RefKeyword or SyntaxKind.OutKeyword or SyntaxKind.InKeyword)
                    refKindKeyword = ReadToken();

                SyntaxToken? bindingKeyword = null;
                if (PeekToken().Kind is SyntaxKind.LetKeyword or SyntaxKind.ValKeyword or SyntaxKind.VarKeyword or SyntaxKind.ConstKeyword)
                    bindingKeyword = ReadToken();

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

                EqualsValueClauseSyntax? defaultValue = null;
                if (IsNextToken(SyntaxKind.EqualsToken, out _))
                {
                    defaultValue = new EqualsValueClauseSyntaxParser(this).Parse();
                }

                parameterList.Add(Parameter(attributeLists, refKindKeyword, bindingKeyword, name, typeAnnotation, defaultValue));
                parsedParameters++;
            }

            ConsumeTokenOrMissing(SyntaxKind.CloseBracketToken, out closeBracketToken);
        }
        finally
        {
            SetTreatNewlinesAsTokens(restoreNewlinesAsTokens);
        }

        if (closeBracketToken.IsMissing)
        {
            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.CharacterExpected,
                    GetSpanOfLastToken(),
                    "]"));
        }

        return BracketedParameterList(openBracketToken, List(parameterList.ToArray()), closeBracketToken, Diagnostics);
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
