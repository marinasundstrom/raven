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
        var modifiers = SyntaxList.Empty;

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

    private MemberDeclarationSyntax ParseMember()
    {
        var keyword = PeekToken();

        if (keyword.IsKind(SyntaxKind.FuncKeyword))
        {
            return ParseFuntionDeclaration();
        }
        else if (keyword.IsKind(SyntaxKind.LetKeyword) || keyword.IsKind(SyntaxKind.VarKeyword))
        {
            return ParseFieldDeclarationSyntax();
        }

        return null;  //StructOrTypeMemberDeclaration(identifier);
    }

    private MemberDeclarationSyntax ParseFuntionDeclaration()
    {
        var funcKeyword = ReadToken();

        if (!ConsumeTokenOrMissing(SyntaxKind.IdentifierToken, out var identifier))
        {
            if (!ConsumeTokenOrMissing(SyntaxKind.InitKeyword, out identifier))
            {

            }
        }

        var parameterList = ParseParameterList();

        var returnParameterAnnotation = new TypeAnnotationSyntaxParser(this).ParseReturnTypeAnnotation();

        var block = new ExpressionSyntaxParser(this).ParseBlockSyntax();

        return MethodDeclaration(SyntaxList.Empty, funcKeyword, identifier, parameterList, returnParameterAnnotation, block);
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

    private FieldDeclarationSyntax ParseFieldDeclarationSyntax()
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

        return FieldDeclaration(SyntaxList.Empty, declaration, terminatorToken, Diagnostics);
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