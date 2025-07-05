
namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

internal class NamespaceDeclarationParser : SyntaxParser
{
    public NamespaceDeclarationParser(ParseContext context) : base(context)
    {

    }

    public MemberDeclarationSyntax ParseNamespaceDeclaration()
    {
        List<ImportDirectiveSyntax> importDirectives = [];
        List<MemberDeclarationSyntax> memberDeclarations = [];

        var namespaceKeyword = ReadToken();

        var name = new NameSyntaxParser(this).ParseName();

        if (ConsumeToken(SyntaxKind.OpenBraceToken, out var openBraceToken))
        {
            while (!IsNextToken(SyntaxKind.EndOfFileToken, out var nextToken) && nextToken.Kind != SyntaxKind.CloseBraceToken)
            {
                ParseNamespaceMemberDeclarations(nextToken, importDirectives, memberDeclarations);
            }

            if (!ConsumeTokenOrMissing(SyntaxKind.CloseBraceToken, out var closeBraceToken))
            {
                AddDiagnostic(
                    DiagnosticInfo.Create(
                        CompilerDiagnostics.CharacterExpected,
                        GetEndOfLastToken(),
                        ['}']
                    ));
            }

            ConsumeTokenOrNull(SyntaxKind.SemicolonToken, out var terminatorToken);

            return NamespaceDeclaration(
                SyntaxList.Empty,
                namespaceKeyword, name, openBraceToken,
                new SyntaxList(importDirectives.ToArray()), new SyntaxList(memberDeclarations.ToArray()),
                closeBraceToken, terminatorToken, Diagnostics);
        }

        return ParseFileScopedNamespaceDeclarationCore(namespaceKeyword, name, importDirectives, memberDeclarations);
    }

    private MemberDeclarationSyntax ParseFileScopedNamespaceDeclarationCore(SyntaxToken namespaceKeyword, NameSyntax name, List<ImportDirectiveSyntax> importDirectives, List<MemberDeclarationSyntax> memberDeclarations)
    {
        DiagnosticInfo[]? diagnostics = null;

        SetTreatNewlinesAsTokens(true);

        TryConsumeTerminator(out var terminatorToken);

        SetTreatNewlinesAsTokens(false);

        while (!IsNextToken(SyntaxKind.EndOfFileToken, out var nextToken))
        {
            ParseNamespaceMemberDeclarations(nextToken, importDirectives, memberDeclarations);

            SetTreatNewlinesAsTokens(false);
        }

        return FileScopedNamespaceDeclaration(
            SyntaxList.Empty,
            namespaceKeyword, name, terminatorToken,
            List(importDirectives), List(memberDeclarations), diagnostics);
    }

    private void ParseNamespaceMemberDeclarations(SyntaxToken nextToken, List<ImportDirectiveSyntax> importDirectives, List<MemberDeclarationSyntax> memberDeclarations)
    {
        if (nextToken.IsKind(SyntaxKind.ImportKeyword))
        {
            var importDirective = new ImportDirectiveSyntaxParser(this).ParseImportDirective();

            importDirectives.Add(importDirective);
        }
        else if (nextToken.IsKind(SyntaxKind.NamespaceKeyword))
        {
            var namespaceDeclaration = new NamespaceDeclarationParser(this).ParseNamespaceDeclaration();

            memberDeclarations.Add(namespaceDeclaration);
        }
        else if (nextToken.IsKind(SyntaxKind.EnumKeyword))
        {
            var enumDeclaration = new EnumDeclarationParser(this).Parse();

            memberDeclarations.Add(enumDeclaration);
        }
        else if (nextToken.IsKind(SyntaxKind.StructKeyword) || nextToken.IsKind(SyntaxKind.ClassKeyword))
        {
            var typeDeclaration = new TypeDeclarationParser(this).Parse();

            memberDeclarations.Add(typeDeclaration);
        }
        else
        {
            // Should warn (?)

            var statement = new StatementSyntaxParser(this).ParseStatement();

            if (statement is null)
                return;

            var globalStatement = GlobalStatement(SyntaxList.Empty, statement);

            memberDeclarations.Add(globalStatement);
        }
    }
}