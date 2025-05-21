
namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

internal class NamespaceDeclarationParser : SyntaxParser
{
    public NamespaceDeclarationParser(SyntaxParser parent) : base(parent)
    {

    }

    public MemberDeclarationSyntax ParseNamespaceDeclaration()
    {
        List<ImportDirectiveSyntax> importDirectives = [];
        List<MemberDeclarationSyntax> memberDeclarations = [];

        List<DiagnosticInfo>? diagnostics = null;

        var namespaceKeyword = ReadToken();

        var name = new NameParser(this).ParseName();

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

            ConsumeTokenOrNull(SyntaxKind.SemicolonToken, out var semicolonToken);

            return NamespaceDeclaration(
                namespaceKeyword, name, openBraceToken,
                new SyntaxList(importDirectives.ToArray()), new SyntaxList(memberDeclarations.ToArray()),
                closeBraceToken, semicolonToken, diagnostics);
        }

        return ParseFileScopedNamespaceDeclarationCore(namespaceKeyword, name, importDirectives, memberDeclarations);
    }

    private MemberDeclarationSyntax ParseFileScopedNamespaceDeclarationCore(SyntaxToken namespaceKeyword, NameSyntax name, List<ImportDirectiveSyntax> importDirectives, List<MemberDeclarationSyntax> memberDeclarations)
    {
        DiagnosticInfo[]? diagnostics = null;

        if (!ConsumeTokenOrMissing(SyntaxKind.SemicolonToken, out var semicolonToken))
        {
            diagnostics = [
                DiagnosticInfo.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    GetEndOfLastToken()
                ) ];
        }

        var fileScopedNamespaceDeclaration = FileScopedNamespaceDeclaration(
            namespaceKeyword, name, semicolonToken,
            SyntaxList.Empty, SyntaxList.Empty, diagnostics);

        while (!IsNextToken(SyntaxKind.EndOfFileToken, out var nextToken))
        {
            ParseNamespaceMemberDeclarations(nextToken, importDirectives, memberDeclarations);
        }

        return fileScopedNamespaceDeclaration;
    }

    private void ParseNamespaceMemberDeclarations(SyntaxToken nextToken, List<ImportDirectiveSyntax> importDirectives, List<MemberDeclarationSyntax> memberDeclarations)
    {
        if (nextToken.Kind == SyntaxKind.ImportKeyword)
        {
            var importDirective = ParseImportDirective();

            importDirectives.Add(importDirective);
        }
        else if (nextToken.Kind == SyntaxKind.NamespaceKeyword)
        {
            var namespaceDeclaration = new NamespaceDeclarationParser(this).ParseNamespaceDeclaration();

            memberDeclarations.Add(namespaceDeclaration);
        }
        else
        {
            // Should warn (?)

            var statement = new StatementParser(this).ParseStatement();

            if (statement is null)
                return;

            var globalStatement = GlobalStatement(statement);

            memberDeclarations.Add(globalStatement);
        }
    }

    private ImportDirectiveSyntax ParseImportDirective()
    {
        var importKeyword = ReadToken();

        var namespaceName = new NameParser(this).ParseName();

        if (!ConsumeTokenOrMissing(SyntaxKind.SemicolonToken, out var semicolonToken))
        {
            return ImportDirective(importKeyword, namespaceName, semicolonToken,
                [DiagnosticInfo.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    GetEndOfLastToken()
                )]);
        }

        return ImportDirective(importKeyword, namespaceName, semicolonToken);
    }
}