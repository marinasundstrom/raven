namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

internal class CompilationUnitSyntaxParser : SyntaxParser
{
    public CompilationUnitSyntaxParser(ParseContext parent) : base(parent)
    {

    }

    public CompilationUnitSyntax Parse()
    {
        List<ImportDirectiveSyntax> importDirectives = [];
        List<MemberDeclarationSyntax> memberDeclarations = [];

        SyntaxToken nextToken;

        while (!ConsumeToken(SyntaxKind.EndOfFileToken, out nextToken))
        {
            ParseNamespaceMemberDeclarations(nextToken, importDirectives, memberDeclarations);
        }

        return CompilationUnit(List(importDirectives), List(memberDeclarations), nextToken);
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
        else
        {
            // Should warn (?)

            var statement = new StatementSyntaxParser(this).ParseStatement();

            if (statement is null)
                return;

            var globalStatement = GlobalStatement(statement);

            memberDeclarations.Add(globalStatement);
        }
    }
}