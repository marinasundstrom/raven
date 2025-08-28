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
        List<AliasDirectiveSyntax> aliasDirectives = [];
        List<MemberDeclarationSyntax> memberDeclarations = [];

        SyntaxToken nextToken;

        SetTreatNewlinesAsTokens(false);

        while (!ConsumeToken(SyntaxKind.EndOfFileToken, out nextToken))
        {
            ParseNamespaceMemberDeclarations(nextToken, importDirectives, aliasDirectives, memberDeclarations);

            SetTreatNewlinesAsTokens(false);
        }

        return CompilationUnit(List(importDirectives), List(aliasDirectives), List(memberDeclarations), nextToken);
    }

    private void ParseNamespaceMemberDeclarations(SyntaxToken nextToken, List<ImportDirectiveSyntax> importDirectives, List<AliasDirectiveSyntax> aliasDirectives, List<MemberDeclarationSyntax> memberDeclarations)
    {
        if (nextToken.IsKind(SyntaxKind.ImportKeyword))
        {
            var importDirective = new ImportDirectiveSyntaxParser(this).ParseImportDirective();

            importDirectives.Add(importDirective);
        }
        else if (nextToken.IsKind(Raven.CodeAnalysis.Syntax.SyntaxKind.AliasKeyword))
        {
            var aliasDirective = new AliasDirectiveSyntaxParser(this).ParseAliasDirective();

            aliasDirectives.Add(aliasDirective);
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
        else if (nextToken.IsKind(SyntaxKind.StructKeyword) || nextToken.IsKind(SyntaxKind.ClassKeyword) ||
                 nextToken.IsKind(SyntaxKind.PublicKeyword) || nextToken.IsKind(SyntaxKind.PrivateKeyword) ||
                 nextToken.IsKind(SyntaxKind.InternalKeyword) || nextToken.IsKind(SyntaxKind.ProtectedKeyword) ||
                 nextToken.IsKind(SyntaxKind.StaticKeyword) || nextToken.IsKind(SyntaxKind.AbstractKeyword) ||
                 nextToken.IsKind(SyntaxKind.SealedKeyword) || nextToken.IsKind(SyntaxKind.OpenKeyword) ||
                 nextToken.IsKind(SyntaxKind.OverrideKeyword))
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