namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

internal class CompilationUnitParser : SyntaxParser
{
    public CompilationUnitParser(ParseContext parent) : base(parent)
    {

    }

    public CompilationUnitSyntax Parse()
    {
        List<ImportDirectiveSyntax> importDirectives = [];
        List<MemberDeclarationSyntax> memberDeclarations = [];

        SyntaxToken nextToken;

        while (!ConsumeToken(SyntaxKind.EndOfFileToken, out nextToken))
        {
            var memberDeclaration = new NamespaceDeclarationParser(this).ParseNamespaceDeclaration();
            memberDeclarations.Add(memberDeclaration);
        }

        return CompilationUnit(List(importDirectives), List(memberDeclarations), nextToken);
    }
}
