namespace Raven.CodeAnalysis.Syntax;

public partial class CompilationUnitSyntax : SyntaxNode
{
    public CompilationUnitSyntax(SyntaxTree syntaxTree, SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<AliasDirectiveSyntax> aliases, SyntaxList<MemberDeclarationSyntax> members, SyntaxToken endOfFileToken)
        : base(new InternalSyntax.CompilationUnitSyntax(imports.Green, aliases.Green, members.Green, endOfFileToken.Green), syntaxTree)
    {
    }

    private CompilationUnitSyntax(SyntaxTree syntaxTree, InternalSyntax.CompilationUnitSyntax green)
        : base(green, syntaxTree)
    {
    }

    internal CompilationUnitSyntax WithSyntaxTree(SyntaxTree syntaxTree)
    {
        return new CompilationUnitSyntax(syntaxTree, (InternalSyntax.CompilationUnitSyntax)Green);
    }
}
