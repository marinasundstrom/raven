namespace Raven.CodeAnalysis.Syntax;

public partial class CompilationUnitSyntax : SyntaxNode
{
    public CompilationUnitSyntax(SyntaxTree syntaxTree, SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members, SyntaxToken endOfFileToken)
        : base(new InternalSyntax.CompilationUnitSyntax(imports.Green, members.Green, endOfFileToken.Green), syntaxTree)
    {
    }

    internal CompilationUnitSyntax WithSyntaxTree(SyntaxTree syntaxTree)
    {
        return new CompilationUnitSyntax(syntaxTree, Imports, Members, EndOfFileToken);
    }
}
