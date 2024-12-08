

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Syntax;

public partial class CompilationUnitSyntax : SyntaxNode
{
    public partial SyntaxList<ImportDirectiveSyntax> Imports { get; }

    public partial SyntaxList<MemberDeclarationSyntax> Members { get; }

    public partial SyntaxToken EndOfFileToken { get; }

    public CompilationUnitSyntax(GreenNode greenNode, SyntaxNode parent) : base(greenNode, parent)
    {
    }

    public CompilationUnitSyntax(GreenNode greenNode, SyntaxTree syntaxTree) : base(greenNode, syntaxTree)
    {
    }

    public CompilationUnitSyntax()
    : this(new Syntax.InternalSyntax.CompilationUnitSyntax(null, null, SyntaxFactory.EndOfFileToken.Green), (SyntaxNode)null)
    {
    }

    public CompilationUnitSyntax(SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members)
        : this(new Syntax.InternalSyntax.CompilationUnitSyntax(imports?.Green, members?.Green, SyntaxFactory.EndOfFileToken.Green), (SyntaxNode)null)
    {
    }

    public CompilationUnitSyntax(SyntaxTree syntaxTree, SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members)
    : this(new Syntax.InternalSyntax.CompilationUnitSyntax(imports?.Green, members?.Green, SyntaxFactory.EndOfFileToken.Green), syntaxTree)
    {
    }

    public CompilationUnitSyntax WithImports(SyntaxList<ImportDirectiveSyntax> imports)
    {
        return new CompilationUnitSyntax(imports, Members);
    }

    public CompilationUnitSyntax WithMembers(SyntaxList<MemberDeclarationSyntax> members)
    {
        return new CompilationUnitSyntax(Imports, members);
    }

    public CompilationUnitSyntax NormalizeWhitespace()
    {
        return this;
    }

    public CompilationUnitSyntax WithRoot(SyntaxTree syntaxTree)
    {
        return new CompilationUnitSyntax(syntaxTree, Imports, Members);
    }
}