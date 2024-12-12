

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Syntax;

public partial class CompilationUnitSyntax : SyntaxNode
{
    public partial SyntaxList<ImportDirectiveSyntax> Imports { get; }

    public partial SyntaxList<MemberDeclarationSyntax> Members { get; }

    public partial SyntaxToken EndOfFileToken { get; }

    public CompilationUnitSyntax(GreenNode greenNode, SyntaxNode parent) : base(greenNode, parent, 0)
    {
    }

    public CompilationUnitSyntax(GreenNode greenNode, SyntaxTree syntaxTree) : base(greenNode, syntaxTree)
    {
    }

    public CompilationUnitSyntax()
    : this(new Syntax.InternalSyntax.CompilationUnitSyntax(null, null, SyntaxFactory.EndOfFile.Green), (SyntaxNode)null)
    {
    }

    public CompilationUnitSyntax(SyntaxTree syntaxTree, SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members)
    : this(new Syntax.InternalSyntax.CompilationUnitSyntax(imports?.Green, members?.Green, SyntaxFactory.EndOfFile.Green), syntaxTree)
    {
    }

    public CompilationUnitSyntax(SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members, SyntaxToken? endOfFileToken = null)
        : this(new Syntax.InternalSyntax.CompilationUnitSyntax(imports?.Green, members?.Green, endOfFileToken?.Green), (SyntaxTree)null)
    {
    }

    public CompilationUnitSyntax NormalizeWhitespace()
    {
        return (CompilationUnitSyntax)new SyntaxNormalizer().Visit(this);
    }

    public CompilationUnitSyntax WithRoot(SyntaxTree syntaxTree)
    {
        return new CompilationUnitSyntax(syntaxTree, Imports, Members);
    }
}

public static partial class SyntaxFactory
{
    public static CompilationUnitSyntax CompilationUnit()
        => new CompilationUnitSyntax();
}
