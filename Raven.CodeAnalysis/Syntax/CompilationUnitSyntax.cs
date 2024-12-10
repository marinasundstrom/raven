

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

    public CompilationUnitSyntax WithImports(SyntaxList<ImportDirectiveSyntax> imports)
    {
        return new CompilationUnitSyntax(SyntaxTree, imports, Members);
    }

    public CompilationUnitSyntax WithMembers(SyntaxList<MemberDeclarationSyntax> members)
    {
        return new CompilationUnitSyntax(SyntaxTree, Imports, members);
    }

    public CompilationUnitSyntax NormalizeWhitespace()
    {
        return this;
    }

    public CompilationUnitSyntax WithRoot(SyntaxTree syntaxTree)
    {
        return new CompilationUnitSyntax(syntaxTree, Imports, Members);
    }

    public override void Accept(SyntaxVisitor visitor)
    {
        visitor.VisitCompilationUnit(this);
    }

    public override TNode Accept<TNode>(SyntaxVisitor<TNode> visitor)
    {
        return visitor.VisitCompilationUnit(this);
    }
}

public static partial class SyntaxFactory
{
    public static CompilationUnitSyntax CompilationUnit()
        => new CompilationUnitSyntax();
}
