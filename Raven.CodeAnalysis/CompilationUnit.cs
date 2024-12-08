using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class CompilationUnit : SyntaxNode
{
    public CompilationUnit(GreenNode greenNode, SyntaxNode parent) : base(greenNode, parent)
    {
    }

    public CompilationUnit(GreenNode greenNode, SyntaxTree syntaxTree) : base(greenNode, syntaxTree)
    {
    }

    public CompilationUnit()
    : this(new Syntax.InternalSyntax.CompilationUnit(null, null), (SyntaxNode)null)
    {
    }

    public CompilationUnit(SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members)
        : this(new Syntax.InternalSyntax.CompilationUnit(imports?.Green, members?.Green), (SyntaxNode)null)
    {
    }

    public partial SyntaxList<ImportDirectiveSyntax> Imports { get; }

    public partial SyntaxList<MemberDeclarationSyntax> Members { get; }

    public CompilationUnit WithImports(SyntaxList<ImportDirectiveSyntax> imports)
    {
        return new CompilationUnit(imports, Members);
    }

    public CompilationUnit WithMembers(SyntaxList<MemberDeclarationSyntax> members)
    {
        return new CompilationUnit(Imports, members);
    }
}