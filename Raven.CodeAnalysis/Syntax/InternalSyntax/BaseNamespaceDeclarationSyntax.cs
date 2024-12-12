namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public abstract class BaseNamespaceDeclarationSyntax : MemberDeclarationSyntax
{
    protected BaseNamespaceDeclarationSyntax(SyntaxKind kind, GreenNode[] slots)
        : base(kind, slots)
    {
    }
}