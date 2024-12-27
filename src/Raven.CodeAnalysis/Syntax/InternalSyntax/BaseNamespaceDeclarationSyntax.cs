namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract class BaseNamespaceDeclarationSyntax : MemberDeclarationSyntax
{
    protected BaseNamespaceDeclarationSyntax(SyntaxKind kind, GreenNode[] slots)
        : base(kind, slots)
    {
    }
}