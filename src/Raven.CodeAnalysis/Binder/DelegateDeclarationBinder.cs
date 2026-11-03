namespace Raven.CodeAnalysis;

using Raven.CodeAnalysis.Syntax;

internal sealed class DelegateDeclarationBinder : TypeDeclarationBinder
{
    public DelegateDeclarationBinder(Binder parent, NamedTypeSymbol containingType, DelegateDeclarationSyntax syntax)
        : base(parent, containingType, syntax)
    {
    }
}
