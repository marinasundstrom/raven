namespace Raven.CodeAnalysis;

using Raven.CodeAnalysis.Syntax;

internal sealed class DelegateDeclarationBinder : TypeDeclarationBinder
{
    public DelegateDeclarationBinder(Binder parent, INamedTypeSymbol containingType, DelegateDeclarationSyntax syntax)
        : base(parent, containingType, syntax)
    {
    }
}
