namespace Raven.CodeAnalysis;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

internal sealed class ExtensionDeclarationBinder : TypeDeclarationBinder
{
    public ExtensionDeclarationBinder(Binder parent, INamedTypeSymbol containingType, ExtensionDeclarationSyntax syntax)
        : base(parent, containingType, syntax)
    {
    }
}
