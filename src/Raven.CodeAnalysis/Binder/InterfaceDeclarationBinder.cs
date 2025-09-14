using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed class InterfaceDeclarationBinder : TypeDeclarationBinder
{
    public InterfaceDeclarationBinder(Binder parent, INamedTypeSymbol containingType, InterfaceDeclarationSyntax syntax)
        : base(parent, containingType, syntax)
    {
    }
}
