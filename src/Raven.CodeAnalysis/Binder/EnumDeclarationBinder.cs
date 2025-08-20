using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed class EnumDeclarationBinder : TypeDeclarationBinder
{
    public EnumDeclarationBinder(Binder parent, INamedTypeSymbol containingType, EnumDeclarationSyntax syntax)
        : base(parent, containingType, syntax)
    {
    }
}
