using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed class UnionDeclarationBinder : TypeDeclarationBinder
{
    public UnionDeclarationBinder(Binder parent, INamedTypeSymbol containingType, UnionDeclarationSyntax syntax)
        : base(parent, containingType, syntax)
    {
    }
}
