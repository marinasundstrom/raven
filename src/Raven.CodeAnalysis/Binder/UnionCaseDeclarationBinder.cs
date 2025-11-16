using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed class UnionCaseDeclarationBinder : TypeDeclarationBinder
{
    public UnionCaseDeclarationBinder(Binder parent, INamedTypeSymbol containingType, UnionCaseDeclarationSyntax syntax)
        : base(parent, containingType, syntax)
    {
    }
}
