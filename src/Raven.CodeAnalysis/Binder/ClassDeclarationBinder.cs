using System.Linq;
using System.Collections.Immutable;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed class ClassDeclarationBinder : TypeDeclarationBinder
{
    public ClassDeclarationBinder(Binder parent, INamedTypeSymbol containingType, ClassDeclarationSyntax syntax)
        : base(parent, containingType, syntax)
    {
    }

    public void EnsureDefaultConstructor()
    {
        if (ContainingSymbol is INamedTypeSymbol named && !named.Constructors.Any(x => x.Parameters.Length == 0))
        {
            var classSyntax = (ClassDeclarationSyntax)Syntax;
            _ = new SourceMethodSymbol(
                ".ctor",
                Compilation.GetSpecialType(SpecialType.System_Void),
                ImmutableArray<SourceParameterSymbol>.Empty,
                ContainingSymbol,
                ContainingSymbol,
                CurrentNamespace!.AsSourceNamespace(),
                [classSyntax.GetLocation()],
                [classSyntax.GetReference()],
                isStatic: false,
                methodKind: MethodKind.Constructor);
        }
    }
}
