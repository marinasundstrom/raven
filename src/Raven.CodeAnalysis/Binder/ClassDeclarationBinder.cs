using System.Collections.Immutable;
using System.Linq;

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
        if (ContainingSymbol is INamedTypeSymbol named)
        {
            var classSyntax = (ClassDeclarationSyntax)Syntax;

            if (!named.Constructors.Any(x => x.Parameters.Length == 0 && !x.IsStatic))
            {
                _ = new SourceMethodSymbol(
                    ".ctor",
                    Compilation.GetSpecialType(SpecialType.System_Unit),
                    ImmutableArray<SourceParameterSymbol>.Empty,
                    ContainingSymbol,
                    ContainingSymbol,
                    CurrentNamespace!.AsSourceNamespace(),
                    [classSyntax.GetLocation()],
                    [classSyntax.GetReference()],
                    isStatic: false,
                    methodKind: MethodKind.Constructor);
            }

            bool hasStaticCtor = named.GetMembers()
                .OfType<IMethodSymbol>()
                .Any(m => m.IsStatic && m.MethodKind == MethodKind.Constructor);

            if (!hasStaticCtor)
            {
                bool needsStaticCtor = named.GetMembers()
                    .OfType<SourceFieldSymbol>()
                    .Any(f => f.IsStatic && f.Initializer is not null);

                if (needsStaticCtor)
                {
                    _ = new SourceMethodSymbol(
                        ".cctor",
                        Compilation.GetSpecialType(SpecialType.System_Unit),
                        ImmutableArray<SourceParameterSymbol>.Empty,
                        ContainingSymbol,
                        ContainingSymbol,
                        CurrentNamespace!.AsSourceNamespace(),
                        [classSyntax.GetLocation()],
                        [classSyntax.GetReference()],
                        isStatic: true,
                        methodKind: MethodKind.Constructor);
                }
            }
        }
    }
}
