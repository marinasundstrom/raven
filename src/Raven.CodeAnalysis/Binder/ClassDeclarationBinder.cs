using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed class ClassDeclarationBinder : TypeDeclarationBinder
{
    public ClassDeclarationBinder(Binder parent, INamedTypeSymbol containingType, TypeDeclarationSyntax syntax)
        : base(parent, containingType, syntax)
    {
    }

    public void EnsureDefaultConstructor()
    {
        if (ContainingSymbol is INamedTypeSymbol named)
        {
            var typeSyntax = (TypeDeclarationSyntax)Syntax;

            if (named.IsStatic)
            {
                EnsureStaticConstructorIfNeeded(named, typeSyntax);
                return;
            }

            var hasPrimaryConstructor = typeSyntax is ClassDeclarationSyntax { ParameterList: not null } or RecordDeclarationSyntax { ParameterList: not null };
            var hasExplicitInstanceConstructor = typeSyntax.Members
                .OfType<ConstructorDeclarationSyntax>()
                .Any(ctor => !ctor.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword));
            var hasNamedConstructor = named.GetMembers()
                .OfType<IMethodSymbol>()
                .Any(method => method.MethodKind == MethodKind.NamedConstructor);

            var hasParameterlessCtor = named.Constructors
                .Any(ctor => !ctor.IsStatic && ctor.Parameters.Length == 0);
            var hasPublicParameterlessCtor = named.Constructors
                .Any(ctor => !ctor.IsStatic && ctor.Parameters.Length == 0 && ctor.DeclaredAccessibility == Accessibility.Public);

            if (!hasPrimaryConstructor &&
                !hasExplicitInstanceConstructor &&
                !hasNamedConstructor &&
                !hasParameterlessCtor)
            {
                _ = new SourceMethodSymbol(
                    ".ctor",
                    Compilation.GetSpecialType(SpecialType.System_Unit),
                    ImmutableArray<SourceParameterSymbol>.Empty,
                    ContainingSymbol,
                    ContainingSymbol,
                    CurrentNamespace!.AsSourceNamespace(),
                    [typeSyntax.GetLocation()],
                    [typeSyntax.GetReference()],
                    isStatic: false,
                    methodKind: MethodKind.Constructor,
                    declaredAccessibility: Accessibility.Public);
            }

            if (hasNamedConstructor &&
                !hasPublicParameterlessCtor &&
                !hasParameterlessCtor)
            {
                _ = new SourceMethodSymbol(
                    ".ctor",
                    Compilation.GetSpecialType(SpecialType.System_Unit),
                    ImmutableArray<SourceParameterSymbol>.Empty,
                    ContainingSymbol,
                    ContainingSymbol,
                    CurrentNamespace!.AsSourceNamespace(),
                    [typeSyntax.GetLocation()],
                    [typeSyntax.GetReference()],
                    isStatic: false,
                    methodKind: MethodKind.Constructor,
                    declaredAccessibility: Accessibility.Private);
            }

            bool hasStaticCtor = named.GetMembers()
                .OfType<IMethodSymbol>()
                .Any(m => m.IsStatic && m.MethodKind == MethodKind.Constructor);

            if (!hasStaticCtor)
            {
                EnsureStaticConstructorIfNeeded(named, typeSyntax);
            }
        }
    }

    private void EnsureStaticConstructorIfNeeded(INamedTypeSymbol named, TypeDeclarationSyntax typeSyntax)
    {
        bool needsStaticCtor = named.GetMembers()
            .OfType<SourceFieldSymbol>()
            .Any(f => f.IsStatic && f.Initializer is not null);

        if (!needsStaticCtor)
            return;

        _ = new SourceMethodSymbol(
            ".cctor",
            Compilation.GetSpecialType(SpecialType.System_Unit),
            ImmutableArray<SourceParameterSymbol>.Empty,
            ContainingSymbol,
            ContainingSymbol,
            CurrentNamespace!.AsSourceNamespace(),
            [typeSyntax.GetLocation()],
            [typeSyntax.GetReference()],
            isStatic: true,
            methodKind: MethodKind.Constructor,
            declaredAccessibility: Accessibility.Private);
    }
}
