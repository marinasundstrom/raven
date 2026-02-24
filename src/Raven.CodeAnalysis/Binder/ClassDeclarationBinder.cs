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

            var hasPrimaryConstructor = typeSyntax is ClassDeclarationSyntax { ParameterList: not null }
                or RecordDeclarationSyntax { ParameterList: not null }
                or StructDeclarationSyntax { ParameterList: not null };
            var hasExplicitInstanceConstructor = typeSyntax.Members
                .OfType<ConstructorDeclarationSyntax>()
                .Any(ctor => !ctor.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword))
                || typeSyntax.Members
                    .OfType<InitDeclarationSyntax>()
                    .Any(initDecl => !initDecl.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword));

            var hasParameterlessCtor = named.Constructors
                .Any(ctor => !ctor.IsStatic && ctor.Parameters.Length == 0);

            if (!hasPrimaryConstructor &&
                !hasExplicitInstanceConstructor &&
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

            bool hasStaticCtor = named.GetMembers()
                .OfType<IMethodSymbol>()
                .Any(m => m.MethodKind == MethodKind.StaticConstructor);

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
            methodKind: MethodKind.StaticConstructor,
            declaredAccessibility: Accessibility.Private);
    }
}
