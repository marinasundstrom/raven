using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed class ClassDeclarationBinder : TypeDeclarationBinder
{
    public ClassDeclarationBinder(Binder parent, NamedTypeSymbol containingType, TypeDeclarationSyntax syntax)
        : base(parent, containingType, syntax)
    {
    }

    public void EnsureDefaultConstructor()
    {
        if (ContainingSymbol is NamedTypeSymbol named)
        {
            var typeSyntax = (TypeDeclarationSyntax)Syntax;

            if (named.IsStatic)
            {
                EnsureStaticConstructorIfNeeded(named, typeSyntax);
                return;
            }

            var declarations = named.DeclaringSyntaxReferences
                .Select(static reference => reference.GetSyntax())
                .OfType<TypeDeclarationSyntax>()
                .ToArray();
            var hasPrimaryConstructor = declarations.Any(static declaration => declaration is
                ClassDeclarationSyntax { ParameterList: not null }
                or RecordDeclarationSyntax { ParameterList: not null }
                or StructDeclarationSyntax { ParameterList: not null });
            var hasExplicitInstanceConstructor = declarations.SelectMany(static declaration => declaration.Members)
                .OfType<ConstructorDeclarationSyntax>()
                .Any(static ctor => !ctor.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.StaticKeyword));

            var hasParameterlessCtor = named.Constructors
                .Any(ctor => !ctor.IsStatic && ctor.Parameters.Length == 0);

            if (named.TypeKind == TypeKind.Struct && !hasParameterlessCtor &&
                (hasPrimaryConstructor || hasExplicitInstanceConstructor))
            {
                _ = new SynthesizedDefaultValueTypeConstructorSymbol(named, Compilation);
            }

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
                .OfType<MethodSymbol>()
                .Any(m => m.MethodKind == MethodKind.StaticConstructor);

            if (!hasStaticCtor)
            {
                EnsureStaticConstructorIfNeeded(named, typeSyntax);
            }
        }
    }

    public void ValidateImplicitBaseConstructors()
    {
        var type = ContainingSymbol;
        if (type.IsStatic || type.IsValueType || type.BaseType is not { IsStatic: false, TypeKind: not TypeKind.Error } baseType)
            return;

        if (baseType.Constructors.Any(static constructor => !constructor.IsStatic && constructor.Parameters.Length == 0))
            return;

        foreach (var constructor in type.Constructors.OfType<SourceMethodSymbol>())
        {
            if (constructor.IsStatic || constructor.HasConstructorInitializerSyntax || constructor.ConstructorInitializer is not null)
                continue;

            // Synthesized record copy constructors call the base copy constructor.
            if (type is SourceNamedTypeSymbol { IsRecord: true } && constructor.Parameters.Length == 1 &&
                SymbolEqualityComparer.Default.Equals(constructor.Parameters[0].Type, type))
            {
                continue;
            }

            var declaration = constructor.GetDeclaringTypeSyntax();
            if (declaration?.SyntaxTree != Syntax.SyntaxTree || declaration.Span != Syntax.Span)
                continue;

            _diagnostics.ReportNoOverloadForMethod(
                "constructor for type", baseType.Name, 0, constructor.Locations[0]);
        }
    }

    private void EnsureStaticConstructorIfNeeded(NamedTypeSymbol named, TypeDeclarationSyntax typeSyntax)
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
