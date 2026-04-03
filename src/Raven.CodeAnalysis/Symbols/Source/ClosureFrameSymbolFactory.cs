using System;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

namespace Raven.CodeAnalysis.Symbols;

internal static class ClosureFrameSymbolFactory
{
    internal static SourceNamedTypeSymbol Create(IMethodSymbol owner)
    {
        if (owner is null)
            throw new ArgumentNullException(nameof(owner));

        var baseType = owner.ContainingAssembly.GetTypeByMetadataName("System.Object") as INamedTypeSymbol
            ?? throw new InvalidOperationException("Unable to resolve System.Object for closure frame.");

        var name = CreateClosureName(owner);
        var closureSymbol = new SourceNamedTypeSymbol(
            name,
            baseType,
            TypeKind.Class,
            owner.ContainingSymbol,
            containingType: owner.ContainingType,
            containingNamespace: owner.ContainingNamespace,
            locations: owner.Locations.ToArray(),
            declaringSyntaxReferences: owner.DeclaringSyntaxReferences.ToArray(),
            isSealed: true,
            declaredAccessibility: Accessibility.Private,
            metadataName: name);

        if (!owner.TypeParameters.IsDefaultOrEmpty)
            closureSymbol.SetTypeParameters(CloneClosureTypeParameters(owner, closureSymbol));

        return closureSymbol;
    }

    private static string CreateClosureName(IMethodSymbol owner)
    {
        var sourceName = owner.MetadataName;
        if (string.IsNullOrWhiteSpace(sourceName))
            sourceName = string.IsNullOrWhiteSpace(owner.Name) ? owner.MethodKind.ToString() : owner.Name;

        var builder = new StringBuilder("<>c__DisplayClass_");
        foreach (var ch in sourceName)
            builder.Append(char.IsLetterOrDigit(ch) ? ch : '_');

        return builder.ToString();
    }

    private static ImmutableArray<ITypeParameterSymbol> CloneClosureTypeParameters(
        IMethodSymbol owner,
        SourceNamedTypeSymbol closureSymbol)
    {
        var builder = ImmutableArray.CreateBuilder<ITypeParameterSymbol>(owner.TypeParameters.Length);
        foreach (var typeParameter in owner.TypeParameters)
        {
            var clone = new SourceTypeParameterSymbol(
                typeParameter.Name,
                closureSymbol,
                closureSymbol,
                closureSymbol.ContainingNamespace,
                typeParameter.Locations.ToArray(),
                typeParameter.DeclaringSyntaxReferences.ToArray(),
                typeParameter.Ordinal,
                typeParameter.ConstraintKind,
                typeParameter is SourceTypeParameterSymbol sourceTypeParameter
                    ? sourceTypeParameter.ConstraintTypeReferences
                    : ImmutableArray<SyntaxReference>.Empty,
                typeParameter.Variance);

            if (!typeParameter.ConstraintTypes.IsDefaultOrEmpty)
                clone.SetConstraintTypes(typeParameter.ConstraintTypes);

            builder.Add(clone);
        }

        return builder.ToImmutable();
    }
}
