using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class SealedHierarchyFacts
{
    public static bool TryGetCaseDefinition(
        TypeSymbol? lookupType,
        string caseName,
        [NotNullWhen(true)] out NamedTypeSymbol? sealedRoot,
        out NamedTypeSymbol? caseDefinition)
    {
        sealedRoot = null;
        caseDefinition = null;

        if (lookupType is null || !TypeCoverageHelper.TryGetSealedHierarchy(lookupType, out sealedRoot))
            return false;

        var cases = sealedRoot.PermittedDirectSubtypes;
        if (!cases.IsDefaultOrEmpty)
            caseDefinition = cases.FirstOrDefault(candidate => string.Equals(candidate.Name, caseName, System.StringComparison.Ordinal));
        return true;
    }

    public static NamedTypeSymbol ProjectCaseTypeToHierarchyArguments(
        NamedTypeSymbol caseType,
        NamedTypeSymbol hierarchyType)
    {
        var caseDefinition = (caseType.OriginalDefinition as NamedTypeSymbol) ?? caseType;
        var hierarchyDefinition = (hierarchyType.OriginalDefinition as NamedTypeSymbol) ?? hierarchyType;

        if (!caseDefinition.IsGenericType || caseDefinition.TypeParameters.IsDefaultOrEmpty)
            return caseDefinition;

        if (hierarchyType.TypeArguments.IsDefaultOrEmpty || hierarchyDefinition.TypeParameters.IsDefaultOrEmpty)
            return caseDefinition;

        var implementedHierarchy = FindImplementedHierarchy(caseDefinition, hierarchyDefinition);
        if (implementedHierarchy is null)
            return caseDefinition;

        var substitutions = new Dictionary<TypeParameterSymbol, TypeSymbol>(SymbolEqualityComparer.Default);
        var templateArguments = implementedHierarchy.TypeArguments;
        var actualArguments = hierarchyType.TypeArguments;
        var count = System.Math.Min(templateArguments.Length, actualArguments.Length);
        for (var i = 0; i < count; i++)
        {
            if (!TryCollectTypeParameterSubstitutions(templateArguments[i], actualArguments[i], substitutions))
                return caseDefinition;
        }

        if (substitutions.Count == 0)
            return caseDefinition;

        var projectedArguments = new TypeSymbol[caseDefinition.TypeParameters.Length];
        var changed = false;
        for (var i = 0; i < caseDefinition.TypeParameters.Length; i++)
        {
            var parameter = caseDefinition.TypeParameters[i];
            if (substitutions.TryGetValue(parameter, out var mapped))
            {
                projectedArguments[i] = mapped;
                if (!SymbolEqualityComparer.Default.Equals(mapped, parameter))
                    changed = true;
            }
            else
            {
                projectedArguments[i] = parameter;
            }
        }

        return changed
            ? (NamedTypeSymbol)caseDefinition.Construct(projectedArguments)
            : caseDefinition;
    }

    private static NamedTypeSymbol? FindImplementedHierarchy(
        NamedTypeSymbol caseDefinition,
        NamedTypeSymbol hierarchyDefinition)
    {
        if (caseDefinition.BaseType is NamedTypeSymbol baseType &&
            SymbolEqualityComparer.Default.Equals(
                (baseType.OriginalDefinition as NamedTypeSymbol) ?? baseType,
                hierarchyDefinition))
        {
            return baseType;
        }

        return caseDefinition.Interfaces.FirstOrDefault(interfaceType =>
            SymbolEqualityComparer.Default.Equals(
                (interfaceType.OriginalDefinition as NamedTypeSymbol) ?? interfaceType,
                hierarchyDefinition));
    }

    private static bool TryCollectTypeParameterSubstitutions(
        TypeSymbol template,
        TypeSymbol actual,
        Dictionary<TypeParameterSymbol, TypeSymbol> substitutions)
    {
        template = template.UnwrapLiteralType() ?? template;
        actual = actual.UnwrapLiteralType() ?? actual;

        if (template is TypeParameterSymbol templateParameter)
        {
            if (substitutions.TryGetValue(templateParameter, out var existing))
                return SymbolEqualityComparer.Default.Equals(existing, actual);

            substitutions[templateParameter] = actual;
            return true;
        }

        if (template is NullableTypeSymbol templateNullable &&
            actual is NullableTypeSymbol actualNullable)
        {
            return TryCollectTypeParameterSubstitutions(
                templateNullable.UnderlyingType,
                actualNullable.UnderlyingType,
                substitutions);
        }

        if (template is ArrayTypeSymbol templateArray &&
            actual is ArrayTypeSymbol actualArray &&
            templateArray.Rank == actualArray.Rank)
        {
            return TryCollectTypeParameterSubstitutions(
                templateArray.ElementType,
                actualArray.ElementType,
                substitutions);
        }

        if (template is RefTypeSymbol templateRef &&
            actual is RefTypeSymbol actualRef)
        {
            return TryCollectTypeParameterSubstitutions(
                templateRef.ElementType,
                actualRef.ElementType,
                substitutions);
        }

        if (template is AddressTypeSymbol templateAddress &&
            actual is AddressTypeSymbol actualAddress)
        {
            return TryCollectTypeParameterSubstitutions(
                templateAddress.ReferencedType,
                actualAddress.ReferencedType,
                substitutions);
        }

        if (template is NamedTypeSymbol templateNamed &&
            actual is NamedTypeSymbol actualNamed)
        {
            var templateDefinition = (templateNamed.OriginalDefinition as NamedTypeSymbol) ?? templateNamed;
            var actualDefinition = (actualNamed.OriginalDefinition as NamedTypeSymbol) ?? actualNamed;
            if (!SymbolEqualityComparer.Default.Equals(templateDefinition, actualDefinition))
                return false;

            if (templateNamed.TypeArguments.IsDefaultOrEmpty || actualNamed.TypeArguments.IsDefaultOrEmpty)
                return true;

            if (templateNamed.TypeArguments.Length != actualNamed.TypeArguments.Length)
                return false;

            for (var i = 0; i < templateNamed.TypeArguments.Length; i++)
            {
                if (!TryCollectTypeParameterSubstitutions(
                        templateNamed.TypeArguments[i],
                        actualNamed.TypeArguments[i],
                        substitutions))
                {
                    return false;
                }
            }

            return true;
        }

        return SymbolEqualityComparer.Default.Equals(template, actual);
    }
}
