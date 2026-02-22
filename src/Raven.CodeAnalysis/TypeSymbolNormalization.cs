using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class TypeSymbolNormalization
{
    public static ITypeSymbol NormalizeForInference(ITypeSymbol type)
    {
        if (type is IAddressTypeSymbol address)
        {
            var referenced = NormalizeForInference(address.ReferencedType);
            return new RefTypeSymbol(referenced);
        }

        if (type is ITypeUnionSymbol union)
            return NormalizeUnion(union.Types);

        if (type is LiteralTypeSymbol literal)
            return literal.UnderlyingType;

        return type;
    }

    public static ITypeSymbol NormalizeUnion(
        IEnumerable<ITypeSymbol> types,
        DiagnosticBag? diagnostics = null,
        Location? location = null,
        ITypeSymbol? errorTypeSymbol = null)
    {
        var members = ImmutableArray.CreateBuilder<ITypeSymbol>();

        foreach (var member in types)
            AddNormalizedUnionMember(members, member);

        var filtered = RemoveRedundantUnionMembers(members.ToImmutable());

        if (TryCollapseToDiscriminatedUnion(filtered, out var discriminatedUnion))
            return NormalizeForInference(discriminatedUnion);

        if (TryCollapseToNullable(filtered, out var collapsed, diagnostics, location, errorTypeSymbol))
            return NormalizeForInference(collapsed);

        if (filtered.Length == 1)
            return NormalizeForInference(filtered[0]);

        return new TypeUnionSymbol(filtered, null, null, null, []);
    }

    private static void AddNormalizedUnionMember(ImmutableArray<ITypeSymbol>.Builder builder, ITypeSymbol member)
    {
        switch (member)
        {
            case ITypeUnionSymbol nested:
                foreach (var nestedMember in nested.Types)
                    AddNormalizedUnionMember(builder, nestedMember);
                break;
            case LiteralTypeSymbol literal:
                builder.Add(literal);
                break;
            default:
                builder.Add(NormalizeForInference(member));
                break;
        }
    }

    private static ImmutableArray<ITypeSymbol> RemoveRedundantUnionMembers(ImmutableArray<ITypeSymbol> members)
    {
        var filtered = ImmutableArray.CreateBuilder<ITypeSymbol>();
        var hasNonLiteral = members.Any(member => member is not LiteralTypeSymbol);

        foreach (var member in members)
        {
            var candidate = member;

            if (member is LiteralTypeSymbol literal)
            {
                if (hasNonLiteral)
                    candidate = NormalizeForInference(literal.UnderlyingType);

                if (members.Any(other => !ReferenceEquals(other, member) &&
                                         SymbolEqualityComparer.Default.Equals(other, literal.UnderlyingType)))
                {
                    continue;
                }
            }

            if (candidate.TypeKind == TypeKind.Null &&
                members.Any(other => !ReferenceEquals(other, member) && other is NullableTypeSymbol))
            {
                continue;
            }

            if (candidate is not NullableTypeSymbol &&
                members.Any(other => !ReferenceEquals(other, member) &&
                                     other is NullableTypeSymbol nullable &&
                                     SymbolEqualityComparer.Default.Equals(nullable.UnderlyingType, candidate)))
            {
                continue;
            }

            if (!filtered.Any(existing => SymbolEqualityComparer.Default.Equals(existing, candidate)))
                filtered.Add(candidate);
        }

        return filtered.ToImmutable();
    }

    private static bool TryCollapseToDiscriminatedUnion(
        ImmutableArray<ITypeSymbol> members,
        out ITypeSymbol? result)
    {
        result = null;

        if (members.IsDefaultOrEmpty)
            return false;

        INamedTypeSymbol? discriminatedUnion = null;

        foreach (var member in members)
        {
            if (!TryGetContainingDiscriminatedUnion(member, out var currentUnion))
                return false;

            if (discriminatedUnion is null)
            {
                discriminatedUnion = currentUnion;
            }
            else if (!SymbolEqualityComparer.Default.Equals(discriminatedUnion, currentUnion))
            {
                var leftDefinition = discriminatedUnion.OriginalDefinition as INamedTypeSymbol ?? discriminatedUnion;
                var rightDefinition = currentUnion.OriginalDefinition as INamedTypeSymbol ?? currentUnion;
                if (!SymbolEqualityComparer.Default.Equals(leftDefinition, rightDefinition))
                    return false;

                if (!TryMergeConstructedUnion(discriminatedUnion, currentUnion, out var mergedUnion))
                    return false;

                discriminatedUnion = mergedUnion;
            }
        }

        result = discriminatedUnion as ITypeSymbol;
        return result is not null;
    }

    private static bool TryMergeConstructedUnion(INamedTypeSymbol left, INamedTypeSymbol right, out INamedTypeSymbol merged)
    {
        merged = left;

        if (SymbolEqualityComparer.Default.Equals(left, right))
            return true;

        var leftDefinition = left.OriginalDefinition as INamedTypeSymbol ?? left;
        var rightDefinition = right.OriginalDefinition as INamedTypeSymbol ?? right;
        if (!SymbolEqualityComparer.Default.Equals(leftDefinition, rightDefinition))
            return false;

        if (left.TypeArguments.IsDefaultOrEmpty && right.TypeArguments.IsDefaultOrEmpty)
        {
            merged = left;
            return true;
        }

        if (left.TypeArguments.Length != right.TypeArguments.Length)
            return false;

        var mergedArguments = new ITypeSymbol[left.TypeArguments.Length];
        var changed = false;

        for (var i = 0; i < mergedArguments.Length; i++)
        {
            var leftArgument = left.TypeArguments[i];
            var rightArgument = right.TypeArguments[i];

            if (SymbolEqualityComparer.Default.Equals(leftArgument, rightArgument))
            {
                mergedArguments[i] = leftArgument;
                continue;
            }

            if (leftArgument is ITypeParameterSymbol && rightArgument is not ITypeParameterSymbol)
            {
                mergedArguments[i] = rightArgument;
                changed = true;
                continue;
            }

            if (rightArgument is ITypeParameterSymbol && leftArgument is not ITypeParameterSymbol)
            {
                mergedArguments[i] = leftArgument;
                continue;
            }

            return false;
        }

        if (!changed)
        {
            merged = left;
            return true;
        }

        merged = (INamedTypeSymbol)leftDefinition.Construct(mergedArguments);
        return true;
    }

    private static bool TryGetContainingDiscriminatedUnion(ITypeSymbol member, out INamedTypeSymbol? union)
    {
        union = null;

        var caseSymbol = member.TryGetDiscriminatedUnionCase();
        if (caseSymbol is not null)
        {
            if (member is INamedTypeSymbol caseNamed &&
                TryProjectUnionFromCaseArguments(caseNamed, caseSymbol, out var projectedUnion))
            {
                union = projectedUnion;
                return true;
            }

            union = UnwrapAlias(caseSymbol.Union) as INamedTypeSymbol;
            if (union is not null)
                return true;
        }

        if (member is not INamedTypeSymbol namedMember)
            return false;

        if (namedMember.ContainingType is not INamedTypeSymbol containingType)
            return false;

        var containingUnion = UnwrapAlias(containingType).TryGetDiscriminatedUnion() as INamedTypeSymbol;
        if (containingUnion is null)
            return false;

        if (containingUnion is not IDiscriminatedUnionSymbol discriminatedUnionSymbol)
            return false;

        if (!discriminatedUnionSymbol.Cases.Any(@case => string.Equals(@case.Name, namedMember.Name, System.StringComparison.Ordinal)))
            return false;

        union = containingType;
        return true;
    }

    private static bool TryProjectUnionFromCaseArguments(
        INamedTypeSymbol caseType,
        IDiscriminatedUnionCaseSymbol caseSymbol,
        out INamedTypeSymbol? projectedUnion)
    {
        projectedUnion = null;

        if (caseType.TypeArguments.IsDefaultOrEmpty)
            return false;

        var caseDefinition = caseSymbol.OriginalDefinition as IDiscriminatedUnionCaseSymbol ?? caseSymbol;
        if (caseDefinition is not INamedTypeSymbol caseDefinitionNamed ||
            caseDefinitionNamed.TypeParameters.IsDefaultOrEmpty ||
            caseDefinitionNamed.TypeParameters.Length != caseType.TypeArguments.Length)
        {
            return false;
        }

        var unionDefinition = caseDefinition.Union.OriginalDefinition as INamedTypeSymbol ?? caseDefinition.Union as INamedTypeSymbol;
        if (unionDefinition is null || unionDefinition.TypeParameters.IsDefaultOrEmpty)
            return false;

        var unionTypeArguments = unionDefinition.TypeParameters
            .Select(typeParameter => (ITypeSymbol)typeParameter)
            .ToArray();

        var changed = false;

        for (var i = 0; i < caseDefinitionNamed.TypeParameters.Length; i++)
        {
            var caseTypeParameter = caseDefinitionNamed.TypeParameters[i];
            var unionTypeParameter = default(ITypeParameterSymbol);

            if (caseDefinition is SourceDiscriminatedUnionCaseTypeSymbol sourceCaseDefinition &&
                sourceCaseDefinition.TryGetProjectedUnionTypeParameter(caseTypeParameter, out var mapped))
            {
                unionTypeParameter = mapped;
            }
            else
            {
                unionTypeParameter = unionDefinition.TypeParameters
                    .FirstOrDefault(tp => string.Equals(tp.Name, caseTypeParameter.Name, System.StringComparison.Ordinal));
            }

            if (unionTypeParameter is null)
                continue;

            var unionIndex = -1;
            for (var unionParameterIndex = 0; unionParameterIndex < unionDefinition.TypeParameters.Length; unionParameterIndex++)
            {
                if (SymbolEqualityComparer.Default.Equals(unionDefinition.TypeParameters[unionParameterIndex], unionTypeParameter))
                {
                    unionIndex = unionParameterIndex;
                    break;
                }
            }

            if (unionIndex < 0 || unionIndex >= unionTypeArguments.Length)
                continue;

            unionTypeArguments[unionIndex] = caseType.TypeArguments[i];
            changed = true;
        }

        if (!changed)
            return false;

        projectedUnion = (INamedTypeSymbol)unionDefinition.Construct(unionTypeArguments);
        return true;
    }

    private static bool TryCollapseToNullable(
        ImmutableArray<ITypeSymbol> members,
        out ITypeSymbol? result,
        DiagnosticBag? diagnostics,
        Location? location,
        ITypeSymbol? errorTypeSymbol)
    {
        result = null;

        if (members.Length != 2)
            return false;

        ITypeSymbol? nullMember = null;
        ITypeSymbol? other = null;

        foreach (var member in members)
        {
            if (member.TypeKind == TypeKind.Null)
            {
                if (nullMember is not null)
                    return false;

                nullMember = member;
                continue;
            }

            if (other is not null)
                return false;

            other = member;
        }

        if (nullMember is null || other is null)
            return false;

        if (other is NullableTypeSymbol nullable)
        {
            result = nullable;
            return true;
        }

        if (other.TypeKind == TypeKind.Error)
            return false;

        if (other is ITypeParameterSymbol typeParameter &&
            (typeParameter.ConstraintKind & TypeParameterConstraintKind.NotNull) != 0)
        {
            if (diagnostics is not null && location is not null)
                diagnostics.ReportNotNullTypeParameterCannotBeNullable(typeParameter.Name, location);

            if (errorTypeSymbol is not null)
            {
                result = errorTypeSymbol;
                return true;
            }

            return false;
        }

        result = other.MakeNullable();
        return true;
    }

    private static ITypeSymbol UnwrapAlias(ITypeSymbol type)
    {
        while (type.IsAlias && type.UnderlyingSymbol is ITypeSymbol aliasTarget)
            type = aliasTarget;

        return type;
    }
}
