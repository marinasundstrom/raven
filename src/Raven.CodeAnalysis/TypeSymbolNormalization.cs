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

        IDiscriminatedUnionSymbol? discriminatedUnion = null;

        foreach (var member in members)
        {
            var caseSymbol = member.TryGetDiscriminatedUnionCase();
            if (caseSymbol is null)
                return false;

            var currentUnion = UnwrapAlias(caseSymbol.Union) as IDiscriminatedUnionSymbol;
            if (currentUnion is null)
                return false;

            if (discriminatedUnion is null)
            {
                discriminatedUnion = currentUnion;
            }
            else if (!SymbolEqualityComparer.Default.Equals(discriminatedUnion, currentUnion))
            {
                return false;
            }
        }

        result = discriminatedUnion as ITypeSymbol;
        return result is not null;
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
