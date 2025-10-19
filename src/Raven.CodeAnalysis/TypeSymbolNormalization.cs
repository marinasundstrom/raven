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
            return new ByRefTypeSymbol(referenced);
        }

        if (type is IUnionTypeSymbol union)
            return NormalizeUnion(union.Types);

        if (type is LiteralTypeSymbol literal)
            return literal.UnderlyingType;

        return type;
    }

    public static ITypeSymbol NormalizeUnion(IEnumerable<ITypeSymbol> types)
    {
        var members = ImmutableArray.CreateBuilder<ITypeSymbol>();

        foreach (var member in types)
            AddNormalizedUnionMember(members, member);

        var filtered = RemoveRedundantUnionMembers(members.ToImmutable());

        if (TryCollapseToNullable(filtered, out var collapsed))
            return NormalizeForInference(collapsed);

        if (filtered.Length == 1)
            return NormalizeForInference(filtered[0]);

        return new UnionTypeSymbol(filtered, null, null, null, []);
    }

    private static void AddNormalizedUnionMember(ImmutableArray<ITypeSymbol>.Builder builder, ITypeSymbol member)
    {
        switch (member)
        {
            case IUnionTypeSymbol nested:
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

    private static bool TryCollapseToNullable(ImmutableArray<ITypeSymbol> members, out ITypeSymbol? result)
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

        result = new NullableTypeSymbol(other, null, null, null, []);
        return true;
    }
}
