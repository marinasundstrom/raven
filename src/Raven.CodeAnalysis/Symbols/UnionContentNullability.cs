using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis.Symbols;

internal static class UnionContentNullability
{
    public static bool IsNullableContentType(TypeSymbol type)
        => GetNonNullContentType(type, out var mayBeNull).TypeKind == TypeKind.Null || mayBeNull;

    public static TypeSymbol GetNonNullContentType(TypeSymbol type, out bool mayBeNull)
    {
        type = UnwrapAlias(type);

        if (type.TypeKind == TypeKind.Null)
        {
            mayBeNull = true;
            return type;
        }

        if (type is INullableTypeIdentity nullable)
        {
            mayBeNull = true;
            return UnwrapAlias(nullable.UnderlyingType);
        }

        if (type is NamedTypeSymbol { SpecialType: SpecialType.System_Nullable_T } nullableValue &&
            nullableValue.TypeArguments.Length == 1)
        {
            mayBeNull = true;
            return UnwrapAlias(nullableValue.TypeArguments[0]);
        }

        mayBeNull = type.IsNullable;
        return type;
    }

    public static ImmutableArray<TypeSymbol> GetPatternDomainTypes(UnionSymbol union, TypeSymbol nullTypeSymbol)
    {
        var builder = ImmutableArray.CreateBuilder<TypeSymbol>();
        var includeNull = false;

        foreach (var member in union.MemberTypes)
        {
            var nonNullMember = GetNonNullContentType(member, out var memberMayBeNull);
            includeNull |= memberMayBeNull;

            if (nonNullMember.TypeKind == TypeKind.Null)
                continue;

            AddDistinct(builder, nonNullMember);
        }

        if (includeNull || union.ContentMayBeNull)
            AddDistinct(builder, nullTypeSymbol);

        return builder.ToImmutable();
    }

    private static void AddDistinct(ImmutableArray<TypeSymbol>.Builder builder, TypeSymbol type)
    {
        if (builder.Any(existing => SymbolEqualityComparer.Default.Equals(existing, type)))
            return;

        builder.Add(type);
    }

    private static TypeSymbol UnwrapAlias(TypeSymbol type)
        => type is AliasSymbol { UnderlyingSymbol: TypeSymbol underlying } ? underlying : type;
}
