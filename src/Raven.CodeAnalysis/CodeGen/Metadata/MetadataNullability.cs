using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen.Metadata;

internal static class MetadataNullability
{
    public static bool RequiresNullableAttribute(ITypeSymbol? type)
    {
        if (type is null)
            return false;

        if (type is NullableTypeSymbol nullable && !nullable.UnderlyingType.IsValueType)
            return true;

        if (type is IUnionTypeSymbol union)
        {
            var flattened = Flatten(union.Types).ToArray();
            var hasNull = false;
            var nonNull = new List<ITypeSymbol>();

            foreach (var candidate in flattened)
            {
                if (candidate.TypeKind == TypeKind.Null)
                {
                    hasNull = true;
                }
                else
                {
                    nonNull.Add(candidate);
                }
            }

            if (hasNull)
            {
                if (!(nonNull.Count == 1 && nonNull[0].IsValueType))
                    return true;
            }
        }

        return false;
    }

    private static IEnumerable<ITypeSymbol> Flatten(IEnumerable<ITypeSymbol> types)
    {
        foreach (var type in types)
        {
            if (type is IUnionTypeSymbol union)
            {
                foreach (var nested in Flatten(union.Types))
                    yield return nested;
            }
            else
            {
                yield return type;
            }
        }
    }
}
