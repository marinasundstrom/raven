using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public sealed class SymbolEqualityComparer : IEqualityComparer<ISymbol>
{
    public static SymbolEqualityComparer Default { get; } = new(includeNullability: true);

    public static SymbolEqualityComparer IgnoringNullability { get; } = new(includeNullability: false);

    private readonly bool _includeNullability;

    public SymbolEqualityComparer(bool includeNullability = true)
    {
        _includeNullability = includeNullability;
    }

    public bool Equals(ISymbol? x, ISymbol? y)
    {
        if (ReferenceEquals(x, y))
            return true;

        if (x is null || y is null)
            return false;

        x = Normalize(x);
        y = Normalize(y);

        if (ReferenceEquals(x, y))
            return true;

        if (x.Kind != y.Kind)
            return false;

        if (x is ITypeSymbol typeX && y is ITypeSymbol typeY)
        {
            if (typeX.SpecialType != SpecialType.None &&
                typeX.SpecialType == typeY.SpecialType &&
                IsSimpleSpecialType(typeX.SpecialType))
            {
                return true;
            }
        }

        if (x is IParameterSymbol px && y is IParameterSymbol py)
        {
            if (px.RefKind != py.RefKind)
                return false;

            return Equals(px.Type, py.Type);
        }

        if (!string.Equals(x.Name, y.Name, StringComparison.Ordinal))
            return false;

        var namespaceX = x.ContainingNamespace?.ToDisplayString();
        var namespaceY = y.ContainingNamespace?.ToDisplayString();
        if (!string.Equals(namespaceX, namespaceY, StringComparison.Ordinal))
            return false;

        if (!Equals(x.ContainingSymbol, y.ContainingSymbol))
            return false;

        if (x is IMethodSymbol methodX && y is IMethodSymbol methodY)
        {
            if (!Equals(methodX.ReturnType, methodY.ReturnType))
                return false;

            if (methodX.Parameters.Length != methodY.Parameters.Length)
                return false;

            for (int i = 0; i < methodX.Parameters.Length; i++)
            {
                if (!Equals(methodX.Parameters[i], methodY.Parameters[i]))
                    return false;
            }
        }

        return true;
    }

    private static bool IsSimpleSpecialType(SpecialType specialType)
    {
        return specialType is SpecialType.System_Boolean
            or SpecialType.System_Char
            or SpecialType.System_Double
            or SpecialType.System_Int32
            or SpecialType.System_Int64
            or SpecialType.System_Type
            or SpecialType.System_Object
            or SpecialType.System_Single
            or SpecialType.System_String
            or SpecialType.System_Unit;
    }

    public int GetHashCode(ISymbol obj)
    {
        if (obj is null)
            return 0;

        obj = Normalize(obj);

        var hash = new HashCode();
        hash.Add(obj.Kind);
        hash.Add(obj.Name, StringComparer.Ordinal);
        hash.Add(obj.MetadataName, StringComparer.Ordinal);

        var containingNamespace = obj.ContainingNamespace?.ToDisplayString();
        if (containingNamespace is not null)
            hash.Add(containingNamespace, StringComparer.Ordinal);

        if (obj is IParameterSymbol parameterSymbol)
        {
            hash.Add(parameterSymbol.RefKind);
            hash.Add(GetHashCode(parameterSymbol.Type));
            return hash.ToHashCode();
        }

        if (obj is IMethodSymbol method)
        {
            hash.Add(GetHashCode(method.ReturnType));
            hash.Add(method.Parameters.Length);

            foreach (var param in method.Parameters)
            {
                hash.Add(param.RefKind);
                hash.Add(GetHashCode(param.Type));
            }
        }

        if (obj.ContainingSymbol is { } containingSymbol && obj is not ITypeParameterSymbol)
        {
            hash.Add(GetHashCode(containingSymbol));
        }

        return hash.ToHashCode();
    }

    private ISymbol Normalize(ISymbol symbol)
    {
        var current = symbol;

        while (true)
        {
            if (current.IsAlias)
            {
                current = current.UnderlyingSymbol;
                continue;
            }

            if (!_includeNullability && current is NullableTypeSymbol nullable)
            {
                current = nullable.UnderlyingType;
                continue;
            }

            return current;
        }
    }
}
