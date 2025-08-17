namespace Raven.CodeAnalysis;

public class SymbolEqualityComparer : IEqualityComparer<ISymbol>
{
    public static SymbolEqualityComparer Default { get; } = new SymbolEqualityComparer();

    public bool Equals(ISymbol? x, ISymbol? y)
    {
        if (x == null || y == null) return x == y;

        // Compare kinds
        if (x.Kind != y.Kind) return false;

        // Compare names
        if (x.Name != y.Name) return false;

        // Compare containing namespaces
        if (!Equals(x.ContainingNamespace?.ToDisplayString(), y.ContainingNamespace?.ToDisplayString()))
            return false;

        // Compare parameters (for methods)
        if (x is IMethodSymbol methodX && y is IMethodSymbol methodY)
        {
            if (!methodX.Parameters.SequenceEqual((IEnumerable<ISymbol>)methodY.Parameters, Default))
                return false;

            for (int i = 0; i < methodX.Parameters.Length; i++)
            {
                var paramX = methodX.Parameters[i];
                var paramY = methodY.Parameters[i];
                if (!paramX.Type.Equals(paramY.Type, Default))
                    return false;
            }
        }

        // Other checks as needed
        return true;
    }

    public int GetHashCode(ISymbol obj)
    {
        if (obj == null) return 0;

        // Combine key fields into hash code
        int hash = obj.Kind.GetHashCode();
        hash = (hash * 31) + (obj.Name?.GetHashCode() ?? 0);
        hash = (hash * 31) + (obj.ContainingNamespace?.ToDisplayString().GetHashCode() ?? 0);
        return hash;
    }
}