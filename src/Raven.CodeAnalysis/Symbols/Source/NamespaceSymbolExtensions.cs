namespace Raven.CodeAnalysis.Symbols;

public static class NamespaceSymbolExtensions
{
    public static IEnumerable<ISymbol> GetAllMembersRecursive(this INamespaceSymbol ns)
    {
        foreach (var member in ns.GetMembers())
        {
            yield return member;
            if (member is INamespaceSymbol nestedNs)
            {
                foreach (var sub in GetAllMembersRecursive(nestedNs))
                    yield return sub;
            }
            else if (member is INamedTypeSymbol type)
            {
                foreach (var sub in type.GetMembers())
                    yield return sub;
            }
        }
    }

    /*

    public static IEnumerable<ISymbol> GetAllMembersRecursive(this INamespaceSymbol ns)
    {
        foreach (var member in ns.GetMembers())
        {
            yield return member;

            if (member is INamespaceSymbol childNs)
            {
                foreach (var nested in childNs.GetAllMembersRecursive())
                    yield return nested;
            }
        }
    }
    */

    internal static SourceNamespaceSymbol? AsSourceNamespace(this INamespaceSymbol ns)
    {
        if (ns is SourceNamespaceSymbol sourceNamespace)
            return sourceNamespace;

        if (ns is MergedNamespaceSymbol mergedNamespace)
        {
            return mergedNamespace
                .GetMergedNamespaces()
                .OfType<SourceNamespaceSymbol>()
                .FirstOrDefault();
        }

        return null;
    }
}