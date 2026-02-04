namespace Raven.CodeAnalysis.Symbols;

internal static class NamespaceSymbolExtensions
{
    public static IEnumerable<ISymbol> GetAllMembersRecursive(this INamespaceSymbol ns)
    {
        if (ns is null)
            yield break;

        var members = ns.GetMembers();
        if (members.IsDefaultOrEmpty)
            yield break;

        foreach (var member in members)
        {
            if (member is null)
                continue;

            yield return member;
            if (member is INamespaceSymbol nestedNs)
            {
                foreach (var sub in GetAllMembersRecursive(nestedNs))
                    yield return sub;
            }
            else if (member is INamedTypeSymbol type)
            {
                var typeMembers = type.GetMembers();
                if (typeMembers.IsDefaultOrEmpty)
                    continue;

                foreach (var sub in typeMembers)
                    yield return sub;
            }
        }
    }

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

    public static string QualifyName(this INamespaceSymbol? parentNamespace, string name)
    {
        if (string.IsNullOrEmpty(name))
            return parentNamespace?.ToMetadataName() ?? string.Empty;

        if (parentNamespace is null || parentNamespace.IsGlobalNamespace)
            return name;

        var parentMetadataName = parentNamespace.ToMetadataName();
        if (string.IsNullOrEmpty(parentMetadataName))
            return name;

        return string.Concat(parentMetadataName, ".", name);
    }
}
