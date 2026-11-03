using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Symbols;

internal static class NamespaceSymbolExtensions
{
    public static IEnumerable<Symbol> GetAllMembersRecursive(this NamespaceSymbol ns)
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
            if (member is NamespaceSymbol nestedNs)
            {
                foreach (var sub in GetAllMembersRecursive(nestedNs))
                    yield return sub;
            }
            else if (member is NamedTypeSymbol type)
            {
                var typeMembers = type.GetMembers();
                if (typeMembers.IsDefaultOrEmpty)
                    continue;

                foreach (var sub in typeMembers)
                    yield return sub;
            }
        }
    }

    public static IEnumerable<NamedTypeSymbol> GetAllTypesRecursive(this NamespaceSymbol ns)
    {
        if (ns is null)
            yield break;

        var members = ns.GetMembers();
        if (members.IsDefaultOrEmpty)
            yield break;

        foreach (var member in members)
        {
            switch (member)
            {
                case null:
                    continue;
                case NamespaceSymbol nestedNamespace:
                    foreach (var nestedType in GetAllTypesRecursive(nestedNamespace))
                        yield return nestedType;
                    break;
                case NamedTypeSymbol type:
                    yield return type;

                    foreach (var nestedType in GetNestedTypesRecursive(type))
                        yield return nestedType;
                    break;
            }
        }
    }

    private static IEnumerable<NamedTypeSymbol> GetNestedTypesRecursive(NamedTypeSymbol type)
    {
        foreach (var nestedType in GetTypeMembers(type))
        {
            yield return nestedType;

            foreach (var descendant in GetNestedTypesRecursive(nestedType))
                yield return descendant;
        }
    }

    private static IEnumerable<NamedTypeSymbol> GetTypeMembers(NamedTypeSymbol type)
        => type is INestedTypeDiscovery discovery
            ? discovery.GetNestedTypesForDiscovery()
            : type.GetMembers().OfType<NamedTypeSymbol>();

    internal static SourceNamespaceSymbol? AsSourceNamespace(this NamespaceSymbol ns)
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

    public static string QualifyName(this NamespaceSymbol? parentNamespace, string name)
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
