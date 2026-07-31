namespace Raven.CodeAnalysis.Symbols;

internal static class ModuleNamespaceResolver
{
    public static INamespaceSymbol? Resolve(IModuleSymbol module, INamespaceSymbol namespaceSymbol)
    {
        if (namespaceSymbol.IsGlobalNamespace)
            return module.GlobalNamespace;

        var metadataName = namespaceSymbol.ToMetadataName();
        if (string.IsNullOrEmpty(metadataName))
            return null;

        var current = module.GlobalNamespace;
        foreach (var segment in metadataName.Split('.'))
        {
            current = current.LookupNamespace(segment);
            if (current is null)
                return null;
        }

        return ReferenceEquals(current.ContainingModule, module)
            ? current
            : null;
    }
}
