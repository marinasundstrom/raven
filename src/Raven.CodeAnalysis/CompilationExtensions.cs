using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public static class CompilationExtensions
{
    public static INamespaceSymbol? GetNamespaceSymbol(this Compilation compilation, string? ns)
    {
        if (ns is null)
            return compilation.GlobalNamespace;

        // Split the namespace into parts
        var namespaceParts = ns.Split('.');

        // Start with the global namespace
        var currentNamespace = compilation.GlobalNamespace;

        // Traverse the namespace hierarchy
        foreach (var part in namespaceParts)
        {
            currentNamespace = currentNamespace.GetMembers(part).FirstOrDefault() as NamespaceSymbol;

            if (currentNamespace == null)
            {
                return null; // Namespace not found
            }
        }

        return currentNamespace;
    }
}