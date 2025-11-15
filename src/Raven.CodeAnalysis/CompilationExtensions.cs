using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public static class CompilationExtensions
{
    public static INamespaceSymbol? GetNamespaceSymbol(this Compilation compilation, string? ns)
    {
        if (ns is null)
            return compilation.GlobalNamespace;

        var namespaceParts = ns.Split('.', StringSplitOptions.RemoveEmptyEntries);
        if (namespaceParts.Length == 0)
            return compilation.GlobalNamespace;

        var fromSource = TryResolve(compilation.GlobalNamespace, namespaceParts);
        if (fromSource is not null)
            return fromSource;

        foreach (var referencedAssembly in compilation.ReferencedAssemblySymbols)
        {
            var candidate = TryResolve(referencedAssembly.GlobalNamespace, namespaceParts);
            if (candidate is not null)
                return candidate;
        }

        return null;

        static INamespaceSymbol? TryResolve(INamespaceSymbol root, string[] parts)
        {
            var current = root;
            foreach (var part in parts)
            {
                current = current.GetMembers(part)
                    .OfType<INamespaceSymbol>()
                    .FirstOrDefault();

                if (current is null)
                    return null;
            }

            return current;
        }
    }
}
