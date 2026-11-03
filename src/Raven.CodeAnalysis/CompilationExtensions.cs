using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public static class CompilationExtensions
{
    public static NamespaceSymbol? GetNamespaceSymbol(this Compilation compilation, string? ns)
    {
        return compilation.GetNamespaceSymbolCached(ns);
    }
}
