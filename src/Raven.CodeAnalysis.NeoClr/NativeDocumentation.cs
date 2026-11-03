using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.NeoClr;

// Uses Raven's existing Markdown-first/XML-fallback sidecar contract, without reflection.
internal static class NativeDocumentation
{
    internal static DocumentationComment? Get(Symbol symbol)
        => symbol.ContainingAssembly is NativeAssemblySymbol assembly && DocumentationCommentIdBuilder.TryGetMemberId(symbol, out var id)
            ? assembly.Reference.Documentation?.GetDocumentationComment(id) : null;
}
