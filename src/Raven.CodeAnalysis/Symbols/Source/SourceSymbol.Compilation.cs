using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Symbols;

// Compatibility helpers for source components that still require Compilation/SemanticModel.
internal abstract partial class SourceSymbol
{
    protected Compilation? GetDeclaringCompilation()
    {
        if (this is SourceAssemblySymbol assemblySymbol)
            return assemblySymbol.Compilation;

        return ContainingAssembly is SourceAssemblySymbol containingAssembly
            ? containingAssembly.Compilation
            : null;
    }

    protected bool TryGetCurrentDeclaringSyntax(Compilation compilation, SyntaxReference reference,
        out SyntaxNode syntax, out SemanticModel semanticModel)
        => compilation.TryGetCurrentSourceDeclaration(reference, out syntax, out semanticModel);
}
