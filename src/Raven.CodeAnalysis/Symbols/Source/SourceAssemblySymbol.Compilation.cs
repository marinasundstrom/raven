namespace Raven.CodeAnalysis.Symbols;

// Retained for unported bootstrap consumers. Shared container logic uses Services.
internal partial class SourceAssemblySymbol
{
    public SourceAssemblySymbol(Compilation compilation, string name,
        SyntaxReference[] declaringSyntaxReferences, Location[]? locations = null)
        : this((SourceContainerServices)compilation, name, declaringSyntaxReferences, locations)
    {
    }

    internal Compilation Compilation => (Compilation)Services;
}
