namespace Raven.CodeAnalysis.Symbols;

// Distinguishes the generated entry point from a source function named Main.
internal interface SynthesizedEntryPointMethod : MethodSymbol
{
    bool ContainsExecutableCode { get; }
    MethodSymbol? AsyncImplementation { get; }
}
