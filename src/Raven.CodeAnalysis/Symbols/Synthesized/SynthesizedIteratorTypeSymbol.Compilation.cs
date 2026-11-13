namespace Raven.CodeAnalysis.Symbols;

// Retained for unported bootstrap consumers. Shared synthesis logic uses Services.
internal sealed partial class SynthesizedIteratorTypeSymbol
{
    public SynthesizedIteratorTypeSymbol(Compilation compilation, MethodSymbol iteratorMethod,
        string name, IteratorMethodKind iteratorKind, TypeSymbol elementType, TypeSymbol? selfType = null)
        : this((StateMachineServices)compilation, iteratorMethod, name, iteratorKind, elementType, selfType)
    {
    }

    public Compilation Compilation => (Compilation)Services;
}
