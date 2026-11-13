namespace Raven.CodeAnalysis.Symbols;

// Retained for unported bootstrap consumers. Shared synthesis logic uses Services.
internal sealed partial class SynthesizedAsyncStateMachineTypeSymbol
{
    public SynthesizedAsyncStateMachineTypeSymbol(Compilation compilation, MethodSymbol asyncMethod,
        string name, TypeSymbol? selfType = null)
        : this((StateMachineServices)compilation, asyncMethod, name, selfType)
    {
    }

    public Compilation Compilation => (Compilation)Services;
}
