using System.Collections.Immutable;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// Admission for the bounded shared plan, not a description of an entire runtime.
// Each adapter explicitly opts into supported logical operations and built-in types.
// This contains neither CLI opcodes nor backend metadata handles.
internal sealed class EmissionCapabilities(
    IEnumerable<EmissionPrimitiveType> types, IEnumerable<LinearInstructionKind> instructions)
{
    private readonly ImmutableHashSet<EmissionPrimitiveType> types = types.ToImmutableHashSet();
    private readonly ImmutableHashSet<LinearInstructionKind> instructions = instructions.ToImmutableHashSet();

    internal bool Allows(EmissionPrimitiveType type) => types.Contains(type);
    internal bool Allows(LinearInstructionKind instruction) => instructions.Contains(instruction);
    internal bool Allows(PrimitiveCallableSignature signature)
        => Allows(signature.ReturnType) && signature.ParameterTypes.All(Allows);
}
