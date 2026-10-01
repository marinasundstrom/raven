using System.Collections.Immutable;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// Logical source ownership/categories. Physical CLI carrier types are adapter policy.
internal enum EmissionDeclarationKind { AssemblyFunction, NamespacedAssemblyFunction, StaticMethod, StaticType, RootClass, InstanceMethod }

// Admission for the bounded shared plan, not a description of an entire runtime.
// Each adapter explicitly opts into supported logical operations and built-in types.
// This contains neither CLI opcodes nor backend metadata handles.
internal sealed class EmissionCapabilities(
    IEnumerable<EmissionPrimitiveType> types, IEnumerable<LinearInstructionKind> instructions,
    IEnumerable<EmissionDeclarationKind>? declarations = null,
    IEnumerable<Accessibility>? typeVisibilities = null,
    IEnumerable<Accessibility>? methodVisibilities = null,
    IEnumerable<Accessibility>? functionVisibilities = null)
{
    private readonly ImmutableHashSet<EmissionPrimitiveType> types = types.ToImmutableHashSet();
    private readonly ImmutableHashSet<LinearInstructionKind> instructions = instructions.ToImmutableHashSet();

    private readonly ImmutableHashSet<EmissionDeclarationKind> declarations = (declarations ?? []).ToImmutableHashSet();

    private readonly ImmutableHashSet<Accessibility> typeVisibilities = (typeVisibilities ?? []).ToImmutableHashSet();

    private readonly ImmutableHashSet<Accessibility> methodVisibilities = (methodVisibilities ?? []).ToImmutableHashSet();

    private readonly ImmutableHashSet<Accessibility> functionVisibilities = (functionVisibilities ?? []).ToImmutableHashSet();

    internal bool AllowsFunctionVisibility(Accessibility visibility) => functionVisibilities.Contains(visibility);
    internal bool AllowsMethodVisibility(Accessibility visibility) => methodVisibilities.Contains(visibility);
    internal bool AllowsTypeVisibility(Accessibility visibility) => typeVisibilities.Contains(visibility);
    internal bool Allows(EmissionDeclarationKind declaration) => declarations.Contains(declaration);
    internal bool Allows(EmissionPrimitiveType type) => types.Contains(type);
    internal bool Allows(LinearInstructionKind instruction) => instructions.Contains(instruction);
    internal bool Allows(PrimitiveCallableSignature signature)
        => Allows(signature.ReturnType) && signature.ParameterTypes.All(Allows);
}
