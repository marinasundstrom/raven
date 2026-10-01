using System.Collections.Immutable;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// Logical source ownership/categories. Physical CLI carrier types are adapter policy.
internal enum EmissionDeclarationKind { AssemblyFunction, NamespacedAssemblyFunction, StaticMethod, StaticType, RootClass, InstanceMethod, Constructor, PropertyAccessor, IndexerAccessor }

// Admission for the bounded shared plan, not a description of an entire runtime.
// Each adapter explicitly opts into supported logical operations and built-in types.
// This contains neither CLI opcodes nor backend metadata handles.
internal sealed class EmissionCapabilities(
    IEnumerable<EmissionPrimitiveType> types, IEnumerable<LinearInstructionKind> instructions,
    IEnumerable<EmissionDeclarationKind>? declarations = null,
    IEnumerable<Accessibility>? typeVisibilities = null,
    IEnumerable<Accessibility>? methodVisibilities = null,
    IEnumerable<Accessibility>? functionVisibilities = null,
    bool allowsRootClassLocals = false, bool allowsRootClassSignatures = false, bool allowsArrays = false, bool allowsGenericMethods = false, bool allowsGenericInstanceMethods = false, bool allowsGenericStaticOwners = false, bool allowsGenericClassOwners = false, bool allowsConstructedFieldReferences = false, bool allowsNominalTypeBounds = false)
{
    private readonly ImmutableHashSet<EmissionPrimitiveType> types = types.ToImmutableHashSet();
    private readonly ImmutableHashSet<LinearInstructionKind> instructions = instructions.ToImmutableHashSet();

    private readonly ImmutableHashSet<EmissionDeclarationKind> declarations = (declarations ?? []).ToImmutableHashSet();

    private readonly ImmutableHashSet<Accessibility> typeVisibilities = (typeVisibilities ?? []).ToImmutableHashSet();

    private readonly ImmutableHashSet<Accessibility> methodVisibilities = (methodVisibilities ?? []).ToImmutableHashSet();

    private readonly ImmutableHashSet<Accessibility> functionVisibilities = (functionVisibilities ?? []).ToImmutableHashSet();

    internal bool AllowsNominalTypeBounds { get; } = allowsNominalTypeBounds;
    internal bool AllowsGenericClassOwners { get; } = allowsGenericClassOwners;
    internal bool AllowsConstructedFieldReferences { get; } = allowsConstructedFieldReferences;
    internal bool AllowsGenericStaticOwners { get; } = allowsGenericStaticOwners;

    internal bool AllowsGenericInstanceMethods { get; } = allowsGenericInstanceMethods;

    internal bool AllowsGenericMethods { get; } = allowsGenericMethods;

    internal bool AllowsArrays { get; } = allowsArrays;

    internal bool AllowsRootClassSignatures { get; } = allowsRootClassSignatures;

    internal bool AllowsRootClassLocals { get; } = allowsRootClassLocals;

    internal bool AllowsFunctionVisibility(Accessibility visibility) => functionVisibilities.Contains(visibility);
    internal bool AllowsMethodVisibility(Accessibility visibility) => methodVisibilities.Contains(visibility);
    internal bool AllowsTypeVisibility(Accessibility visibility) => typeVisibilities.Contains(visibility);
    internal bool Allows(EmissionDeclarationKind declaration) => declarations.Contains(declaration);
    internal bool Allows(EmissionPrimitiveType type) => types.Contains(type);
    internal bool Allows(LinearInstructionKind instruction) => instructions.Contains(instruction);
    internal bool Allows(EmissionType type)
    {
        if (type.Primitive is { } primitive) return Allows(primitive);
        if (type.Array is { } array)
            return AllowsArrays && CallableSignature.TryType(array.ElementType, false, out var element) && Allows(element);
        if (type.OwnerParameter is { } parameter)
            return parameter.DeclaringTypeParameterOwner!.IsStatic ? AllowsGenericStaticOwners : AllowsGenericClassOwners;
        if (type.MethodParameter is not null) return AllowsGenericMethods;
        if (type.Class is not { } owner || !AllowsRootClassSignatures) return false;
        return owner.Arity == 0 || AllowsGenericClassOwners && SourceTypePlan.TryCreate(owner, out _, this) && owner.TypeArguments.All(t =>
            CallableSignature.TryType(t, false, out var argument) && Allows(argument));
    }
    internal bool Allows(CallableSignature signature) => (!signature.HasTypeBounds || AllowsNominalTypeBounds) && (signature.DeclaringTypeArity == 0 || (signature.DeclaringTypeIsStatic ? AllowsGenericStaticOwners : AllowsGenericClassOwners)) && (signature.GenericParameterNames.IsDefaultOrEmpty || AllowsGenericMethods && (!signature.IsInstance || AllowsGenericInstanceMethods)) && Allows(signature.ReturnType) && signature.ParameterTypes.All(Allows);
    internal bool Allows(PrimitiveCallableSignature signature)
        => Allows(signature.ReturnType) && signature.ParameterTypes.All(Allows);
}
