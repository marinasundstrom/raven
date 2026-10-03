using System.Collections.Immutable;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// Logical source ownership/categories. Physical CLI carrier types are adapter policy.
internal enum EmissionDeclarationKind { AssemblyFunction, NamespacedAssemblyFunction, StaticMethod, StaticType, RootClass, ValueType, NestedType, InstanceMethod, Constructor, PropertyAccessor, IndexerAccessor, Interface, InterfaceMethod, InterfaceProperty, InterfaceIndexer, InterfaceInheritance, InterfaceImplementation, ValueInterfaceImplementation, ValueObjectOverride }

// Admission for the bounded shared plan, not a description of an entire runtime.
// Each adapter explicitly opts into supported logical operations and built-in types.
// This contains neither CLI opcodes nor backend metadata handles.
internal sealed class EmissionCapabilities(
    IEnumerable<EmissionPrimitiveType> types, IEnumerable<LinearInstructionKind> instructions,
    IEnumerable<EmissionDeclarationKind>? declarations = null,
    IEnumerable<Accessibility>? typeVisibilities = null,
    IEnumerable<Accessibility>? methodVisibilities = null,
    IEnumerable<Accessibility>? functionVisibilities = null,
    bool allowsRootClassLocals = false, bool allowsRootClassSignatures = false, bool allowsArrays = false, bool allowsGenericMethods = false, bool allowsGenericInstanceMethods = false, bool allowsGenericStaticOwners = false, bool allowsGenericClassOwners = false, bool allowsConstructedFieldReferences = false, bool allowsNominalTypeBounds = false, bool allowsSpecialTypeConstraints = false, bool allowsGenericInterfaceDeclarations = false, bool allowsInterfaceSignatures = false, bool allowsInterfaceDispatch = false, bool allowsExternalReferenceSignatures = false, bool allowsExternalValueSignatures = false, bool allowsExternalInstanceCalls = false, bool allowsManagedReferences = false, bool allowsExternalValueInstanceCalls = false, bool allowsExternalConstructors = false, bool allowsNestedExternalTypes = false, bool allowsFunctionValues = false, bool allowsLoweredExtensionCalls = false, bool allowsCasePatterns = false, bool allowsReferenceEnumeration = false, bool allowsConstructedInterfaceInheritance = false, bool allowsConstructedInterfaceImplementations = false, bool allowsExternalInterfaceDeclarations = false)
{
    private readonly ImmutableHashSet<EmissionPrimitiveType> types = types.ToImmutableHashSet();
    private readonly ImmutableHashSet<LinearInstructionKind> instructions = instructions.ToImmutableHashSet();

    private readonly ImmutableHashSet<EmissionDeclarationKind> declarations = (declarations ?? []).ToImmutableHashSet();

    private readonly ImmutableHashSet<Accessibility> typeVisibilities = (typeVisibilities ?? []).ToImmutableHashSet();

    private readonly ImmutableHashSet<Accessibility> methodVisibilities = (methodVisibilities ?? []).ToImmutableHashSet();

    private readonly ImmutableHashSet<Accessibility> functionVisibilities = (functionVisibilities ?? []).ToImmutableHashSet();

    internal bool AllowsExternalInterfaceDeclarations { get; } = allowsExternalInterfaceDeclarations;
    internal bool AllowsConstructedInterfaceInheritance { get; } = allowsConstructedInterfaceInheritance;
    internal bool AllowsConstructedInterfaceImplementations { get; } = allowsConstructedInterfaceImplementations;
    internal bool AllowsReferenceEnumeration { get; } = allowsReferenceEnumeration;
    internal bool AllowsCasePatterns { get; } = allowsCasePatterns;
    internal bool AllowsLoweredExtensionCalls { get; } = allowsLoweredExtensionCalls;
    internal bool AllowsFunctionValues { get; } = allowsFunctionValues;
    internal bool AllowsExternalConstructors { get; } = allowsExternalConstructors;
    internal bool AllowsNestedExternalTypes { get; } = allowsNestedExternalTypes;
    internal bool AllowsExternalValueInstanceCalls { get; } = allowsExternalValueInstanceCalls;
    internal bool AllowsManagedReferences { get; } = allowsManagedReferences;
    internal bool AllowsExternalInstanceCalls { get; } = allowsExternalInstanceCalls;
    internal bool AllowsExternalValueSignatures { get; } = allowsExternalValueSignatures;
    internal bool AllowsExternalReferenceSignatures { get; } = allowsExternalReferenceSignatures;
    internal bool AllowsInterfaceDispatch { get; } = allowsInterfaceDispatch;
    internal bool AllowsInterfaceSignatures { get; } = allowsInterfaceSignatures;
    internal bool AllowsGenericInterfaceDeclarations { get; } = allowsGenericInterfaceDeclarations;
    internal bool AllowsSpecialTypeConstraints { get; } = allowsSpecialTypeConstraints;
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
        if (type.Nominal is { TypeKind: TypeKind.Delegate } function)
            return AllowsFunctionValues && CallableSignature.TryFunction(function, out var shape, this) && Allows(shape);
        if (type.IsByReference) return AllowsManagedReferences && Allows(type with { IsByReference = false });
        if (type.Nominal is { } externalValue && AllowsExternalValueSignatures && CallableSignature.IsExternalValue(externalValue, AllowsNestedExternalTypes))
            return externalValue.Arity == 0 || AllowsGenericClassOwners && externalValue.TypeArguments.All(t => CallableSignature.TryType(t, false, out var argument, this) && Allows(argument));
        if (type.Nominal is { } external && AllowsExternalReferenceSignatures && CallableSignature.IsExternalReference(external, AllowsNestedExternalTypes))
            return (external.TypeKind == TypeKind.Interface ? AllowsInterfaceSignatures : AllowsRootClassSignatures) &&
                (external.Arity == 0 || AllowsGenericClassOwners && external.TypeArguments.All(t => CallableSignature.TryType(t, false, out var argument, this) && Allows(argument)));
        if (type.Primitive is { } primitive) return Allows(primitive);
        if (type.Array is { } array)
            return AllowsArrays && CallableSignature.TryType(array.ElementType, false, out var element, this) && Allows(element);
        if (type.OwnerParameter is { } parameter)
            return parameter.DeclaringTypeParameterOwner!.TypeKind == TypeKind.Interface ? AllowsGenericInterfaceDeclarations
                : parameter.DeclaringTypeParameterOwner.IsStatic ? AllowsGenericStaticOwners : AllowsGenericClassOwners;
        if (type.MethodParameter is not null) return AllowsGenericMethods;
        if (type.Nominal is { TypeKind: TypeKind.Interface } contract)
            return AllowsInterfaceSignatures && SourceInterfacePlan.HasSupportedIdentity(contract) &&
                (contract.Arity == 0 || AllowsGenericInterfaceDeclarations && contract.TypeArguments.All(t =>
                    CallableSignature.TryType(t, false, out var argument, this) && Allows(argument)));
        if (type.Nominal is not { } owner || !AllowsRootClassSignatures) return false;
        return SourceTypePlan.TryCreate(owner, out _, this) && (owner.Arity == 0 || AllowsGenericClassOwners && owner.TypeArguments.All(t =>
            CallableSignature.TryType(t, false, out var argument, this) && Allows(argument)));
    }
    internal bool Allows(CallableSignature signature) => (!signature.HasSpecialTypeConstraints || AllowsSpecialTypeConstraints) && (!signature.HasTypeBounds || AllowsNominalTypeBounds) && (signature.DeclaringTypeArity == 0 || (signature.DeclaringTypeIsStatic ? AllowsGenericStaticOwners : AllowsGenericClassOwners)) && (signature.GenericParameterNames.IsDefaultOrEmpty || AllowsGenericMethods && (!signature.IsInstance || AllowsGenericInstanceMethods)) && Allows(signature.ReturnType) && signature.ParameterTypes.All(Allows);
    internal bool Allows(PrimitiveCallableSignature signature)
        => Allows(signature.ReturnType) && signature.ParameterTypes.All(Allows);
}
