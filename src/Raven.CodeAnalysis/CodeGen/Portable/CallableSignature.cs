using Raven.CodeAnalysis.Symbols;

using System.Collections.Immutable;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// Logical type identity is compiler-owned; physical signature handles belong to each backend.
internal sealed record CallableSignature(EmissionType ReturnType, ImmutableArray<EmissionType> ParameterTypes, ImmutableArray<string> GenericParameterNames = default, bool IsInstance = false, int DeclaringTypeArity = 0, bool DeclaringTypeIsStatic = false, bool HasTypeBounds = false, bool HasSpecialTypeConstraints = false, ImmutableArray<int> OutParameters = default)
{
    internal int ParameterCount => ParameterTypes.Length;
    internal bool ReturnsValue => ReturnType.Primitive != EmissionPrimitiveType.NoResult;

    internal static bool TryType(TypeSymbol type, bool result, out EmissionType value, EmissionCapabilities? capabilities = null, int depth = 0)
    {
        value = default;
        if (depth >= 16) return false;
        if (capabilities?.AllowsFunctionReturnProjection == true && type is FunctionReturnTypeSymbol projection)
        { value = new(FunctionReturn: projection); return true; }
        if (type is TupleTypeSymbol tuple)
        {
            if (capabilities?.Allows(LinearInstructionKind.NewTuple) != true || tuple.TupleElements.Length is 0 or > 256 || tuple.TupleElements.Any(e =>
                e.Type is not TupleTypeSymbol && !EmissionPrimitiveTypes.TryGetValueType(e.Type, out _) ||
                !TryType(e.Type, false, out _, capabilities, depth + 1))) return false;
            value = new(Nominal: (NamedTypeSymbol)tuple);
            return true;
        }
        if (capabilities?.AllowsImportedCompositionCalls == true && type is StructuralUnionTypeSymbol or IntersectionTypeSymbol)
        {
            value = new(Composition: type);
            return true;
        }
        if (type.GetNullableAbiProjection() == NullableAbiProjection.AnnotatedUnderlyingType)
            type = type.GetNonNullableType();
        if ((result ? EmissionPrimitiveTypes.TryGetReturnType(type, out var primitive) : EmissionPrimitiveTypes.TryGetValueType(type, out primitive)))
        { value = new(Primitive: primitive); return true; }
        if (type is PointerTypeSymbol pointer)
        {
            if (capabilities?.AllowsUnmanagedPointers != true || !IsSupportedPointer(pointer, allowGenericTargets: capabilities.AllowsGenericPointerTargets, allowValueTargets: capabilities.AllowsValuePointerTargets)) return false;
            value = new(Pointer: pointer);
            return true;
        }
        // Callable unit results remain no-result above. In value positions the
        // explicit runtime contract supplies an inhabited nominal representation.
        if (!result && type is UnitTypeSymbol { RuntimeRepresentation: { } representation })
            return TryType(representation, false, out value, capabilities, depth + 1);
        if (capabilities?.AllowsFunctionValues == true && type is NamedTypeSymbol { TypeKind: TypeKind.Delegate } function && TryFunction(function, out _, capabilities, depth + 1))
        { value = new(Nominal: function); return true; }
        if (type is TypeParameterSymbol { DeclaringMethodParameterOwner: not null } parameter)
        { value = new(MethodParameter: parameter); return true; }
        if (type is TypeParameterSymbol { DeclaringTypeParameterOwner: not null } ownerParameter)
        { value = new(OwnerParameter: ownerParameter); return true; }
        if (capabilities?.Allows(EmissionDeclarationKind.Enum) == true && type is NamedTypeSymbol { TypeKind: TypeKind.Enum, EnumUnderlyingType.SpecialType: SpecialType.System_Int32 } enumType)
        { value = new(Nominal: enumType); return true; }
        if (capabilities?.AllowsExternalValueSignatures == true && type is NamedTypeSymbol externalValue && IsExternalValue(externalValue, capabilities?.AllowsNestedExternalTypes == true, capabilities?.AllowsInternalExternalSymbols == true) &&
            externalValue.TypeArguments.All(t => t is not PointerTypeSymbol && TryType(t, false, out _, capabilities, depth + 1)))
        { value = new(Nominal: externalValue); return true; }
        if (capabilities?.AllowsExternalReferenceSignatures == true && type is NamedTypeSymbol external && IsExternalReference(external, capabilities?.AllowsNestedExternalTypes == true, capabilities?.AllowsInternalExternalSymbols == true) &&
            external.TypeArguments.All(t => t is not PointerTypeSymbol && TryType(t, false, out _, capabilities, depth + 1)))
        { value = new(Nominal: external); return true; }
        if (type is NamedTypeSymbol { TypeKind: TypeKind.Interface } contract && SourceInterfacePlan.HasSupportedIdentity(contract, capabilities))
        { value = new(Nominal: contract); return true; }
        if (type is NamedTypeSymbol named && SourceTypePlan.TryCreate(named, out var plan, capabilities) && !plan!.IsStatic)
        { value = new(Nominal: named); return true; }
        if (type is ArrayTypeSymbol { Rank: 1, FixedLength: null } array && array.ElementType is not (PointerTypeSymbol or TupleTypeSymbol) && TryType(array.ElementType, false, out _, capabilities, depth + 1))
        { value = new(Array: array); return true; }
        return false;
    }
    internal static bool IsSupportedPointer(PointerTypeSymbol pointer, int depth = 0, bool allowGenericTargets = false, bool allowValueTargets = false) =>
        depth < 16 && (pointer.PointedAtType is PointerTypeSymbol nested
            ? IsSupportedPointer(nested, depth + 1, allowGenericTargets, allowValueTargets)
            : allowValueTargets && pointer.PointedAtType is NamedTypeSymbol { IsValueType: true, IsRefLikeType: false } ||
              allowGenericTargets && pointer.PointedAtType is TypeParameterSymbol || EmissionPrimitiveTypes.TryGetReturnType(pointer.PointedAtType, out var primitive) && primitive != EmissionPrimitiveType.String);

    internal static bool TryFunction(NamedTypeSymbol type, out CallableSignature signature, EmissionCapabilities capabilities, int depth = 0)
    {
        signature = null!;
        if (depth >= 16) return false;
        // Native signatures are distinct symbols. Core Func/Action remain an explicit
        // legacy transport contract; that transport does not grant structural identity.
        var nativeTransport = type is StructuralFunctionTypeSymbol;
        if (type.TypeKind != TypeKind.Delegate ||
            !nativeTransport && (type.ContainingNamespace?.ToDisplayString() != "System" || type.Name is not ("Func" or "Action") ||
                !SymbolEqualityComparer.Default.Equals(type.ContainingAssembly, type.BaseType?.ContainingAssembly)) ||
            type.GetDelegateInvokeMethod() is not { } invoke ||
            invoke.Parameters.Any(p => p.RefKind != RefKind.None || p.Type is PointerTypeSymbol) || invoke.ReturnType is PointerTypeSymbol || invoke.Parameters.Length > 16 ||
            !TryType(invoke.ReturnType, type.Name == "Action" || invoke.ReturnType.SpecialType == SpecialType.System_Void, out var result, capabilities, depth + 1)) return false;
        var parameters = ImmutableArray.CreateBuilder<EmissionType>();
        foreach (var parameter in invoke.Parameters)
        {
            if (!TryType(parameter.Type, false, out var value, capabilities, depth + 1)) return false;
            parameters.Add(value);
        }
        signature = new(result, parameters.ToImmutable());
        return true;
    }
    // Union cases can carry different semantic carrier substitutions while sharing
    // one physical nested case definition (notably nongeneric None).
    internal static bool SameStorageType(TypeSymbol left, TypeSymbol right) =>
        SymbolEqualityComparer.Default.Equals(left, right) ||
        left is UnionCaseTypeSymbol a && right is UnionCaseTypeSymbol b &&
        SymbolEqualityComparer.Default.Equals(a.ContainingAssembly, b.ContainingAssembly) &&
        a.ToFullyQualifiedMetadataName() == b.ToFullyQualifiedMetadataName() &&
        Enumerable.SequenceEqual<TypeSymbol>(a.TypeArguments, b.TypeArguments, SymbolEqualityComparer.Default);

    internal static bool IsExternalValue(NamedTypeSymbol type, bool allowNested = false, bool allowInternal = false) =>
        type.OriginalDefinition?.DeclaringSyntaxReferences.IsEmpty == true && type.TypeKind == TypeKind.Struct &&
        type.IsValueType && (type.ContainingType is null || allowNested) &&
        (type.DeclaredAccessibility == Accessibility.Public || allowInternal && type.DeclaredAccessibility == Accessibility.Internal);

    internal static bool IsExternalReference(NamedTypeSymbol type, bool allowNested = false, bool allowInternal = false) =>
        type.OriginalDefinition?.DeclaringSyntaxReferences.IsEmpty == true && type.TypeKind is TypeKind.Class or TypeKind.Interface &&
        type.IsReferenceType && !type.IsStatic && (type.ContainingType is null || allowNested) &&
        (type.DeclaredAccessibility == Accessibility.Public || allowInternal && type.DeclaredAccessibility == Accessibility.Internal);

    internal static bool AllowsParameterArray(MethodSymbol method, ParameterSymbol parameter, EmissionCapabilities? capabilities) =>
        !parameter.IsVarParams || capabilities?.AllowsParameterArrays == true && parameter.RefKind == RefKind.None &&
        parameter.Type is ArrayTypeSymbol { Rank: 1, FixedLength: null } &&
        SymbolEqualityComparer.Default.Equals(parameter, method.Parameters.LastOrDefault());

    internal static bool TryCreate(MethodSymbol method, out CallableSignature signature, EmissionCapabilities? capabilities = null)
    {
        signature = null!;
        bool ClosedCompositionBound(TypeSymbol type) => type is ArrayTypeSymbol array
            ? ClosedCompositionBound(array.ElementType)
            : type is NamedTypeSymbol { IsUnboundGenericType: false } named &&
                named.TypeArguments.All(ClosedCompositionBound) &&
                (named.ContainingType is null || ClosedCompositionBound(named.ContainingType));
        bool AdmittedComposition(TypeSymbol type)
        {
            var bounds = type switch
            {
                IntersectionTypeSymbol intersection => intersection.ConstituentTypes,
                StructuralUnionTypeSymbol union => union.ConstituentTypes,
                _ => default
            };
            bool External(NamedTypeSymbol nominal) => IsExternalReference(nominal,
                capabilities?.AllowsNestedExternalTypes == true, capabilities?.AllowsInternalExternalSymbols == true);
            if (!bounds.IsDefault)
                return bounds.All(t => t is NamedTypeSymbol { IsReferenceType: true } nominal &&
                    ClosedCompositionBound(nominal) && External(nominal));
            if (type is not NamedTypeSymbol { TypeKind: TypeKind.Class, IsUnboundGenericType: false } wrapper || !External(wrapper))
                return false;
            var hasComposition = false;
            foreach (var argument in wrapper.TypeArguments)
            {
                if (ClosedCompositionBound(argument)) continue;
                if (!AdmittedComposition(argument)) return false;
                hasComposition = true;
            }
            return hasComposition;
        }
        bool TryCallType(TypeSymbol type, bool result, out EmissionType value)
        {
            if (capabilities?.AllowsImportedCompositionCalls == true && method.IsStatic &&
                method.ContainingType is null && !method.IsGenericMethod && method.DeclaringSyntaxReferences.IsEmpty &&
                AdmittedComposition(type))
            {
                value = new(Composition: type);
                return true;
            }
            if (result && capabilities?.AllowsFunctionReturnProjection == true &&
                (method.OriginalDefinition is { DeclaredFunctionType: null, ReturnType: FunctionReturnTypeSymbol } ||
                 method.DeclaredFunctionType is StructuralFunctionTypeSymbol { ReturnType.SpecialType: not SpecialType.System_Void }))
                return TryType(type, false, out value, capabilities);
            return TryType(type, result, out value, capabilities);
        }
        bool UnsupportedConstraint(TypeParameterSymbol parameter) =>
            !(capabilities?.AllowsMethodFunctionTypeConstraints == true && parameter.ConstraintKind == TypeParameterConstraintKind.FunctionType && parameter.ConstraintTypes.IsEmpty) &&
            (parameter.ConstraintKind != TypeParameterConstraintKind.None || !parameter.ConstraintTypes.IsEmpty) &&
            (capabilities?.AllowsMethodInterfaceBounds != true ||
             (parameter.ConstraintKind & ~(TypeParameterConstraintKind.TypeConstraint | (capabilities?.AllowsMethodCategoryConstraints == true ? TypeParameterConstraintKind.ReferenceType | TypeParameterConstraintKind.ValueType | TypeParameterConstraintKind.Constructor : TypeParameterConstraintKind.None))) != 0 ||
             !(capabilities?.AllowsImportedCompositionCalls == true && parameter.ConstraintTypes.Length > 1 &&
                parameter.ConstraintTypes.All(t => t is NamedTypeSymbol { IsReferenceType: true } named && ClosedCompositionBound(named))) &&
             parameter.ConstraintTypes.Any(t => t is not NamedTypeSymbol { TypeKind: TypeKind.Interface, Arity: 0 } &&
                !(capabilities?.AllowsMethodClassBounds == true && t is NamedTypeSymbol { TypeKind: TypeKind.Class, Arity: 0, IsStatic: false }) &&
                !(capabilities?.AllowsImportedCompositionCalls == true && StructuralConstraint(t))));
        bool StructuralConstraint(TypeSymbol type)
        {
            var bounds = type switch
            {
                StructuralUnionTypeSymbol union => union.ConstituentTypes,
                IntersectionTypeSymbol intersection => intersection.ConstituentTypes,
                _ => default
            };
            return !bounds.IsDefault && bounds.All(t => t is NamedTypeSymbol { IsReferenceType: true } named && ClosedCompositionBound(named));
        }
        if (method.IsGenericMethod && method.TypeParameters.Any(UnsupportedConstraint) ||
            method.IsExtensionMethod && (capabilities?.AllowsLoweredExtensionCalls != true || !method.IsStatic) ||
            method.IsAsync && capabilities?.Allows(EmissionDeclarationKind.AsyncMethod) != true || !TryCallType(method.ReturnType, true, out var result)) return false;
        if (method.ContainingType is { Arity: > 0 } owner && ((!SourceTypePlan.TryCreate(owner, out _, capabilities) && !(capabilities?.AllowsConstructedInterfaceInheritance == true && SourceInterfacePlan.HasSupportedIdentity(owner, capabilities)) && !(capabilities?.AllowsExternalReferenceSignatures == true && IsExternalReference(owner, capabilities?.AllowsNestedExternalTypes == true, capabilities?.AllowsInternalExternalSymbols == true)) && !(capabilities?.AllowsExternalValueSignatures == true && IsExternalValue(owner, capabilities?.AllowsNestedExternalTypes == true, capabilities?.AllowsInternalExternalSymbols == true))) || owner.TypeArguments.Any(t => !TryType(t, false, out _, capabilities)))) return false;
        if (method.IsGenericMethod && method.TypeArguments.Any(t => t is PointerTypeSymbol || !TryType(t, false, out _, capabilities))) return false;
        var parameters = ImmutableArray.CreateBuilder<EmissionType>(method.Parameters.Length);
        foreach (var parameter in method.Parameters)
        {
            if ((parameter.RefKind != RefKind.None && (capabilities?.AllowsManagedReferences != true || parameter.Type is TupleTypeSymbol || parameter.RefKind is not (RefKind.Ref or RefKind.Out))) || (parameter.HasExplicitDefaultValue && (capabilities?.AllowsParameterDefaults != true || !SupportsParameterDefault(parameter))) || !AllowsParameterArray(method, parameter, capabilities) || !TryCallType(parameter.Type, false, out var type)) return false;
            parameters.Add(type with { IsByReference = parameter.RefKind != RefKind.None });
        }
        signature = new(result, parameters.MoveToImmutable(), method.TypeParameters.Select(p => p.Name).ToImmutableArray(), !method.IsStatic, method.ContainingType?.OriginalDefinition is SourceNamedTypeSymbol { IsExtensionDeclaration: true } ? 0 : method.ContainingType?.Arity ?? 0, method.ContainingType is { } physicalOwner && SourceTypePlan.IsStaticContainer(physicalOwner), method.ContainingType is { } declaring && ((NamedTypeSymbol)declaring.OriginalDefinition).TypeParameters.Any(p => !p.ConstraintTypes.IsEmpty),
            method.ContainingType is { } constrained && ((NamedTypeSymbol)constrained.OriginalDefinition).TypeParameters.Any(p => (p.ConstraintKind & (TypeParameterConstraintKind.ReferenceType | TypeParameterConstraintKind.ValueType | TypeParameterConstraintKind.Constructor | TypeParameterConstraintKind.FunctionType)) != 0), method.Parameters.Select((p, i) => (p, i)).Where(x => x.p.RefKind == RefKind.Out).Select(x => x.i).ToImmutableArray());
        return true;
    }
    // Explicit target capability: ordinary .NET emission keeps its own constant rules.
    internal static bool SupportsParameterDefault(ParameterSymbol parameter)
    {
        if (parameter.RefKind != RefKind.None || parameter.IsVarParams) return false;
        if (parameter.ExplicitDefaultValue is null)
            return parameter.Type.IsReferenceType ||
                parameter.Type is NamedTypeSymbol { TypeKind: TypeKind.Struct, SpecialType: SpecialType.None } &&
                parameter.Type.TryGetUnion() is null;
        return parameter.Type.SpecialType switch
        {
            SpecialType.System_Boolean => parameter.ExplicitDefaultValue is bool,
            SpecialType.System_SByte => parameter.ExplicitDefaultValue is sbyte,
            SpecialType.System_Byte => parameter.ExplicitDefaultValue is byte,
            SpecialType.System_Int16 => parameter.ExplicitDefaultValue is short,
            SpecialType.System_UInt16 => parameter.ExplicitDefaultValue is ushort,
            SpecialType.System_Int32 => parameter.ExplicitDefaultValue is int,
            SpecialType.System_UInt32 => parameter.ExplicitDefaultValue is uint,
            SpecialType.System_Int64 => parameter.ExplicitDefaultValue is long,
            SpecialType.System_UInt64 => parameter.ExplicitDefaultValue is ulong,
            SpecialType.System_Single => parameter.ExplicitDefaultValue is float,
            SpecialType.System_Double => parameter.ExplicitDefaultValue is double,
            SpecialType.System_String => parameter.ExplicitDefaultValue is string,
            _ => false
        };
    }

}
