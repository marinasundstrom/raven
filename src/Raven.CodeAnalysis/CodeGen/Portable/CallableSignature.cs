using Raven.CodeAnalysis.Symbols;

using System.Collections.Immutable;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// Logical type identity is compiler-owned; physical signature handles belong to each backend.
internal sealed record CallableSignature(EmissionType ReturnType, ImmutableArray<EmissionType> ParameterTypes, ImmutableArray<string> GenericParameterNames = default, bool IsInstance = false, int DeclaringTypeArity = 0, bool DeclaringTypeIsStatic = false, bool HasTypeBounds = false, bool HasSpecialTypeConstraints = false, ImmutableArray<int> OutParameters = default)
{
    internal int ParameterCount => ParameterTypes.Length;
    internal bool ReturnsValue => ReturnType.Primitive != EmissionPrimitiveType.NoResult;

    internal static bool TryType(ITypeSymbol type, bool result, out EmissionType value, EmissionCapabilities? capabilities = null, int depth = 0)
    {
        value = default;
        if (depth >= 16) return false;
        if (type.IsNullable && type.GetNonNullableType().IsReferenceType)
            type = type.GetNonNullableType();
        if ((result ? EmissionPrimitiveTypes.TryGetReturnType(type, out var primitive) : EmissionPrimitiveTypes.TryGetValueType(type, out primitive)))
        { value = new(Primitive: primitive); return true; }
        if (capabilities?.AllowsFunctionValues == true && type is INamedTypeSymbol { TypeKind: TypeKind.Delegate } function && TryFunction(function, out _, capabilities, depth + 1))
        { value = new(Nominal: function); return true; }
        if (type is ITypeParameterSymbol { DeclaringMethodParameterOwner: not null } parameter)
        { value = new(MethodParameter: parameter); return true; }
        if (type is ITypeParameterSymbol { DeclaringTypeParameterOwner: not null } ownerParameter)
        { value = new(OwnerParameter: ownerParameter); return true; }
        if (capabilities?.AllowsExternalValueSignatures == true && type is INamedTypeSymbol externalValue && IsExternalValue(externalValue, capabilities?.AllowsNestedExternalTypes == true) &&
            externalValue.TypeArguments.All(t => TryType(t, false, out _, capabilities, depth + 1)))
        { value = new(Nominal: externalValue); return true; }
        if (capabilities?.AllowsExternalReferenceSignatures == true && type is INamedTypeSymbol external && IsExternalReference(external, capabilities?.AllowsNestedExternalTypes == true) &&
            external.TypeArguments.All(t => TryType(t, false, out _, capabilities, depth + 1)))
        { value = new(Nominal: external); return true; }
        if (type is INamedTypeSymbol { TypeKind: TypeKind.Interface } contract && SourceInterfacePlan.HasSupportedIdentity(contract))
        { value = new(Nominal: contract); return true; }
        if (type is INamedTypeSymbol named && SourceTypePlan.TryCreate(named, out var plan, capabilities) && !plan!.IsStatic)
        { value = new(Nominal: named); return true; }
        if (type is IArrayTypeSymbol { Rank: 1, FixedLength: null } array && TryType(array.ElementType, false, out _, capabilities, depth + 1))
        { value = new(Array: array); return true; }
        return false;
    }
    internal static bool TryFunction(INamedTypeSymbol type, out CallableSignature signature, EmissionCapabilities capabilities, int depth = 0)
    {
        signature = null!;
        if (depth >= 16) return false;
        // The first transport profile admits core Func/Action shapes only. Named
        // delegates retain nominal semantics and must not silently lose identity.
        if (type.TypeKind != TypeKind.Delegate || type.ContainingNamespace?.ToDisplayString() != "System" ||
            type.Name is not ("Func" or "Action") ||
            !SymbolEqualityComparer.Default.Equals(type.ContainingAssembly, type.BaseType?.ContainingAssembly) || type.GetDelegateInvokeMethod() is not { } invoke ||
            invoke.Parameters.Any(p => p.RefKind != RefKind.None) || invoke.Parameters.Length > 16 ||
            !TryType(invoke.ReturnType, true, out var result, capabilities, depth + 1)) return false;
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
    internal static bool SameStorageType(ITypeSymbol left, ITypeSymbol right) =>
        SymbolEqualityComparer.Default.Equals(left, right) ||
        left is IUnionCaseTypeSymbol a && right is IUnionCaseTypeSymbol b &&
        SymbolEqualityComparer.Default.Equals(a.ContainingAssembly, b.ContainingAssembly) &&
        a.ToFullyQualifiedMetadataName() == b.ToFullyQualifiedMetadataName() &&
        Enumerable.SequenceEqual<ITypeSymbol>(a.TypeArguments, b.TypeArguments, SymbolEqualityComparer.Default);

    internal static bool IsExternalValue(INamedTypeSymbol type, bool allowNested = false) =>
        type.OriginalDefinition.DeclaringSyntaxReferences.IsEmpty && type.TypeKind == TypeKind.Struct &&
        type.IsValueType && (type.ContainingType is null || allowNested) && type.DeclaredAccessibility == Accessibility.Public;

    internal static bool IsExternalReference(INamedTypeSymbol type, bool allowNested = false) =>
        type.OriginalDefinition.DeclaringSyntaxReferences.IsEmpty && type.TypeKind is TypeKind.Class or TypeKind.Interface &&
        type.IsReferenceType && !type.IsStatic && (type.ContainingType is null || allowNested) && type.DeclaredAccessibility == Accessibility.Public;

    internal static bool TryCreate(IMethodSymbol method, out CallableSignature signature, EmissionCapabilities? capabilities = null)
    {
        signature = null!;
        if ((method.IsGenericMethod && method.TypeParameters.Any(p => p.ConstraintKind != TypeParameterConstraintKind.None || !p.ConstraintTypes.IsEmpty)) || method.IsExtensionMethod && (capabilities?.AllowsLoweredExtensionCalls != true || !method.IsStatic) || method.IsAsync || !TryType(method.ReturnType, true, out var result, capabilities)) return false;
        if (method.ContainingType is { Arity: > 0 } owner && ((!SourceTypePlan.TryCreate(owner, out _, capabilities) && !(capabilities?.AllowsConstructedInterfaceInheritance == true && SourceInterfacePlan.HasSupportedIdentity(owner)) && !(capabilities?.AllowsExternalReferenceSignatures == true && IsExternalReference(owner, capabilities?.AllowsNestedExternalTypes == true)) && !(capabilities?.AllowsExternalValueSignatures == true && IsExternalValue(owner, capabilities?.AllowsNestedExternalTypes == true))) || owner.TypeArguments.Any(t => !TryType(t, false, out _, capabilities)))) return false;
        if (method.IsGenericMethod && method.TypeArguments.Any(t => !TryType(t, false, out _, capabilities))) return false;
        var parameters = ImmutableArray.CreateBuilder<EmissionType>(method.Parameters.Length);
        foreach (var parameter in method.Parameters)
        {
            if ((parameter.RefKind != RefKind.None && (capabilities?.AllowsManagedReferences != true || parameter.RefKind is not (RefKind.Ref or RefKind.Out))) || parameter.HasExplicitDefaultValue || parameter.IsVarParams || !TryType(parameter.Type, false, out var type, capabilities)) return false;
            parameters.Add(type with { IsByReference = parameter.RefKind != RefKind.None });
        }
        signature = new(result, parameters.MoveToImmutable(), method.TypeParameters.Select(p => p.Name).ToImmutableArray(), !method.IsStatic, method.ContainingType?.OriginalDefinition is SourceNamedTypeSymbol { IsExtensionDeclaration: true } ? 0 : method.ContainingType?.Arity ?? 0, method.ContainingType is { } physicalOwner && SourceTypePlan.IsStaticContainer(physicalOwner), method.ContainingType is { } declaring && ((INamedTypeSymbol)declaring.OriginalDefinition).TypeParameters.Any(p => !p.ConstraintTypes.IsEmpty),
            method.ContainingType is { } constrained && ((INamedTypeSymbol)constrained.OriginalDefinition).TypeParameters.Any(p => (p.ConstraintKind & (TypeParameterConstraintKind.ReferenceType | TypeParameterConstraintKind.ValueType | TypeParameterConstraintKind.Constructor)) != 0), method.Parameters.Select((p, i) => (p, i)).Where(x => x.p.RefKind == RefKind.Out).Select(x => x.i).ToImmutableArray());
        return true;
    }
}
