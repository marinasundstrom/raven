using System.Collections.Immutable;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// Logical type identity is compiler-owned; physical signature handles belong to each backend.
internal sealed record CallableSignature(EmissionType ReturnType, ImmutableArray<EmissionType> ParameterTypes, ImmutableArray<string> GenericParameterNames = default, bool IsInstance = false, int DeclaringTypeArity = 0, bool DeclaringTypeIsStatic = false, bool HasTypeBounds = false, bool HasSpecialTypeConstraints = false)
{
    internal int ParameterCount => ParameterTypes.Length;
    internal bool ReturnsValue => ReturnType.Primitive != EmissionPrimitiveType.NoResult;

    internal static bool TryType(ITypeSymbol type, bool result, out EmissionType value, EmissionCapabilities? capabilities = null)
    {
        value = default;
        if (type.IsNullable && type.GetNonNullableType().IsReferenceType)
            type = type.GetNonNullableType();
        if ((result ? EmissionPrimitiveTypes.TryGetReturnType(type, out var primitive) : EmissionPrimitiveTypes.TryGetValueType(type, out primitive)))
        { value = new(Primitive: primitive); return true; }
        if (type is ITypeParameterSymbol { DeclaringMethodParameterOwner: not null } parameter)
        { value = new(MethodParameter: parameter); return true; }
        if (type is ITypeParameterSymbol { DeclaringTypeParameterOwner: not null } ownerParameter)
        { value = new(OwnerParameter: ownerParameter); return true; }
        if (capabilities?.AllowsExternalValueSignatures == true && type is INamedTypeSymbol externalValue && IsExternalValue(externalValue) &&
            externalValue.TypeArguments.All(t => TryType(t, false, out _, capabilities)))
        { value = new(Nominal: externalValue); return true; }
        if (capabilities?.AllowsExternalReferenceSignatures == true && type is INamedTypeSymbol external && IsExternalReference(external) &&
            external.TypeArguments.All(t => TryType(t, false, out _, capabilities)))
        { value = new(Nominal: external); return true; }
        if (type is INamedTypeSymbol { TypeKind: TypeKind.Interface } contract && SourceInterfacePlan.HasSupportedIdentity(contract))
        { value = new(Nominal: contract); return true; }
        if (type is INamedTypeSymbol named && SourceTypePlan.TryCreate(named, out var plan) && !plan!.IsStatic)
        { value = new(Nominal: named); return true; }
        if (type is IArrayTypeSymbol { Rank: 1, FixedLength: null, ElementType: not IArrayTypeSymbol } array && TryType(array.ElementType, false, out _, capabilities))
        { value = new(Array: array); return true; }
        return false;
    }
    internal static bool IsExternalValue(INamedTypeSymbol type) =>
        type.OriginalDefinition.DeclaringSyntaxReferences.IsEmpty && type.TypeKind == TypeKind.Struct &&
        type.IsValueType && type.ContainingType is null && type.DeclaredAccessibility == Accessibility.Public;

    internal static bool IsExternalReference(INamedTypeSymbol type) =>
        type.OriginalDefinition.DeclaringSyntaxReferences.IsEmpty && type.TypeKind is TypeKind.Class or TypeKind.Interface &&
        type.IsReferenceType && !type.IsStatic && type.ContainingType is null && type.DeclaredAccessibility == Accessibility.Public;

    internal static bool TryCreate(IMethodSymbol method, out CallableSignature signature, EmissionCapabilities? capabilities = null)
    {
        signature = null!;
        if ((method.IsGenericMethod && method.TypeParameters.Any(p => p.ConstraintKind != TypeParameterConstraintKind.None || !p.ConstraintTypes.IsEmpty)) || method.IsExtensionMethod || method.IsAsync || !TryType(method.ReturnType, true, out var result, capabilities)) return false;
        if (method.ContainingType is { Arity: > 0 } owner && ((!SourceTypePlan.TryCreate(owner, out _) && !(capabilities?.AllowsExternalReferenceSignatures == true && IsExternalReference(owner))) || owner.TypeArguments.Any(t => !TryType(t, false, out _, capabilities)))) return false;
        if (method.IsGenericMethod && method.TypeArguments.Any(t => !TryType(t, false, out _, capabilities))) return false;
        var parameters = ImmutableArray.CreateBuilder<EmissionType>(method.Parameters.Length);
        foreach (var parameter in method.Parameters)
        {
            if (parameter.RefKind != RefKind.None || parameter.HasExplicitDefaultValue || parameter.IsVarParams || !TryType(parameter.Type, false, out var type, capabilities)) return false;
            parameters.Add(type);
        }
        signature = new(result, parameters.MoveToImmutable(), method.TypeParameters.Select(p => p.Name).ToImmutableArray(), !method.IsStatic, method.ContainingType?.Arity ?? 0, method.ContainingType?.IsStatic ?? false, method.ContainingType is { } declaring && ((INamedTypeSymbol)declaring.OriginalDefinition).TypeParameters.Any(p => !p.ConstraintTypes.IsEmpty),
            method.ContainingType is { } constrained && ((INamedTypeSymbol)constrained.OriginalDefinition).TypeParameters.Any(p => (p.ConstraintKind & (TypeParameterConstraintKind.ReferenceType | TypeParameterConstraintKind.ValueType | TypeParameterConstraintKind.Constructor)) != 0));
        return true;
    }
}
