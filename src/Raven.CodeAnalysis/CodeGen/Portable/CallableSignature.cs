using System.Collections.Immutable;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// Logical type identity is compiler-owned; physical signature handles belong to each backend.
internal sealed record CallableSignature(EmissionType ReturnType, ImmutableArray<EmissionType> ParameterTypes)
{
    internal int ParameterCount => ParameterTypes.Length;
    internal bool ReturnsValue => ReturnType.Primitive != EmissionPrimitiveType.NoResult;

    internal static bool TryType(ITypeSymbol type, bool result, out EmissionType value)
    {
        value = default;
        if ((result ? EmissionPrimitiveTypes.TryGetReturnType(type, out var primitive) : EmissionPrimitiveTypes.TryGetValueType(type, out primitive)))
        { value = new(Primitive: primitive); return true; }
        if (type is INamedTypeSymbol named && SourceTypePlan.TryCreate(named, out var plan) && !plan!.IsStatic)
        { value = new(Class: named); return true; }
        return false;
    }
    internal static bool TryCreate(IMethodSymbol method, out CallableSignature signature)
    {
        signature = null!;
        if (method.IsGenericMethod || method.IsExtensionMethod || method.IsAsync || !TryType(method.ReturnType, true, out var result)) return false;
        var parameters = ImmutableArray.CreateBuilder<EmissionType>(method.Parameters.Length);
        foreach (var parameter in method.Parameters)
        {
            if (parameter.RefKind != RefKind.None || parameter.HasExplicitDefaultValue || parameter.IsVarParams || !TryType(parameter.Type, false, out var type)) return false;
            parameters.Add(type);
        }
        signature = new(result, parameters.MoveToImmutable());
        return true;
    }
}
