namespace Raven.CodeAnalysis.CodeGen.Portable;

// Common signature shape for the current native subset. Ownership, visibility,
// target type handles and on-disk signature encoding remain backend responsibilities.
internal sealed record PrimitiveCallableSignature(EmissionPrimitiveType ReturnType, System.Collections.Immutable.ImmutableArray<EmissionPrimitiveType> ParameterTypes)
{
    internal int ParameterCount => ParameterTypes.Length;
    internal bool ReturnsValue => ReturnType != EmissionPrimitiveType.NoResult;

    internal static bool TryCreate(IMethodSymbol method, out PrimitiveCallableSignature signature)
    {
        signature = null!;
        if (method.IsGenericMethod || method.IsExtensionMethod || method.IsAsync ||
            !EmissionPrimitiveTypes.TryGetReturnType(method.ReturnType, out var result))
            return false;
        var parameters = System.Collections.Immutable.ImmutableArray.CreateBuilder<EmissionPrimitiveType>(method.Parameters.Length);
        foreach (var parameter in method.Parameters)
        {
            if (!EmissionPrimitiveTypes.TryGetValueType(parameter.Type, out var type) || parameter.RefKind != RefKind.None ||
                parameter.HasExplicitDefaultValue || parameter.IsVarParams)
                return false;
            parameters.Add(type);
        }
        signature = new(result, parameters.MoveToImmutable());
        return true;
    }
}

// The result is a backend-owned handle: callers never cast between CLR MethodInfo
// and native metadata builders. A builder can represent a type or assembly owner.
internal interface ICallableDefinitionBuilder<TMethod>
{
    TMethod DefineMethod(string metadataName, SourceCallablePlan plan);
}
