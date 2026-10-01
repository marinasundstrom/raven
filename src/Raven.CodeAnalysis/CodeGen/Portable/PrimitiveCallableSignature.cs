namespace Raven.CodeAnalysis.CodeGen.Portable;

// Common signature shape for the current native subset. Ownership, visibility,
// target type handles and on-disk signature encoding remain backend responsibilities.
internal sealed record PrimitiveCallableSignature(SpecialType ReturnType, System.Collections.Immutable.ImmutableArray<SpecialType> ParameterTypes)
{
    internal int ParameterCount => ParameterTypes.Length;
    internal bool ReturnsValue => ReturnType != SpecialType.System_Void;

    internal static bool TryCreate(IMethodSymbol method, out PrimitiveCallableSignature signature)
    {
        signature = null!;
        if (method.IsGenericMethod || method.IsExtensionMethod || method.IsAsync ||
            method.ReturnType.SpecialType is not (SpecialType.System_Int32 or SpecialType.System_Int64 or SpecialType.System_Boolean or SpecialType.System_Unit or SpecialType.System_Void) ||
            method.Parameters.Any(p => p.Type.SpecialType is not (SpecialType.System_Int32 or SpecialType.System_Int64 or SpecialType.System_Boolean) || p.RefKind != RefKind.None ||
                p.HasExplicitDefaultValue || p.IsVarParams))
            return false;
        signature = new(method.ReturnType.SpecialType is SpecialType.System_Unit or SpecialType.System_Void
            ? SpecialType.System_Void : method.ReturnType.SpecialType,
            [.. method.Parameters.Select(p => p.Type.SpecialType)]);
        return true;
    }
}

// The result is a backend-owned handle: callers never cast between CLR MethodInfo
// and native metadata builders. A builder can represent a type or assembly owner.
internal interface ICallableDefinitionBuilder<TMethod>
{
    TMethod DefineMethod(string metadataName, PrimitiveCallableSignature signature);
}
