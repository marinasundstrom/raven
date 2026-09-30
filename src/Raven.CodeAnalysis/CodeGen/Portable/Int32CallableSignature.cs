namespace Raven.CodeAnalysis.CodeGen.Portable;

// Common signature shape for the current native subset. Ownership, visibility,
// target type handles and on-disk signature encoding remain backend responsibilities.
internal readonly record struct Int32CallableSignature(int ParameterCount, bool ReturnsValue)
{
    internal static bool TryCreate(IMethodSymbol method, out Int32CallableSignature signature)
    {
        signature = default;
        if (method.IsGenericMethod || method.IsExtensionMethod || method.IsAsync ||
            method.ReturnType.SpecialType is not (SpecialType.System_Int32 or SpecialType.System_Unit or SpecialType.System_Void) ||
            method.Parameters.Any(p => p.Type.SpecialType != SpecialType.System_Int32 || p.RefKind != RefKind.None ||
                p.HasExplicitDefaultValue || p.IsVarParams))
            return false;
        signature = new(method.Parameters.Length, method.ReturnType.SpecialType == SpecialType.System_Int32);
        return true;
    }
}

// The result is a backend-owned handle: callers never cast between CLR MethodInfo
// and native metadata builders. A builder can represent a type or assembly owner.
internal interface ICallableDefinitionBuilder<TMethod>
{
    TMethod DefineMethod(string metadataName, Int32CallableSignature signature);
}
