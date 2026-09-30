namespace Raven.CodeAnalysis;

public partial class Compilation
{
    internal bool HasNativeSelfContract => Options.TargetPlatform == TargetPlatform.NeoCLR
        && Options.RuntimeSelfTypeContract is not null;

    internal ITypeSymbol SelfImplementingType(ITypeSymbol type)
    {
        // Runtime-library source shadows use the configured core's canonical primitive identity.
        if (Options.TargetCoreAssemblyName is not null && type.ContainingNamespace?.ToDisplayString() == "System")
        {
            var special = type.Name switch
            {
                "SByte" => SpecialType.System_SByte,
                "Byte" => SpecialType.System_Byte,
                "Int16" => SpecialType.System_Int16,
                "UInt16" => SpecialType.System_UInt16,
                "Int32" => SpecialType.System_Int32,
                "UInt32" => SpecialType.System_UInt32,
                "Int64" => SpecialType.System_Int64,
                "UInt64" => SpecialType.System_UInt64,
                "Single" => SpecialType.System_Single,
                "Double" => SpecialType.System_Double,
                _ => SpecialType.None
            };
            if (special != SpecialType.None)
                return GetSpecialType(special);
        }
        return type;
    }

    internal INamedTypeSymbol? ResolveRuntimeSelfType()
    {
        if (!HasNativeSelfContract || Options.RuntimeSelfTypeContract is not { } contract)
            return null;
        var type = Assembly.Name == contract.AssemblyName
            ? Assembly.GetTypeByMetadataName(contract.TypeName)
            : GetTypeByMetadataName(contract.TypeName, contract.AssemblyName);
        return type?.ContainingAssembly?.Name == contract.AssemblyName ? type : null;
    }
}
