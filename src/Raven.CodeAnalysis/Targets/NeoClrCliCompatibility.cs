using System;

namespace Raven.CodeAnalysis.Targets;

// Transitional rules for the experimental CLI transport. Explicit neoCLR selection
// coexists with legacy assembly-name triggers until controlled callers migrate.
internal static class NeoClrCliCompatibility
{
    private const string CoreAssemblyName = NeoClrCliProfile.CoreAssemblyName;

    internal static bool UsesInhabitedDelegateResults(CompilationOptions options) =>
        options.TargetPlatform == TargetPlatform.NeoCLR || options.TargetCoreAssemblyName == CoreAssemblyName;

    internal static string GetTupleTypeName(CompilationOptions options) =>
        options.TargetPlatform == TargetPlatform.NeoCLR || options.TargetCoreAssemblyName == CoreAssemblyName
            ? "System.Tuple" : "System.ValueTuple";

    internal static string? GetSpecialTypeMetadataName(string? assemblyName, bool isValueType, string? fullName)
    {
        if (assemblyName == CoreAssemblyName && isValueType &&
            fullName?.StartsWith("System.Tuple`", StringComparison.Ordinal) == true)
        {
            return fullName.Replace("System.Tuple`", "System.ValueTuple`", StringComparison.Ordinal);
        }

        return fullName;
    }

    // Identify the namespace function by its runtime assembly and namespace-member
    // contract, never by container spelling. This historically depends on the
    // imported method's assembly, independently of the configured target core.
    internal static bool IsTerminalRuntimeFault(IMethodSymbol method)
    {
        if (method.Name != "Fault" || !method.IsStatic || method.IsGenericMethod ||
            method.ContainingAssembly?.Name != CoreAssemblyName ||
            method.ContainingNamespace?.ToMetadataName() != "System" ||
            method.Parameters.Length != 1 ||
            method.Parameters[0].RefKind != RefKind.None ||
            method.Parameters[0].Type.SpecialType != SpecialType.System_String ||
            method.ReturnType.SpecialType is not (SpecialType.System_Void or SpecialType.System_Unit))
        {
            return false;
        }

        return method.ContainingType is Symbols.SynthesizedNamespaceMembersClassSymbol ||
            method.ContainingType is Symbols.PENamedTypeSymbol peType &&
            peType.HasCustomAttribute(static name => name == "System.Runtime.CompilerServices.TopLevelAttribute");
    }
}
