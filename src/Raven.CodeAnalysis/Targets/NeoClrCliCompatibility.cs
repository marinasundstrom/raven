using System;

namespace Raven.CodeAnalysis.Targets;

// Transitional rules for the experimental CLI transport. Explicit neoCLR selection
// coexists with legacy assembly-name triggers until controlled callers migrate.
internal static partial class NeoClrCliCompatibility
{
    private const string CoreAssemblyName = NeoClrCliProfile.CoreAssemblyName;

    internal static bool UsesLegacyContract(CompilationOptions options) =>
        options.TargetCoreAssemblyName == CoreAssemblyName;

    internal static string? GetSpecialTypeMetadataName(string? assemblyName, bool isValueType, string? fullName)
    {
        if (assemblyName == CoreAssemblyName && isValueType &&
            fullName?.StartsWith("System.Tuple`", StringComparison.Ordinal) == true)
        {
            return fullName.Replace("System.Tuple`", "System.ValueTuple`", StringComparison.Ordinal);
        }

        return fullName;
    }

}
