using System.Reflection;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Targets;

namespace Raven.CodeAnalysis.NeoClr;

// Bind through the existing CLI symbol importer, but validate the selected core before
// translating primitive identities to native signatures. This is not a native importer.
internal static class NeoClrBindingContract
{
    internal static string? GetError(Compilation compilation, NeoClrEmitOptions options)
    {
        if (compilation.Options.TargetPlatform == TargetPlatform.DotNet)
            return null; // Existing explicit host-core bootstrap remains supported.
        if (compilation.Options.TargetPlatform != TargetPlatform.NeoCLR)
            return "unsupported binding target";
        if (NeoClrCliProfile.GetConfigurationError(compilation.Options) is { } error)
            return error;
        if (options.CoreLibrary.Name != compilation.Options.TargetCoreAssemblyName)
            return "native core identity must match the selected binding core";

        IAssemblySymbol? core = null;
        foreach (var special in new[] { SpecialType.System_Object, SpecialType.System_Int32,
                     SpecialType.System_Int64, SpecialType.System_Boolean, SpecialType.System_String,
                     SpecialType.System_Unit })
        {
            var type = compilation.GetSpecialType(special);
            if (type.TypeKind == TypeKind.Error || type.ContainingAssembly is not PEAssemblySymbol imported)
                return "native binding requires imported primitive and Unit declarations";
            var identity = new AssemblyName(imported.FullName);
            var nativeIdentity = new AssemblyIdentity(identity.Name!, identity.Version!, identity.CultureName ?? "",
                Convert.ToHexString(identity.GetPublicKeyToken() ?? []));
            if (!options.CoreLibrary.Equals(nativeIdentity))
                return "primitive or Unit declaration does not match the native core identity";
            if (core is not null && !SymbolEqualityComparer.Default.Equals(core, type.ContainingAssembly))
                return "primitive declarations must come from one binding core";
            core = type.ContainingAssembly;
        }
        return null;
    }
}
