using System.Reflection;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Targets;

namespace Raven.CodeAnalysis.NeoClr;

// Validate explicit bootstrap and native primitive providers before emission.
internal static class NeoClrBindingContract
{
    internal static bool MatchesCore(IAssemblySymbol assembly, AssemblyIdentity core)
    {
        if (assembly is NativeAssemblySymbol native) return native.Reference.Definition.Identity.Equals(core);
        if (assembly is not PEAssemblySymbol imported) return false;
        var identity = new AssemblyName(imported.FullName);
        return core.Equals(new AssemblyIdentity(identity.Name!, identity.Version!, identity.CultureName ?? "",
            Convert.ToHexString(identity.GetPublicKeyToken() ?? [])));
    }

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

        if (compilation.Options.MetadataImportOptions is { } imports)
            foreach (var (special, provider) in imports.PrimitiveAssemblies)
            {
                var primitive = compilation.GetSpecialType(special);
                if (primitive.SpecialType != special || primitive.ContainingAssembly?.Name != provider ||
                    primitive.ContainingAssembly is not IImportedAssemblySymbol { ResolvedArtifact: not null })
                    return "native primitive does not match its selected provider";
            }

        IAssemblySymbol? core = null;
        foreach (var special in new[] { SpecialType.System_Object, SpecialType.System_Int32,
                     SpecialType.System_Int64, SpecialType.System_Boolean, SpecialType.System_String,
                     SpecialType.System_Unit })
        {
            if (special == SpecialType.System_Unit && compilation.Options.RuntimeUnitContract is { } unit &&
                unit.AssemblyName != options.CoreLibrary.Name)
            {
                var representation = compilation.ResolveRuntimeUnitType();
                if (representation?.ContainingAssembly?.Name != unit.AssemblyName ||
                    !SymbolEqualityComparer.Default.Equals(representation.ContainingAssembly, compilation.Assembly) &&
                    representation.ContainingAssembly is not IImportedAssemblySymbol { ResolvedArtifact: not null })
                    return "native unit does not match its selected source or artifact owner";
                continue;
            }
            var type = compilation.GetSpecialType(special);
            if (special == SpecialType.System_Object && compilation.UsesSourceObjectRoot && compilation.IsSourceObjectRoot((INamedTypeSymbol)type)) continue;
            if (special == SpecialType.System_Object && compilation.Options.MetadataImportOptions?.ObjectAssemblyName is { } objectProvider)
            {
                if (type.SpecialType != SpecialType.System_Object || type.ContainingAssembly?.Name != objectProvider ||
                    type.ContainingAssembly is not IImportedAssemblySymbol { ResolvedArtifact: not null })
                    return "native Object does not match its selected artifact owner";
                continue;
            }
            if (compilation.Options.MetadataImportOptions?.PrimitiveAssemblies.TryGetValue(special, out var provider) == true)
            {
                if (type.SpecialType != special || type.ContainingAssembly?.Name != provider ||
                    type.ContainingAssembly is not IImportedAssemblySymbol { ResolvedArtifact: not null })
                    return "native primitive does not match its selected provider";
                continue;
            }
            if (compilation.Options.MetadataImportOptions?.UseNativeMetadata == true)
            {
                if (type.TypeKind == TypeKind.Error || type.ContainingAssembly is not NativeAssemblySymbol native ||
                    !native.Reference.Definition.Identity.Equals(options.CoreLibrary))
                    return "native primitive or Unit declaration must belong to the selected native core artifact";
                continue;
            }
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
