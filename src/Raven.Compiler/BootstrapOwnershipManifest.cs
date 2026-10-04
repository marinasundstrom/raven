using System.Text.Json;
using System.Text.Json.Serialization;

using Raven.CodeAnalysis;

namespace Raven;

// Host configuration only: it selects semantic contracts and checks ownership, not metadata representation.
internal sealed record BootstrapSourceLibrary(string AssemblyName, string[] Sources, string[] Types);
internal sealed record BootstrapOwnershipManifest(int Version, BootstrapSourceLibrary[] Libraries, RuntimeIterationContract Iteration, RuntimeTypeOfContract? TypeOf = null, RuntimePropagationContract? Propagation = null, RuntimeUnitContract? Unit = null, RuntimeSelfTypeContract? Self = null, Dictionary<string, string>? NativePrimitives = null)
{
    internal static BootstrapOwnershipManifest Read(string path)
    {
        if (new FileInfo(path).Length > 1024 * 1024) throw new InvalidDataException("Bootstrap ownership manifest exceeds 1 MiB.");
        BootstrapOwnershipManifest manifest;
        try
        {
            manifest = JsonSerializer.Deserialize<BootstrapOwnershipManifest>(File.ReadAllText(path), new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true,
                UnmappedMemberHandling = JsonUnmappedMemberHandling.Disallow
            }) ?? throw new InvalidDataException("Empty bootstrap ownership manifest.");
        }
        catch (JsonException error) { throw new InvalidDataException("Malformed bootstrap ownership manifest: " + error.Message, error); }
        if (manifest.Version != 1 || manifest.Libraries is not { Length: > 0 and <= 32 } || manifest.Iteration is null)
            throw new InvalidDataException("Unsupported bootstrap ownership manifest version or library catalog.");
        var types = new Dictionary<string, string>(StringComparer.Ordinal);
        var names = new HashSet<string>(StringComparer.Ordinal);
        foreach (var library in manifest.Libraries)
        {
            if (library is null || string.IsNullOrWhiteSpace(library.AssemblyName) || !names.Add(library.AssemblyName) ||
                library.Sources is not { Length: > 0 and <= 256 } || library.Sources.Any(string.IsNullOrWhiteSpace) ||
                library.Types is not { Length: > 0 and <= 4096 })
                throw new InvalidDataException("Invalid bootstrap source-library declaration.");
            foreach (var type in library.Types)
                if (string.IsNullOrWhiteSpace(type) || !types.TryAdd(type, library.AssemblyName))
                    throw new InvalidDataException("Duplicate or invalid bootstrap type owner: " + type);
        }
        if (manifest.Iteration.IterableTypeName is null || manifest.Iteration.IteratorTypeName is null ||
            !types.TryGetValue(manifest.Iteration.IterableTypeName, out var iterableOwner) ||
            !types.TryGetValue(manifest.Iteration.IteratorTypeName, out var iteratorOwner) ||
            iterableOwner != manifest.Iteration.AssemblyName || iteratorOwner != manifest.Iteration.AssemblyName)
            throw new InvalidDataException("Iteration contracts must belong to their declared source library.");
        foreach (var (name, owner) in manifest.NativePrimitives ?? [])
            if (!types.TryGetValue(name, out var declaredOwner) || declaredOwner != owner || !NativePrimitiveSpecialType(name, out _))
                throw new InvalidDataException("Native primitive must name its declared source-library owner: " + name);
        return manifest;
    }

    internal CompilationOptions Apply(CompilationOptions options, string? outputAssemblyName = null, string? primitiveCoreAssemblyName = null)
    {
        var configured = options.WithRuntimeIterationContract(Iteration)
            .WithRuntimeTypeOfContract(TypeOf).WithRuntimePropagationContract(Propagation)
            .WithRuntimeSelfTypeContract(Self);
        if (NativePrimitives is { Count: > 0 })
        {
            if (options.TargetPlatform != TargetPlatform.NeoCLR || primitiveCoreAssemblyName is null || outputAssemblyName is null)
                throw new InvalidDataException("Native primitive providers require an explicit NeoCLR core and output identity.");
            // Source implementations bind primitive spellings through the declared bootstrap.
            // Consumers select the completed native declaration, with no fallback on failure.
            var providers = NativePrimitives.Where(p => p.Value != outputAssemblyName).ToDictionary(p =>
            {
                NativePrimitiveSpecialType(p.Key, out var special); return special;
            }, p => p.Value);
            configured = configured.WithMetadataImportOptions(new MetadataImportOptions(primitiveCoreAssemblyName, providers));
        }
        return Unit is null ? configured : configured.WithRuntimeUnitContract(Unit);
    }

    internal static bool NativePrimitiveSpecialType(string name, out SpecialType special)
    {
        special = SpecialType.None;
        return name.StartsWith("System.", StringComparison.Ordinal) && Enum.TryParse(name.Replace('.', '_'), out special) &&
            special is SpecialType.System_SByte or SpecialType.System_Byte or SpecialType.System_Int16 or SpecialType.System_UInt16 or
                SpecialType.System_Int32 or SpecialType.System_UInt32 or SpecialType.System_Int64 or SpecialType.System_UInt64 or
                SpecialType.System_Single or SpecialType.System_Double or SpecialType.System_String;
    }

    internal void Validate(Compilation compilation)
    {
        // Force normal symbol setup before enumerating the selected input catalog.
        _ = compilation.GetSpecialType(SpecialType.System_Object);
        var assemblies = compilation.ReferencedAssemblySymbols.Prepend(compilation.Assembly).ToArray();
        if (Self is not null && compilation.ResolveRuntimeSelfType() is null)
            throw new InvalidDataException("Bootstrap Self contract requires its exact configured marker identity.");
        foreach (var (name, owner) in NativePrimitives ?? [])
        {
            NativePrimitiveSpecialType(name, out var special);
            if (owner != compilation.Assembly.Name && compilation.GetSpecialType(special) is var selected &&
                (selected.TypeKind == TypeKind.Error || selected.SpecialType != special || selected.ContainingAssembly?.Name != owner))
                throw new InvalidDataException("Missing or incompatible native primitive provider: " + name + " in " + owner);
        }
        foreach (var library in Libraries)
            foreach (var name in library.Types)
            {
                _ = compilation.GetTypeByMetadataName(name);
                var matches = new HashSet<INamedTypeSymbol>(NativePrimitives?.ContainsKey(name) == true
                    ? ReferenceEqualityComparer.Instance : SymbolEqualityComparer.Default);
                foreach (var assembly in assemblies)
                {
                    if (assembly.GetTypeByMetadataName(name) is not { } type) continue;
                    if (NativePrimitives?.ContainsKey(name) == true && type.ContainingAssembly?.Name == compilation.Options.MetadataImportOptions?.CoreAssemblyName) continue;
                    matches.Add(type);
                }
                if (matches.Count != 1 || matches.Single().ContainingAssembly?.Name != library.AssemblyName)
                    throw new InvalidDataException($"Bootstrap ownership for '{name}' requires exactly one declaration in '{library.AssemblyName}'; found " +
                        (matches.Count == 0 ? "none." : string.Join(", ", matches.Select(t => t.ContainingAssembly?.Name).Order()) + "."));
            }
    }
}
