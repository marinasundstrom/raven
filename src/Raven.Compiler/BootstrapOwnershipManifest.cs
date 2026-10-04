using System.Text.Json;
using System.Text.Json.Serialization;

using Raven.CodeAnalysis;

namespace Raven;

// Host configuration only: it selects semantic contracts and checks ownership, not metadata representation.
internal sealed record BootstrapSourceLibrary(string AssemblyName, string[] Sources, string[] Types);
internal sealed record BootstrapOwnershipManifest(int Version, BootstrapSourceLibrary[] Libraries, RuntimeIterationContract Iteration, RuntimeTypeOfContract? TypeOf = null, RuntimePropagationContract? Propagation = null, RuntimeUnitContract? Unit = null, RuntimeSelfTypeContract? Self = null)
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
        return manifest;
    }

    internal CompilationOptions Apply(CompilationOptions options)
    {
        var configured = options.WithRuntimeIterationContract(Iteration)
            .WithRuntimeTypeOfContract(TypeOf).WithRuntimePropagationContract(Propagation)
            .WithRuntimeSelfTypeContract(Self);
        return Unit is null ? configured : configured.WithRuntimeUnitContract(Unit);
    }

    internal void Validate(Compilation compilation)
    {
        // Force normal symbol setup before enumerating the selected input catalog.
        _ = compilation.GetSpecialType(SpecialType.System_Object);
        var assemblies = compilation.ReferencedAssemblySymbols.Prepend(compilation.Assembly).ToArray();
        if (Self is not null && compilation.ResolveRuntimeSelfType() is null)
            throw new InvalidDataException("Bootstrap Self contract requires its exact configured marker identity.");
        foreach (var library in Libraries)
            foreach (var name in library.Types)
            {
                _ = compilation.GetTypeByMetadataName(name);
                var matches = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);
                foreach (var assembly in assemblies)
                    if (assembly.GetTypeByMetadataName(name) is { } type) matches.Add(type);
                if (matches.Count != 1 || matches.Single().ContainingAssembly?.Name != library.AssemblyName)
                    throw new InvalidDataException($"Bootstrap ownership for '{name}' requires exactly one declaration in '{library.AssemblyName}'; found " +
                        (matches.Count == 0 ? "none." : string.Join(", ", matches.Select(t => t.ContainingAssembly?.Name).Order()) + "."));
            }
    }
}
