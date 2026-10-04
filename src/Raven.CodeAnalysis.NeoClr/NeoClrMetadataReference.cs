using System.Runtime.CompilerServices;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.NeoClr;

/// <summary>An owned native metadata input, read directly without a CLI projection.</summary>
/// <remarks>The profile supports unconstrained generic root classes, unconstrained generic interfaces and static generic methods/functions with supported local/external nongeneric interface bounds, with primitive, nominal and vector signatures and explicitly resolved dependencies. An explicit CLI core still supplies primitive symbols.</remarks>
public sealed class NeoClrMetadataReference : MetadataReference, ISemanticMetadataReference
{
    private NeoClrMetadataReference(AssemblyDefinition definition, string sha256, NeoClrPrimitiveBootstrap? bootstrap = null)
    {
        Definition = definition;
        Bootstrap = bootstrap;
        var identity = definition.Identity;
        Artifact = new(identity.Name, identity.Version, identity.Culture, identity.PublicKeyToken, identity.Flags, sha256);
    }
    internal NeoClrPrimitiveBootstrap? Bootstrap { get; }
    internal ResolvedAssemblyArtifact Artifact { get; }
    /// <summary>Gets the immutable native definition snapshot.</summary>
    public AssemblyDefinition Definition { get; }
    /// <summary>Reads an API-produced PE/#Neo declaration library. Unsupported declarations fail before compilation.</summary>
    public static NeoClrMetadataReference ReadAssembly(ReadOnlySpan<byte> image)
    {
        var definition = AssemblyDefinition.ReadNativeAssembly(image);
        return new(definition, Convert.ToHexString(System.Security.Cryptography.SHA256.HashData(image)));
    }
    /// <summary>Reads native metadata with an explicit matching CLI primitive bootstrap.</summary>
    /// <remarks>The bootstrap semantic reference must also be supplied to the compilation. Other dependencies remain native-only.</remarks>
    public static NeoClrMetadataReference ReadAssembly(ReadOnlySpan<byte> image, NeoClrPrimitiveBootstrap bootstrap)
    {
        ArgumentNullException.ThrowIfNull(bootstrap);
        return new(AssemblyDefinition.ReadNativeAssembly(image),
            Convert.ToHexString(System.Security.Cryptography.SHA256.HashData(image)), bootstrap);
    }
    public override bool Equals(object? obj) => ReferenceEquals(this, obj);
    public override bool Equals(MetadataReference? other) => ReferenceEquals(this, other);
    public override int GetHashCode() => RuntimeHelpers.GetHashCode(this);

    string? ISemanticMetadataReference.Validate(Compilation compilation)
    {
        if (compilation.Options.TargetPlatform != TargetPlatform.NeoCLR)
            return "native metadata references require the neoCLR target";
        var supplied = compilation.References.OfType<NeoClrMetadataReference>().ToArray();
        var duplicate = supplied.GroupBy(r => r.Definition.Identity).FirstOrDefault(group => group.Count() != 1);
        if (duplicate is not null)
            return "duplicate native assembly identity: " + duplicate.Key.Name;
        if (Bootstrap is { } bootstrap && (!compilation.References.Contains(bootstrap.Reference) ||
            compilation.Options.TargetCoreAssemblyName != bootstrap.Definition.Name))
            return "primitive bootstrap must match the configured core semantic reference";
        var bootstraps = supplied.Select(r => r.Bootstrap).OfType<NeoClrPrimitiveBootstrap>().ToArray();
        if (bootstraps.Select(b => b.Sha256).Distinct(StringComparer.Ordinal).Count() > 1)
            return "conflicting primitive bootstrap snapshots";
        foreach (var dependency in Definition.MainModule.AssemblyReferences)
            if (Bootstrap?.Definition.Identity.Equals(dependency.Identity) != true && supplied.Count(r => r.Definition.Identity.Equals(dependency.Identity)) != 1)
                return "missing or mismatched native dependency: " + dependency.Identity.Name;
        try
        {
            var metadata = NativeMetadataContext.For(compilation);
            foreach (var reference in Definition.MainModule.TypeReferences) _ = metadata.Resolve(reference);
            _ = new NativeUnionContracts(Definition.MainModule.Types.Select(type => metadata.Resolve(type.ToReference())));
        }
        catch (InvalidDataException error) { return "invalid native metadata contract: " + error.Message; }
        catch (NotSupportedException error) { return "unsupported native metadata contract: " + error.Message; }
        return null;
    }
    IAssemblySymbol ISemanticMetadataReference.CreateAssemblySymbol(Compilation compilation)
        => new NativeAssemblySymbol(compilation, this);
}
