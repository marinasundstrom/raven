using System.Runtime.CompilerServices;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.NeoClr;

/// <summary>An owned native metadata input, read directly without a CLI projection.</summary>
/// <remarks>The profile supports unconstrained generic root classes, unconstrained generic interfaces and static generic methods/functions with supported local/external nongeneric interface bounds, with primitive, nominal and vector signatures and explicitly resolved dependencies. An explicit CLI bootstrap or native semantic core supplies primitive symbols.</remarks>
public sealed class NeoClrMetadataReference : MetadataReference, ISemanticMetadataReference
{
    private NeoClrMetadataReference(AssemblyDefinition definition, string sha256, NeoClrPrimitiveBootstrap? bootstrap = null, Raven.CodeAnalysis.Symbols.ExternalDocumentationSet? documentation = null, bool hasExperimentalCompositions = false)
    {
        HasExperimentalCompositions = hasExperimentalCompositions;
        Documentation = documentation;
        Definition = definition;
        Bootstrap = bootstrap;
        var identity = definition.Identity;
        Artifact = new(identity.Name, identity.Version, identity.Culture, identity.PublicKeyToken, identity.Flags, sha256);
    }
    internal Raven.CodeAnalysis.Symbols.ExternalDocumentationSet? Documentation { get; }
    internal static NeoClrMetadataReference ReadDocumentedAssembly(ReadOnlySpan<byte> image, NeoClrPrimitiveBootstrap? bootstrap, string assemblyPath)
        => new(AssemblyDefinition.ReadNativeAssembly(image), Convert.ToHexString(System.Security.Cryptography.SHA256.HashData(image)), bootstrap, Raven.CodeAnalysis.Symbols.ExternalDocumentationSet.TryLoad(assemblyPath));

    internal bool HasExperimentalCompositions { get; }
    internal bool HasExperimentalUnions { get; private init; }
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
    /// <summary>Reads native reference intersections for experimental semantic inspection and opt-in native calls.</summary>
    /// <param name="image">A native declaration image; the reader copies its contents.</param>
    /// <param name="bootstrap">Optional matching CLI primitive bootstrap, whose reference must also be supplied to the compilation.</param>
    /// <returns>An immutable reference admitting intersection free-function signatures.</returns>
    /// <exception cref="InvalidDataException">The image is invalid, exceeds reader limits, or contains a structural union in an exposed callable signature.</exception>
    /// <remarks>Default readers remain closed to compositions. This does not enable explicit intersection storage syntax; native calls require the separate emitter opt-in.
    /// Compiler symbols normalize nominal supertypes; the definition retains the original native descriptor.</remarks>
    public static NeoClrMetadataReference ReadExperimentalIntersectionAssembly(ReadOnlySpan<byte> image, NeoClrPrimitiveBootstrap? bootstrap = null)
    {
        var definition = AssemblyDefinition.ReadNativeAssembly(image, allowExperimentalCompositions: true);
        var hasIntersections = false;
        foreach (var method in definition.MainModule.Functions)
        {
            if (!method.TryGetSignature(out var signature))
                throw new InvalidDataException("unsupported experimental native callable signature");
            hasIntersections |= Check(signature!.ReturnType);
            foreach (var parameter in signature.ParameterTypes) hasIntersections |= Check(parameter);
        }
        return new(definition, Convert.ToHexString(System.Security.Cryptography.SHA256.HashData(image)), bootstrap, hasExperimentalCompositions: hasIntersections);

        static bool Check(SignatureType type)
        {
            if (type.ReferencedGenericInstance is { } wrapper)
            {
                var found = false;
                foreach (var argument in wrapper.TypeArguments) found |= Check(argument);
                return found;
            }
            if (type.Composition is not { } composition) return false;
            if (composition.Kind != ReferenceCompositionKind.Intersection)
                throw new InvalidDataException("structural union import is not supported by the experimental intersection profile");
            foreach (var bound in composition.Constituents) Check(bound);
            return true;
        }
    }

    internal static bool ContainsComposition(SignatureType type) => type.Composition is not null ||
        type.ReferencedGenericInstance?.TypeArguments.Any(ContainsComposition) == true;

    /// <summary>Reads experimental native union and intersection free-function signatures.</summary>
    /// <remarks>Requires the separate composition-call emit option for native execution. Default readers and PE projection remain closed.</remarks>
    public static NeoClrMetadataReference ReadExperimentalCompositionAssembly(ReadOnlySpan<byte> image, NeoClrPrimitiveBootstrap? bootstrap = null)
    {
        var definition = AssemblyDefinition.ReadNativeAssembly(image, allowExperimentalCompositions: true);
        bool hasComposition = false, hasUnion = false;
        void Inspect(SignatureType type)
        {
            if (type.ReferencedGenericInstance is { } wrapper)
                foreach (var argument in wrapper.TypeArguments) Inspect(argument);
            if (type.Composition is not { } composition) return;
            if (composition.Constituents.Any(bound => bound.Composition is not null))
                throw new InvalidDataException("the composition-call profile requires flat nominal bounds");
            hasComposition = true;
            hasUnion |= composition.Kind == ReferenceCompositionKind.Union;
            foreach (var bound in composition.Constituents) Inspect(bound);
        }
        foreach (var method in definition.MainModule.Functions)
        {
            if (!method.TryGetSignature(out var signature)) throw new InvalidDataException("unsupported composition signature");
            Inspect(signature!.ReturnType);
            foreach (var parameter in signature.ParameterTypes) Inspect(parameter);
        }
        return new(definition, Convert.ToHexString(System.Security.Cryptography.SHA256.HashData(image)), bootstrap,
            hasExperimentalCompositions: hasComposition) { HasExperimentalUnions = hasUnion };
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
            NativeAttributeValidation.Validate(metadata, Definition.Identity);
        }
        catch (Exception error) when (error is InvalidDataException or NotSupportedException) { return "invalid native metadata contract: " + error.Message; }
        catch (NotSupportedException error) { return "unsupported native metadata contract: " + error.Message; }
        return null;
    }
    IAssemblySymbol ISemanticMetadataReference.CreateAssemblySymbol(Compilation compilation)
        => new NativeAssemblySymbol(compilation, this);
}
