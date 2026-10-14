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
    internal static NeoClrMetadataReference ReadDocumentedAssembly(ReadOnlySpan<byte> image, NeoClrPrimitiveBootstrap? bootstrap, string assemblyPath, bool allowExperimentalCompositions = false, IAssemblyResolver? resolver = null)
    {
        var reference = allowExperimentalCompositions
            ? ReadExperimentalCompositionAssembly(image, bootstrap, resolver)
            : new NeoClrMetadataReference(AssemblyDefinition.ReadNativeAssembly(image), Convert.ToHexString(System.Security.Cryptography.SHA256.HashData(image)), bootstrap);
        return new(reference.Definition, Convert.ToHexString(System.Security.Cryptography.SHA256.HashData(image)), bootstrap,
            Raven.CodeAnalysis.Symbols.ExternalDocumentationSet.TryLoad(assemblyPath), reference.HasExperimentalCompositions)
        { HasExperimentalUnions = reference.HasExperimentalUnions };
    }

    internal bool HasExperimentalCompositions { get; }
    internal bool HasExperimentalTuples { get; private init; }
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
    /// <summary>Reads native reference intersections in signatures and method bounds for experimental semantic inspection and opt-in native calls.</summary>
    /// <param name="image">A native declaration image; the reader copies its contents.</param>
    /// <param name="bootstrap">Optional matching CLI primitive bootstrap, whose reference must also be supplied to the compilation.</param>
    /// <returns>An immutable reference admitting intersection free-function signatures.</returns>
    /// <exception cref="InvalidDataException">The image is invalid, exceeds reader limits, or contains a structural union in an exposed signature or method bound.</exception>
    /// <remarks>Default readers remain closed to compositions. This does not enable explicit intersection storage syntax; native calls require the separate emitter opt-in.
    /// Compiler symbols normalize nominal supertypes; the definition retains the original native descriptor.</remarks>
    public static NeoClrMetadataReference ReadExperimentalIntersectionAssembly(ReadOnlySpan<byte> image, NeoClrPrimitiveBootstrap? bootstrap = null)
        => ReadExperimentalIntersectionAssembly(image, bootstrap, null);

    /// <summary>Reads native intersections using explicit snapshots to validate external operands.</summary>
    /// <param name="image">A native declaration image; the reader copies its contents.</param>
    /// <param name="bootstrap">Optional matching CLI primitive bootstrap.</param>
    /// <param name="resolver">Explicit dependency lookup; required for external intersection operands.</param>
    /// <returns>An immutable native reference preserving intersection signatures and method bounds.</returns>
    /// <exception cref="InvalidDataException">Invalid metadata, an exposed structural union, or an absent, mismatched or ineligible intersection dependency.</exception>
    /// <remarks>Nominal interface conformance remains explicit. Native execution requires the separate emitter opt-in.</remarks>
    public static NeoClrMetadataReference ReadExperimentalIntersectionAssembly(ReadOnlySpan<byte> image, NeoClrPrimitiveBootstrap? bootstrap, IAssemblyResolver? resolver)
    {
        var definition = AssemblyDefinition.ReadNativeAssembly(image, allowExperimentalCompositions: true, resolver);
        var hasIntersections = false;
        foreach (var signature in ExperimentalSignatures(definition)) hasIntersections |= Check(signature);
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

    /// <summary>Reads experimental native union and intersection signatures and method bounds.</summary>
    /// <remarks>Requires the separate composition-call emit option for native execution. Default readers and PE projection remain closed.</remarks>
    public static NeoClrMetadataReference ReadExperimentalCompositionAssembly(ReadOnlySpan<byte> image, NeoClrPrimitiveBootstrap? bootstrap = null)
        => ReadExperimentalCompositionAssembly(image, bootstrap, null);

    /// <summary>Reads experimental compositions, resolving external intersection operands from explicit dependency snapshots.</summary>
    /// <param name="image">Owned native image snapshot to read.</param>
    /// <param name="bootstrap">Optional matching CLI primitive bootstrap.</param>
    /// <param name="resolver">Explicit dependency lookup; required for external intersection operands.</param>
    /// <returns>An immutable native semantic reference preserving structural grouping.</returns>
    /// <exception cref="InvalidDataException">Invalid metadata, unsupported composition, or absent/mismatched/ineligible intersection dependency.</exception>
    public static NeoClrMetadataReference ReadExperimentalCompositionAssembly(ReadOnlySpan<byte> image, NeoClrPrimitiveBootstrap? bootstrap, IAssemblyResolver? resolver)
    {
        var definition = AssemblyDefinition.ReadNativeAssembly(image, allowExperimentalCompositions: true, resolver);
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
        foreach (var signature in ExperimentalSignatures(definition)) Inspect(signature);
        return new(definition, Convert.ToHexString(System.Security.Cryptography.SHA256.HashData(image)), bootstrap,
            hasExperimentalCompositions: hasComposition)
        { HasExperimentalUnions = hasUnion };
    }

    internal static bool ContainsTuple(SignatureType type) => type.Tuple is not null ||
        type.ReferencedGenericInstance?.TypeArguments.Any(ContainsTuple) == true ||
        type.ArrayElement is { } element && ContainsTuple(element) ||
        type.ByReferenceElement is { } byref && ContainsTuple(byref) ||
        type.FunctionSignature is { } function && function.ParameterTypes.Append(function.ReturnType).Any(ContainsTuple);

    /// <summary>Reads native structural tuple signatures with names for experimental semantic inspection and native calls.</summary>
    /// <param name="image">Native image copied into an immutable snapshot.</param>
    /// <param name="bootstrap">Matching primitive bootstrap, also registered with the compilation.</param>
    /// <param name="resolver">Explicit dependency resolver for imported nominal elements.</param>
    /// <returns>A tuple-enabled native reference; emission requires AllowExperimentalTuples and MetadataImportOptions.WithStructuralTuples.</returns>
    /// <remarks>Composition admission is independent. This bounded compiler profile supports scalar/nested tuple elements. Select WithStructuralTuples for native binding without a nominal tuple carrier.</remarks>
    public static NeoClrMetadataReference ReadExperimentalTupleAssembly(ReadOnlySpan<byte> image, NeoClrPrimitiveBootstrap? bootstrap = null, IAssemblyResolver? resolver = null)
    {
        var definition = AssemblyDefinition.ReadExperimentalTupleAssembly(image, resolver);
        return new(definition, Convert.ToHexString(System.Security.Cryptography.SHA256.HashData(image)), bootstrap)
        { HasExperimentalTuples = true };
    }

    private static IEnumerable<SignatureType> ExperimentalSignatures(AssemblyDefinition definition)
    {
        foreach (var method in definition.MainModule.Functions.Concat(definition.MainModule.Types.SelectMany(t => t.Methods)))
        {
            if (!method.TryGetSignature(out var signature)) throw new InvalidDataException("unsupported composition signature");
            yield return signature!.ReturnType;
            foreach (var parameter in signature.ParameterTypes) yield return parameter;
            foreach (var constraint in method.TypeConstraints) yield return constraint.Bound;
        }
        foreach (var field in definition.MainModule.Types.SelectMany(t => t.Fields))
        {
            if (!field.TryGetSignature(out var signature)) throw new InvalidDataException("unsupported composition field signature");
            yield return signature!;
        }
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
