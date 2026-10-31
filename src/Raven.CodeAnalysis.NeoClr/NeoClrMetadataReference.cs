using System.Runtime.CompilerServices;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.NeoClr;

/// <summary>An owned native metadata input, read directly without a CLI projection.</summary>
/// <remarks>The profile supports unconstrained generic root classes, unconstrained generic interfaces and static generic methods/functions with supported local/external nongeneric interface bounds, with primitive, nominal and vector signatures and explicitly resolved dependencies. An explicit CLI bootstrap or native semantic core supplies primitive symbols.</remarks>
public sealed class NeoClrMetadataReference : MetadataReference, ISemanticMetadataReference
{
    private NeoClrMetadataReference(AssemblyDefinition definition, string sha256, NeoClrPrimitiveBootstrap? bootstrap = null, Raven.CodeAnalysis.Symbols.ExternalDocumentationSet? documentation = null)
    {
        Documentation = documentation;
        Definition = definition;
        Bootstrap = bootstrap;
        var signatures = StructuralSignatures(definition).ToArray();
        HasStructuralCompositions = signatures.Any(ContainsComposition);
        HasStructuralTuples = signatures.Any(ContainsTuple);
        var identity = definition.Identity;
        Artifact = new(identity.Name, identity.Version, identity.Culture, identity.PublicKeyToken, identity.Flags, sha256);
    }
    internal Raven.CodeAnalysis.Symbols.ExternalDocumentationSet? Documentation { get; }
    internal static NeoClrMetadataReference ReadDocumentedAssembly(ReadOnlySpan<byte> image, NeoClrPrimitiveBootstrap? bootstrap, string assemblyPath, IAssemblyResolver? resolver = null)
        => new(AssemblyDefinition.ReadNativeAssembly(image, resolver),
            Convert.ToHexString(System.Security.Cryptography.SHA256.HashData(image)), bootstrap,
            Raven.CodeAnalysis.Symbols.ExternalDocumentationSet.TryLoad(assemblyPath));

    internal bool HasStructuralCompositions { get; }
    internal bool HasStructuralTuples { get; }
    internal NeoClrPrimitiveBootstrap? Bootstrap { get; }
    internal ResolvedAssemblyArtifact Artifact { get; }
    /// <summary>Gets the immutable native definition snapshot.</summary>
    public AssemblyDefinition Definition { get; }
    /// <summary>Reads native nominal and structural signatures without a CLI projection or feature opt-in.</summary>
    /// <param name="image">Complete PE/#Neo or standalone NEOX image, copied by the reader.</param>
    /// <param name="bootstrap">Optional matching CLI primitive bootstrap, also supplied to compilation.</param>
    /// <param name="resolver">Explicit dependency snapshots for external structural bounds; no implicit loading.</param>
    /// <returns>An immutable native reference preserving tuple names and structural grouping.</returns>
    /// <exception cref="InvalidDataException">Malformed metadata, unsupported shapes or missing/ineligible structural dependencies.</exception>
    public static NeoClrMetadataReference ReadAssembly(ReadOnlySpan<byte> image, NeoClrPrimitiveBootstrap? bootstrap = null, IAssemblyResolver? resolver = null)
        => new(AssemblyDefinition.ReadNativeAssembly(image, resolver),
            Convert.ToHexString(System.Security.Cryptography.SHA256.HashData(image)), bootstrap);

    internal static bool ContainsComposition(SignatureType type) => type.Composition is not null ||
        type.FunctionReturnOperand is { } operand && ContainsComposition(operand) ||
        type.Tuple?.ElementTypes.Any(ContainsComposition) == true ||
        type.ReferencedGenericInstance?.TypeArguments.Any(ContainsComposition) == true ||
        type.ArrayElement is { } element && ContainsComposition(element) ||
        type.ByReferenceElement is { } byref && ContainsComposition(byref) ||
        type.FunctionSignature is { } function && function.ParameterTypes.Append(function.ReturnType).Any(ContainsComposition);

    internal static bool ContainsTuple(SignatureType type) => type.Tuple is not null ||
        type.FunctionReturnOperand is { } operand && ContainsTuple(operand) ||
        type.Composition?.Constituents.Any(ContainsTuple) == true ||
        type.ReferencedGenericInstance?.TypeArguments.Any(ContainsTuple) == true ||
        type.ArrayElement is { } element && ContainsTuple(element) ||
        type.ByReferenceElement is { } byref && ContainsTuple(byref) ||
        type.FunctionSignature is { } function && function.ParameterTypes.Append(function.ReturnType).Any(ContainsTuple);

    private static IEnumerable<SignatureType> StructuralSignatures(AssemblyDefinition definition)
    {
        foreach (var method in definition.MainModule.Functions.Concat(definition.MainModule.Types.SelectMany(t => t.Methods)))
        {
            if (!method.TryGetSignature(out var signature)) throw new InvalidDataException("unsupported composition signature");
            yield return signature!.FunctionType;
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
