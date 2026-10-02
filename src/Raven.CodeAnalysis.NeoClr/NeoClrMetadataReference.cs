using System.Runtime.CompilerServices;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.NeoClr;

/// <summary>An owned native metadata input, read directly without a CLI projection.</summary>
/// <remarks>The first profile supports primitive namespace functions and nongeneric top-level classes with primitive/nominal fields and methods with explicitly resolved dependencies. An explicit CLI core still supplies primitive symbols.</remarks>
public sealed class NeoClrMetadataReference : MetadataReference, ISemanticMetadataReference
{
    private NeoClrMetadataReference(AssemblyDefinition definition) => Definition = definition;
    /// <summary>Gets the immutable native definition snapshot.</summary>
    public AssemblyDefinition Definition { get; }
    /// <summary>Reads an API-produced PE/#Neo declaration library. Unsupported declarations fail before compilation.</summary>
    public static NeoClrMetadataReference ReadAssembly(ReadOnlySpan<byte> image)
        => new(AssemblyDefinition.ReadNativeAssembly(image));
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
        foreach (var dependency in Definition.MainModule.AssemblyReferences)
            if (supplied.Count(r => r.Definition.Identity.Equals(dependency.Identity)) != 1)
                return "missing or mismatched native dependency: " + dependency.Identity.Name;
        var resolver = new NativeAssemblyResolver(supplied.Select(r => r.Definition));
        try
        {
            foreach (var reference in Definition.MainModule.TypeReferences) _ = reference.Resolve(resolver);
        }
        catch (InvalidDataException error) { return "invalid native signature dependency: " + error.Message; }
        return null;
    }
    IAssemblySymbol ISemanticMetadataReference.CreateAssemblySymbol(Compilation compilation)
        => new NativeAssemblySymbol(compilation, this);
}
