using System.Runtime.CompilerServices;
using NeoCLR.Metadata.Experimental.Model;
using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.NeoClr;

/// <summary>An owned native metadata input, read directly without a CLI projection.</summary>
/// <remarks>The first profile supports primitive namespace functions and fieldless nongeneric top-level static classes. An explicit CLI core still supplies primitive symbols.</remarks>
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
        if (supplied.Count(r => r.Definition.Identity.Equals(Definition.Identity)) != 1)
            return "duplicate native assembly identity: " + Definition.Name;
        foreach (var dependency in Definition.MainModule.AssemblyReferences)
            if (supplied.Count(r => r.Definition.Identity.Equals(dependency.Identity)) != 1)
                return "missing or mismatched native dependency: " + dependency.Identity.Name;
        return null;
    }
    IAssemblySymbol ISemanticMetadataReference.CreateAssemblySymbol(Compilation compilation)
        => new NativeAssemblySymbol(compilation, this);
}
