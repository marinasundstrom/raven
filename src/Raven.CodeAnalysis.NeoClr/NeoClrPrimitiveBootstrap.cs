using NeoCLR.Metadata.Experimental.Model;

namespace Raven.CodeAnalysis.NeoClr;

/// <summary>An explicit CLI core snapshot shared by primitive symbol loading and native signature resolution.</summary>
/// <remarks>This bootstrap is never a fallback for an application or rebuilt native library.</remarks>
public sealed class NeoClrPrimitiveBootstrap
{
    private NeoClrPrimitiveBootstrap(byte[] image)
    {
        Definition = AssemblyDefinition.ReadAssembly(image, expectedExtended: false);
        Reference = MetadataReference.CreateFromImage(image);
        Sha256 = Convert.ToHexString(System.Security.Cryptography.SHA256.HashData(image));
    }
    internal AssemblyDefinition Definition { get; }
    internal string Sha256 { get; }
    /// <summary>Gets the matching semantic reference. Include this exact reference in the compilation.</summary>
    public PortableExecutableReference Reference { get; }
    /// <summary>Copies and validates an ordinary CLI core image without loading its code.</summary>
    public static NeoClrPrimitiveBootstrap ReadAssembly(ReadOnlySpan<byte> image) => new(image.ToArray());
}
