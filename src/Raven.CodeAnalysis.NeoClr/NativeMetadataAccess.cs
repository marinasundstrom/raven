using NeoCLR.Metadata.Experimental.Introspection;

namespace Raven.CodeAnalysis.NeoClr;

internal static class NativeMetadataAccess
{
    // Metadata classification is library-owned; supported Raven access policy stays here.
    internal static Accessibility Map(MetadataAccessibility accessibility) => accessibility switch
    {
        MetadataAccessibility.Public => Accessibility.Public,
        MetadataAccessibility.Assembly => Accessibility.Internal,
        MetadataAccessibility.Private => Accessibility.Private,
        _ => throw new InvalidDataException("unsupported native accessibility: " + accessibility)
    };
}
