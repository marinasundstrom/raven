using System.Runtime.CompilerServices;

using NeoCLR.Metadata.Experimental.Introspection;

namespace Raven.CodeAnalysis.NeoClr;

// Lifetime belongs to the immutable compilation; dependency policy and view identity belong to the library.
internal static class NativeMetadataContext
{
    private static readonly ConditionalWeakTable<Compilation, MetadataLoadContext> contexts = new();

    internal static MetadataLoadContext For(Compilation compilation) => contexts.GetValue(compilation,
        static current => new(current.References.OfType<NeoClrMetadataReference>().Select(reference => reference.Definition)));
}
