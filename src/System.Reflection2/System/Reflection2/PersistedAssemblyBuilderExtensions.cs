namespace System.Reflection2;

using System.Collections.Immutable;
using System.Reflection.Emit;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;

/// <summary>
/// Extensions that bridge <see cref="PersistedAssemblyBuilder"/> into the metadata-based reflection model.
/// </summary>
public static class PersistedAssemblyBuilderExtensions
{
    /// <summary>
    /// Materializes the metadata produced by <paramref name="assemblyBuilder"/> into a <see cref="MetadataAssembly"/>.
    /// </summary>
    /// <param name="assemblyBuilder">The persisted assembly builder.</param>
    /// <param name="context">The metadata load context used to resolve references.</param>
    /// <returns>A <see cref="MetadataAssembly"/> representing the assembly being built.</returns>
    public static MetadataAssembly ToMetadataAssembly(this PersistedAssemblyBuilder assemblyBuilder, MetadataLoadContext context)
    {
        if (assemblyBuilder is null)
        {
            throw new ArgumentNullException(nameof(assemblyBuilder));
        }

        if (context is null)
        {
            throw new ArgumentNullException(nameof(context));
        }

        var ilStream = new BlobBuilder();
        var mappedFieldData = new BlobBuilder();
        var metadataBuilder = assemblyBuilder.GenerateMetadata(out ilStream, out mappedFieldData);
        var metadataRootBuilder = new MetadataRootBuilder(metadataBuilder);
        var metadataBlob = new BlobBuilder();
        metadataRootBuilder.Serialize(metadataBlob, methodBodyStreamRva: 0, mappedFieldDataStreamRva: 0);
        var metadataBytes = metadataBlob.ToImmutableArray();
        var provider = MetadataReaderProvider.FromMetadataImage(metadataBytes);
        return context.RegisterAssembly(new MetadataResolutionResult(provider));
    }
}
