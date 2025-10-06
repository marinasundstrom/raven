namespace System.Reflection2;

using System.Collections.Immutable;
using System.Reflection;
using System.Reflection.Emit;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;

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
        var entryPointHandle = assemblyBuilder.EntryPoint is MethodInfo entryPoint
            ? MetadataTokens.MethodDefinitionHandle(entryPoint.MetadataToken)
            : default;
        var peBuilder = new ManagedPEBuilder(
            new PEHeaderBuilder(imageCharacteristics: Characteristics.Dll),
            new MetadataRootBuilder(metadataBuilder),
            ilStream: ilStream,
            mappedFieldData: mappedFieldData,
            entryPoint: entryPointHandle);
        var peBlob = new BlobBuilder();
        peBuilder.Serialize(peBlob);
        var peImage = peBlob.ToImmutableArray();
        var peReader = new PEReader(peImage);
        var provider = MetadataReaderProvider.FromMetadataImage(peReader.GetMetadata().GetContent());
        return context.RegisterAssembly(new MetadataResolutionResult(provider, location: null, peReader));
    }
}
