using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;

using Raven.CodeAnalysis.Targets;

namespace Raven.CodeAnalysis.Metadata;

internal sealed partial class DotNetSemanticDataLoader
{
    // The .NET loader validates a previous session against its own input snapshot.
    // Callers offer a candidate; they cannot assert that metadata is reusable.
    internal static DotNetMetadataSession OpenSession(
        IEnumerable<MetadataReference> metadataReferences,
        MetadataImportOptions? importOptions,
        DotNetHostRuntime hostRuntime,
        DotNetMetadataSession? previousSession)
    {
        List<string> paths = metadataReferences
            .OfType<PortableExecutableReference>()
            .Select(portableExecutableReference => portableExecutableReference.FilePath)
            .ToList();

        var inputs = DotNetMetadataInputSnapshot.Capture(paths, importOptions);

        // Establish the target type universe before adding optional host fallbacks.
        var coreAssemblyName = importOptions?.CoreAssemblyName ??
            FindReferenceCoreAssemblyIdentity(paths);
        if (importOptions is not null && coreAssemblyName is null)
            throw new TargetInitializationException("The supplied references do not contain a core library defining System.Object.");

        coreAssemblyName ??= typeof(object).Assembly.GetName().FullName;
        if (importOptions is null)
        {
            var runtimeCorePath = typeof(object).Assembly.Location;
            if (!string.IsNullOrEmpty(runtimeCorePath) && !paths.Contains(runtimeCorePath, StringComparer.OrdinalIgnoreCase))
                paths.Add(runtimeCorePath);

            // Default .NET targeting retains host-assisted transitive dependency lookup.
            foreach (var knownPath in hostRuntime.GetHostMetadataAssemblyPaths())
            {
                if (!string.IsNullOrEmpty(knownPath) && File.Exists(knownPath) && !paths.Contains(knownPath, StringComparer.OrdinalIgnoreCase))
                    paths.Add(knownPath);
            }
        }

        if (previousSession?.CanReuse(inputs, coreAssemblyName) == true)
            return previousSession;

        var references = DotNetMetadataReferenceSet.Create(paths);
        foreach (var reference in references.References)
        {
            // Host registration is distinct from resolver first-match policy.
            if (!string.IsNullOrWhiteSpace(reference.SimpleName))
                DotNetHostRuntime.RegisterSharedMetadataAssemblyPath(reference.SimpleName, reference.Path);
        }

        try
        {
            return DotNetMetadataSession.Create(references, coreAssemblyName, inputs);
        }
        catch (Exception exception) when (exception is IOException or UnauthorizedAccessException or BadImageFormatException or TypeLoadException)
        {
            throw new TargetInitializationException(
                $"The metadata core '{coreAssemblyName}' could not be loaded: {exception.Message}", exception);
        }
    }

    private static string? FindReferenceCoreAssemblyIdentity(IEnumerable<string> paths)
    {
        foreach (var path in paths)
        {
            try
            {
                using var stream = File.OpenRead(path);
                using var peReader = new PEReader(stream);
                if (!peReader.HasMetadata)
                    continue;

                var reader = peReader.GetMetadataReader();
                if (!reader.IsAssembly)
                    continue;

                foreach (var handle in reader.TypeDefinitions)
                {
                    var definition = reader.GetTypeDefinition(handle);
                    if (definition.BaseType.IsNil &&
                        reader.StringComparer.Equals(definition.Namespace, "System") &&
                        reader.StringComparer.Equals(definition.Name, "Object"))
                    {
                        // Include the version: other framework versions may be present
                        // among fallback paths from earlier compilations.
                        return DotNetMetadataContextFactory.ReadAssemblyName(path).FullName;
                    }
                }
            }
            catch (Exception exception) when (exception is IOException or UnauthorizedAccessException or BadImageFormatException)
            {
                // Reference loading reports invalid/unavailable inputs separately.
            }
        }

        return null;
    }
}
