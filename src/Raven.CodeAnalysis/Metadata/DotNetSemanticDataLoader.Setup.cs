using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;

namespace Raven.CodeAnalysis.Metadata;

internal sealed partial class DotNetSemanticDataLoader
{
    // Compilation decides whether an older session is reusable. The .NET loader
    // owns which reference universe and core to open when a fresh one is needed.
    internal static DotNetMetadataSession OpenSession(
        Compilation compilation,
        DotNetMetadataSession? reusableSession)
    {
        List<string> paths = compilation.References
            .OfType<PortableExecutableReference>()
            .Select(portableExecutableReference => portableExecutableReference.FilePath)
            .ToList();

        var importOptions = compilation.Options.MetadataImportOptions;
        // Establish the target type universe before adding optional host fallbacks.
        var coreAssemblyName = importOptions?.CoreAssemblyName ??
            FindReferenceCoreAssemblyIdentity(paths) ?? typeof(object).Assembly.GetName().FullName;
        if (importOptions is null)
        {
            var runtimeCorePath = typeof(object).Assembly.Location;
            if (!string.IsNullOrEmpty(runtimeCorePath) && !paths.Contains(runtimeCorePath, StringComparer.OrdinalIgnoreCase))
                paths.Add(runtimeCorePath);

            // Default .NET targeting retains host-assisted transitive dependency lookup.
            foreach (var knownPath in compilation.GetHostMetadataAssemblyPaths())
            {
                if (!string.IsNullOrEmpty(knownPath) && File.Exists(knownPath) && !paths.Contains(knownPath, StringComparer.OrdinalIgnoreCase))
                    paths.Add(knownPath);
            }
        }

        if (reusableSession is not null)
            return reusableSession;

        var references = DotNetMetadataReferenceSet.Create(paths);
        foreach (var reference in references.References)
        {
            // Host registration is distinct from resolver first-match policy.
            if (!string.IsNullOrWhiteSpace(reference.SimpleName))
                Compilation.RegisterSharedMetadataAssemblyPath(reference.SimpleName, reference.Path);
        }

        return DotNetMetadataSession.Create(references, coreAssemblyName);
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
