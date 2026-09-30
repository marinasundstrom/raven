using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;

namespace Raven.CodeAnalysis.Metadata;

// Shared only between compilations with compatible options and references.
// Never retain a Compilation or its symbols here. The context can outlive any
// one snapshot, so individual compilations must not dispose it.
internal sealed class DotNetMetadataSession
{
    private readonly MetadataLoadContext _context;

    private DotNetMetadataSession(MetadataLoadContext context)
    {
        _context = context;
    }

    internal static DotNetMetadataSession Create(
        IEnumerable<string> paths,
        string? coreAssemblyName,
        Action<string, string> registerAssemblyPath)
        => new(DotNetMetadataContextFactory.Create(paths, coreAssemblyName, registerAssemblyPath));

    internal Assembly CoreAssembly => _context.CoreAssembly!;

    internal Assembly LoadFromAssemblyName(AssemblyName identity)
        => _context.LoadFromAssemblyName(identity);

    internal Assembly LoadFromPath(string fullPath, AssemblyName? fallbackIdentity)
    {
        try
        {
            var bytes = File.ReadAllBytes(fullPath);
            return _context.LoadFromStream(new MemoryStream(bytes));
        }
        catch when (fallbackIdentity is not null)
        {
            // Preserve the existing compatibility fallback for references whose
            // path cannot be loaded directly but whose identity can be resolved.
            return _context.LoadFromAssemblyName(fallbackIdentity);
        }
    }
}
