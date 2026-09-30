using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;

namespace Raven.CodeAnalysis.Metadata;

// Compilation-independent evidence for reusing a .NET metadata session. Preserve
// order: core discovery and duplicate-identity admission both use the first input.
// Host-assisted fallback registration retains its existing process-wide lifetime;
// this snapshots supplied files, not every possible transitive host dependency.
internal sealed class DotNetMetadataInputSnapshot
{
    private readonly ImmutableArray<FileStamp> _files;
    private readonly MetadataImportOptions? _importOptions;

    private DotNetMetadataInputSnapshot(ImmutableArray<FileStamp> files, MetadataImportOptions? importOptions)
    {
        _files = files;
        _importOptions = importOptions;
    }

    internal static DotNetMetadataInputSnapshot Capture(IEnumerable<string> paths, MetadataImportOptions? importOptions)
    {
        var files = ImmutableArray.CreateBuilder<FileStamp>();
        foreach (var path in paths)
        {
            if (string.IsNullOrWhiteSpace(path))
                continue;

            var fullPath = Path.GetFullPath(path);
            var file = new FileInfo(fullPath);
            files.Add(file.Exists
                ? new FileStamp(fullPath, true, file.Length, file.LastWriteTimeUtc.Ticks)
                : new FileStamp(fullPath, false, 0, 0));
        }

        return new(files.ToImmutable(), importOptions);
    }

    internal bool Matches(DotNetMetadataInputSnapshot other)
        => _importOptions == other._importOptions && _files.SequenceEqual(other._files);

    // Ordinal paths conservatively avoid conflating different files on a
    // case-sensitive filesystem. Size/time stamps preserve the existing policy;
    // they are not content hashes or an atomic snapshot of files being rewritten.
    private readonly record struct FileStamp(string Path, bool Exists, long Length, long LastWriteTimeUtcTicks);
}
