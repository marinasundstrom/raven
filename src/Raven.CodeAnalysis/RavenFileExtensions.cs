using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;

namespace Raven.CodeAnalysis;

public static class RavenFileExtensions
{
    public const string Raven = ".rav";
    public const string Notebook = ".rvn";

    public static readonly ImmutableArray<string> All = ImmutableArray.Create(Raven, Notebook);

    public static bool HasRavenExtension(string path)
    {
        if (path is null) throw new ArgumentNullException(nameof(path));
        var ext = Path.GetExtension(path);
        return All.Any(e => string.Equals(e, ext, StringComparison.OrdinalIgnoreCase));
    }
}
