using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;

namespace Raven.CodeAnalysis;

public static class RavenFileExtensions
{
    public const string Raven = ".rvn";
    public const string LegacyRaven = ".rav";
    public const string Project = ".rvnproj";
    public const string LegacyProject = ".ravenproj";

    public static readonly ImmutableArray<string> All = ImmutableArray.Create(Raven, LegacyRaven);
    public static readonly ImmutableArray<string> ProjectAll = ImmutableArray.Create(Project, LegacyProject);

    public static bool HasRavenExtension(string path)
    {
        if (path is null) throw new ArgumentNullException(nameof(path));
        var ext = Path.GetExtension(path);
        return All.Any(e => string.Equals(e, ext, StringComparison.OrdinalIgnoreCase));
    }

    public static bool HasProjectExtension(string path)
    {
        if (path is null) throw new ArgumentNullException(nameof(path));
        var ext = Path.GetExtension(path);
        return ProjectAll.Any(e => string.Equals(e, ext, StringComparison.OrdinalIgnoreCase));
    }
}
