using System.Runtime.InteropServices;
using System.Text.RegularExpressions;

namespace Raven.CodeAnalysis;

public static class ReferenceAssemblyPaths
{
    /// <summary>
    /// Returns full paths to all reference assemblies (*.dll) for the resolved (pack version, TFM).
    /// sdkVersion: exact version (e.g. "9.0.7"), wildcard (e.g. "9.*"), or null/"*" for latest installed.
    /// targetFramework: exact TFM (e.g. "net9.0"), wildcard (e.g. "net9.*"), or null/"*" for latest available under that version.
    /// packId: e.g. "Microsoft.NETCore.App.Ref" (default) or "Microsoft.WindowsDesktop.App.Ref".
    /// </summary>
    public static string[] GetReferenceAssemblyPaths(string? sdkVersion = null, string? targetFramework = null, string packId = "Microsoft.NETCore.App.Ref")
    {
        var dir = GetReferenceAssemblyDir(sdkVersion, targetFramework, packId);
        if (dir is null || !Directory.Exists(dir))
            return Array.Empty<string>();

        return Directory.GetFiles(dir, "*.dll");
    }

    /// <summary>
    /// Resolves the reference assemblies directory for the chosen (pack version, TFM).
    /// See GetReferenceAssemblyPaths for parameter semantics.
    /// </summary>
    public static string? GetReferenceAssemblyDir(string? sdkVersion = null, string? targetFramework = null, string packId = "Microsoft.NETCore.App.Ref")
    {
        foreach (var root in GetDotNetRoots())
        {
            var packRoot = Path.Combine(root, "packs", packId);
            if (!Directory.Exists(packRoot))
                continue;

            // Enumerate candidate versions (filtered by sdkVersion if provided), newest-first
            foreach (var versionDir in EnumerateCandidateVersionDirs(packRoot, sdkVersion))
            {
                var refRoot = Path.Combine(versionDir, "ref");
                if (!Directory.Exists(refRoot))
                    continue;

                var tfmDir = ResolveTfmDirectory(refRoot, targetFramework);
                if (tfmDir is not null)
                    return tfmDir; // <-- pick the first version that actually has the requested TFM
            }
        }

        return null;
    }

    private static IEnumerable<string> EnumerateCandidateVersionDirs(string packRoot, string? sdkVersion)
    {
        var versions = Directory.GetDirectories(packRoot)
                                .Select(Path.GetFileName)
                                .Where(n => !string.IsNullOrEmpty(n))
                                .Select(n => n!)
                                .ToArray()!;
        if (versions.Length == 0)
            yield break;

        // Apply version filter (supports exact, wildcard, or null/"*" = no filter)
        IEnumerable<string> candidates = versions;
        if (!string.IsNullOrWhiteSpace(sdkVersion) && sdkVersion != "*")
        {
            if (sdkVersion.IndexOf('*') >= 0 || sdkVersion.IndexOf('?') >= 0)
            {
                var re = WildcardToRegex(sdkVersion!);
                candidates = versions.Where(v => Regex.IsMatch(v, re, RegexOptions.IgnoreCase));
            }
            else
            {
                var exact = versions.Where(v => string.Equals(v, sdkVersion, StringComparison.OrdinalIgnoreCase)).ToArray();
                candidates = exact.Length > 0
                    ? exact
                    : versions.Where(v => Regex.IsMatch(v, WildcardToRegex(sdkVersion + "*"), RegexOptions.IgnoreCase));
            }
        }

        // Sort semver desc (stable > prerelease), then yield full paths newest-first
        foreach (var v in candidates
            .Select(v => (name: v, sem: SemVer.TryParse(v)))
            .Where(x => x.sem is not null)
            .OrderByDescending(x => x.sem, SemVer.Comparer)
            .Select(x => x.name))
        {
            yield return Path.Combine(packRoot, v);
        }
    }

    public static string GetRuntimeDll(string? sdkVersion = null, string? targetFramework = null, string packId = "Microsoft.NETCore.App.Ref")
    {
        var refAssembliesPath = GetReferenceAssemblyDir(sdkVersion, targetFramework, packId);
        return Path.Combine(refAssembliesPath!, "System.Runtime.dll");
    }

    // ----- helpers -----

    private static IEnumerable<string> GetDotNetRoots()
    {
        // Respect DOTNET_ROOT and DOTNET_ROOT(x86) first
        var envRoots = new[]
        {
            Environment.GetEnvironmentVariable("DOTNET_ROOT"),
            Environment.GetEnvironmentVariable("DOTNET_ROOT(x86)")
        }.Where(s => !string.IsNullOrWhiteSpace(s));

        foreach (var r in envRoots!)
            if (Directory.Exists(r!))
                yield return r!;

        // Default install location for dotnet-install scripts
        var userHome = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);
        var userDotNet = Path.Combine(userHome, ".dotnet");
        if (Directory.Exists(userDotNet))
            yield return userDotNet;

        // Fallbacks by OS
        if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
        {
            var x64 = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles), "dotnet");
            var x86 = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86), "dotnet");
            if (Directory.Exists(x64)) yield return x64;
            if (Directory.Exists(x86)) yield return x86;
        }
        else if (RuntimeInformation.IsOSPlatform(OSPlatform.OSX))
        {
            // Default dotnet location on macOS (MS installer)
            const string ms = "/usr/local/share/dotnet";
            if (Directory.Exists(ms)) yield return ms;

            // Homebrew can symlink DOTNET_ROOT; if not set, try common brew prefix
            const string brew = "/opt/homebrew/opt/dotnet/libexec";
            if (Directory.Exists(brew)) yield return brew;
        }
        else // Linux
        {
            var candidates = new[]
            {
                "/usr/share/dotnet",
                "/usr/lib/dotnet",
                "/snap/dotnet-sdk/current", // snap
            };

            foreach (var c in candidates)
                if (Directory.Exists(c))
                    yield return c;
        }
    }

    private static string? ResolveVersionDirectory(string packRoot, string? sdkVersion)
    {
        var versions = Directory.GetDirectories(packRoot)
                                .Select(Path.GetFileName)
                                .Where(n => !string.IsNullOrEmpty(n))
                                .ToArray()!;

        if (versions.Length == 0)
            return null;

        // Filter by wildcard/explicit if provided
        IEnumerable<string> candidates = versions;
        if (!string.IsNullOrWhiteSpace(sdkVersion) && sdkVersion != "*")
        {
            if (sdkVersion.IndexOf('*') >= 0 || sdkVersion.IndexOf('?') >= 0)
            {
                var re = WildcardToRegex(sdkVersion!);
                candidates = versions.Where(v => Regex.IsMatch(v, re, RegexOptions.IgnoreCase));
            }
            else
            {
                var exact = versions.Where(v => string.Equals(v, sdkVersion, StringComparison.OrdinalIgnoreCase)).ToArray();
                if (exact.Length > 0)
                {
                    candidates = exact;
                }
                else
                {
                    var re = WildcardToRegex(sdkVersion + "*");
                    candidates = versions.Where(v => Regex.IsMatch(v, re, RegexOptions.IgnoreCase));
                }
            }
        }

        // Pick the highest semantic version (stable > prerelease)
        var best = candidates
            .Select(v => (v, sem: SemVer.TryParse(v)))
            .Where(x => x.sem is not null)
            .OrderByDescending(x => x.sem, SemVer.Comparer)
            .FirstOrDefault();

        return best.v is null ? null : Path.Combine(packRoot, best.v);
    }

    private static string? ResolveTfmDirectory(string refRoot, string? targetFramework)
    {
        var tfms = Directory.GetDirectories(refRoot)
                            .Select(Path.GetFileName)
                            .Where(n => !string.IsNullOrEmpty(n))
                            .ToArray()!;

        if (tfms.Length == 0)
            return null;

        IEnumerable<string> candidates = tfms;

        if (!string.IsNullOrWhiteSpace(targetFramework) && targetFramework != "*")
        {
            var re = WildcardToRegex(targetFramework!);
            candidates = candidates.Where(t => Regex.IsMatch(t, re, RegexOptions.IgnoreCase));
        }

        // If still multiple, pick the "highest" TFM by netX.Y (ignores platform qualifiers for ranking)
        var best = candidates
            .Select(t => (t, rank: TfmRank.Parse(t)))
            .OrderByDescending(x => x.rank)
            .ThenByDescending(x => x.t, StringComparer.OrdinalIgnoreCase)
            .FirstOrDefault();

        return best.t is null ? null : Path.Combine(refRoot, best.t);
    }

    private static string WildcardToRegex(string pattern)
    {
        // Convert simple glob (*, ?) into a regex that matches the entire string
        var escaped = Regex.Escape(pattern).Replace("\\*", ".*").Replace("\\?", ".");
        return $"^{escaped}$";
    }

    // ----- version + TFM parsing -----

    private sealed class SemVer
    {
        public int[] Core { get; }
        public string? PreRelease { get; }
        private SemVer(int[] core, string? pre) { Core = core; PreRelease = pre; }

        public static SemVer? TryParse(string s)
        {
            // Examples: 9.0.7, 9.0.0-preview.2.24157.4
            // Split on '-', first part = numeric dotted, rest = prerelease
            var parts = s.Split('-', 2);
            var num = parts[0].Split('.');
            var core = new int[num.Length];
            for (int i = 0; i < num.Length; i++)
            {
                if (!int.TryParse(num[i], out var n)) return null;
                core[i] = n;
            }
            string? pre = parts.Length > 1 ? parts[1] : null;
            return new SemVer(core, pre);
        }

        public static IComparer<SemVer?> Comparer { get; } = new SemVerComparer();

        private sealed class SemVerComparer : IComparer<SemVer?>
        {
            public int Compare(SemVer? x, SemVer? y)
            {
                if (ReferenceEquals(x, y)) return 0;
                if (x is null) return -1;
                if (y is null) return 1;

                // Compare numeric core with different lengths
                var max = Math.Max(x.Core.Length, y.Core.Length);
                for (int i = 0; i < max; i++)
                {
                    var xi = i < x.Core.Length ? x.Core[i] : 0;
                    var yi = i < y.Core.Length ? y.Core[i] : 0;
                    if (xi != yi) return xi.CompareTo(yi);
                }

                // Stable (no prerelease) > prerelease
                var xPre = string.IsNullOrEmpty(x.PreRelease);
                var yPre = string.IsNullOrEmpty(y.PreRelease);
                if (xPre != yPre) return xPre ? 1 : -1;

                // Both stable or both prerelease: compare prerelease lexically (best-effort)
                if (!xPre && !yPre)
                    return string.Compare(x.PreRelease, y.PreRelease, StringComparison.OrdinalIgnoreCase);

                return 0;
            }
        }
    }

    private readonly record struct TfmRank(int Major, int Minor)
        : IComparable<TfmRank>
    {
        public int CompareTo(TfmRank other)
        {
            var c = Major.CompareTo(other.Major);
            if (c != 0) return c;
            return Minor.CompareTo(other.Minor);
        }

        public static TfmRank Parse(string tfm)
        {
            // Accepts: net9, net9.0, net9.0-windows10.0.19041.0, net8.0-android, etc.
            // Extract the first occurrence of "net<major>[.<minor>]" and ignore any qualifiers.
            if (string.IsNullOrWhiteSpace(tfm))
                return new TfmRank(0, 0);

            var m = Regex.Match(tfm, @"\bnet(?<maj>\d+)(?:\.(?<min>\d+))?", RegexOptions.IgnoreCase);
            if (!m.Success)
                return new TfmRank(0, 0);

            int major = int.TryParse(m.Groups["maj"].Value, out var M) ? M : 0;
            int minor = int.TryParse(m.Groups["min"].Value, out var mnr) ? mnr : 0;

            return new TfmRank(major, minor);
        }
    }
}
