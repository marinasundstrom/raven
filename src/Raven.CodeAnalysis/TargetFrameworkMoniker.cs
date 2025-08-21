using System;
using System.Globalization;
using System.Text.RegularExpressions;

namespace Raven.CodeAnalysis;

public enum FrameworkId
{
    NetCoreApp,     // .NET 5+ and .NET Core 1.x–3.x (both map here for framework string purposes)
    NetStandard,
    NetFramework
}

public sealed record TargetFrameworkMoniker(
    FrameworkId Framework,
    Version Version,
    string? TargetPlatform = null,   // e.g., "windows", "ios", "android", "maccatalyst", "tvos"
    Version? TargetPlatformVersion = null, // e.g., 10.0.19041, 16.1, 34
    string? Raw = null               // original input for debugging/round-trip fidelity
)
{
    // --- Parsing ---

    // Recognizes: net8.0, net6.0-android34, net7.0-windows10.0.19041, netstandard2.1, netcoreapp3.1, net48
    private static readonly Regex TfmRegex = new(
        @"^(?:(netstandard)(?<stdver>\d+(\.\d+)*))|(?:(netcoreapp)(?<corever>\d+(\.\d+)*))|(?:(net)(?<netver>\d+(\.\d+)*))(?<plat>(?:-(?<platname>[a-zA-Z]+)(?<platver>\d+(?:\.\d+)*))?)$",
        RegexOptions.Compiled | RegexOptions.CultureInvariant);

    // Recognizes: .NETCoreApp,Version=v8.0  / .NETStandard,Version=v2.1  / .NETFramework,Version=v4.8
    private const string NetCorePrefix = ".NETCoreApp,Version=v";
    private const string NetStandardPrefix = ".NETStandard,Version=v";
    private const string NetFrameworkPrefix = ".NETFramework,Version=v";

    public static TargetFrameworkMoniker Parse(string tfmOrFull)
        => TryParse(tfmOrFull, out var tfm) ? tfm! :
           throw new FormatException($"Invalid target framework: '{tfmOrFull}'.");

    public static bool TryParse(string? tfmOrFull, out TargetFrameworkMoniker? result)
    {
        result = null;
        if (string.IsNullOrWhiteSpace(tfmOrFull))
            return false;

        var s = tfmOrFull.Trim();

        // Full framework-name form?
        if (s.StartsWith(NetCorePrefix, StringComparison.Ordinal) ||
            s.StartsWith(NetStandardPrefix, StringComparison.Ordinal) ||
            s.StartsWith(NetFrameworkPrefix, StringComparison.Ordinal))
        {
            return TryParseFullFrameworkName(s, out result);
        }

        // TFM form
        var m = TfmRegex.Match(s);
        if (!m.Success)
            return false;

        // netstandardX.Y
        if (m.Groups[1].Success) // "netstandard"
        {
            if (!Version.TryParse(m.Groups["stdver"].Value, out var v))
                return false;
            result = new TargetFrameworkMoniker(FrameworkId.NetStandard, Normalize(v), null, null, s);
            return true;
        }

        // netcoreappX.Y
        if (m.Groups[3].Success) // "netcoreapp"
        {
            if (!Version.TryParse(m.Groups["corever"].Value, out var v))
                return false;
            result = new TargetFrameworkMoniker(FrameworkId.NetCoreApp, Normalize(v), null, null, s);
            return true;
        }

        // netX[.Y][.Z][-platformN[.M]]
        if (m.Groups[5].Success) // "net"
        {
            if (!Version.TryParse(m.Groups["netver"].Value, out var v))
                return false;

            // Roslyn/MSBuild use: netXY (e.g., net48) == .NET Framework 4.8 if two digits with no dot
            // We'll map: if original had a dot → .NETCoreApp; if two digits/no dot → .NETFramework
            var originalVer = m.Groups["netver"].Value;
            FrameworkId fid;
            if (originalVer.IndexOf('.') < 0 && originalVer.Length == 2 && char.IsDigit(originalVer[0]) && char.IsDigit(originalVer[1]))
            {
                fid = FrameworkId.NetFramework;                 // net48 -> .NET Framework 4.8
                v = new Version(int.Parse(originalVer[0].ToString()), int.Parse(originalVer[1].ToString()));
            }
            else
            {
                fid = FrameworkId.NetCoreApp;                   // net5.0+, net6.0, etc.
                v = Normalize(v);
            }

            string? platName = null;
            Version? platVer = null;
            if (m.Groups["plat"].Success && m.Groups["platname"].Success)
            {
                platName = m.Groups["platname"].Value.ToLowerInvariant();
                if (m.Groups["platver"].Success)
                {
                    // platform versions can be like "10.0.19041" or "34"
                    if (!Version.TryParse(m.Groups["platver"].Value, out var pv))
                    {
                        // Try to coerce "34" → "34.0"
                        if (int.TryParse(m.Groups["platver"].Value, NumberStyles.Integer, CultureInfo.InvariantCulture, out var major))
                            pv = new Version(major, 0);
                        else
                            return false;
                    }
                    platVer = pv;
                }
            }

            result = new TargetFrameworkMoniker(fid, v, platName, platVer, s);
            return true;
        }

        return false;

        static Version Normalize(Version v) => v.Build < 0 ? new Version(v.Major, v.Minor) : v;
    }

    private static bool TryParseFullFrameworkName(string s, out TargetFrameworkMoniker? result)
    {
        result = null;

        if (s.StartsWith(NetCorePrefix, StringComparison.Ordinal))
        {
            if (!Version.TryParse(s.AsSpan(NetCorePrefix.Length), out var v)) return false;
            result = new TargetFrameworkMoniker(FrameworkId.NetCoreApp, v, null, null, s);
            return true;
        }

        if (s.StartsWith(NetStandardPrefix, StringComparison.Ordinal))
        {
            if (!Version.TryParse(s.AsSpan(NetStandardPrefix.Length), out var v)) return false;
            result = new TargetFrameworkMoniker(FrameworkId.NetStandard, v, null, null, s);
            return true;
        }

        if (s.StartsWith(NetFrameworkPrefix, StringComparison.Ordinal))
        {
            if (!Version.TryParse(s.AsSpan(NetFrameworkPrefix.Length), out var v)) return false;
            result = new TargetFrameworkMoniker(FrameworkId.NetFramework, v, null, null, s);
            return true;
        }

        return false;
    }

    // --- Formatting ---

    public string ToTfm()
    {
        return Framework switch
        {
            FrameworkId.NetStandard => $"netstandard{TrimVersion(Version)}",
            FrameworkId.NetFramework => $"net{Version.Major}{Version.Minor}", // net48 pattern
            FrameworkId.NetCoreApp => ComposeNetCoreAppTfm(),
            _ => Raw ?? ToString()
        };

        string ComposeNetCoreAppTfm()
        {
            var baseTfm = $"net{TrimVersion(Version)}";
            if (!string.IsNullOrEmpty(TargetPlatform))
            {
                baseTfm += "-" + TargetPlatform;
                if (TargetPlatformVersion is not null)
                    baseTfm += TrimVersion(TargetPlatformVersion);
            }
            return baseTfm;
        }

        static string TrimVersion(Version v)
        {
            if (v.Build <= 0 && v.Revision <= 0)
            {
                return v.Minor == 0 ? $"{v.Major}.0" : $"{v.Major}.{v.Minor}";
            }
            return $"{v.Major}.{v.Minor}.{v.Build}";
        }
    }

    public string ToFrameworkString()
    {
        return Framework switch
        {
            FrameworkId.NetStandard => $"{NetStandardPrefix}{Version}",
            FrameworkId.NetFramework => $"{NetFrameworkPrefix}{Version}",
            FrameworkId.NetCoreApp => $"{NetCorePrefix}{Version}",
            _ => Raw ?? ToString()
        };
    }

    public string GetDisplayName()
    {
        return Framework switch
        {
            FrameworkId.NetStandard => $".NET Standard {Version}",
            FrameworkId.NetFramework => $".NET Framework {Version.Major}.{Version.Minor}",
            FrameworkId.NetCoreApp => DisplayForNetCoreApp(),
            _ => Raw ?? ToString()
        };

        string DisplayForNetCoreApp()
        {
            var baseName = $".NET {Version}";
            if (!string.IsNullOrEmpty(TargetPlatform))
            {
                baseName += $" ({Cap(TargetPlatform)}"
                          + (TargetPlatformVersion is null ? "" : $" {TargetPlatformVersion}")
                          + ")";
            }
            return baseName;

            static string Cap(string s) => s.Length switch
            {
                0 => s,
                1 => char.ToUpperInvariant(s[0]).ToString(),
                _ => char.ToUpperInvariant(s[0]) + s[1..]
            };
        }
    }

    public override string ToString() => ToTfm();
}
