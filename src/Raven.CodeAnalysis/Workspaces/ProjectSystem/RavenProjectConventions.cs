using System;
using System.IO;
using System.Linq;

namespace Raven.CodeAnalysis;

public class RavenProjectConventions
{
    public static RavenProjectConventions Default { get; } = new();

    public virtual string DefaultConfiguration => "Debug";

    public virtual string NormalizeConfiguration(string? configuration)
    {
        var candidate = string.IsNullOrWhiteSpace(configuration) ? DefaultConfiguration : configuration.Trim();
        var invalid = Path.GetInvalidFileNameChars();
        var chars = candidate.Where(c => Array.IndexOf(invalid, c) < 0).ToArray();
        return chars.Length == 0 ? DefaultConfiguration : new string(chars);
    }

    public virtual bool IsImplicitSourceFile(string projectDirectory, string filePath)
    {
        if (!RavenFileExtensions.HasRavenExtension(filePath))
            return false;

        var relative = Path.GetRelativePath(projectDirectory, filePath);
        if (relative.StartsWith("..", StringComparison.Ordinal))
            return false;

        var segments = relative
            .Split([Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar], StringSplitOptions.RemoveEmptyEntries);

        foreach (var segment in segments)
        {
            if (segment.Equals("obj", StringComparison.OrdinalIgnoreCase) ||
                segment.Equals("bin", StringComparison.OrdinalIgnoreCase))
            {
                return false;
            }
        }

        return true;
    }

    public virtual string GetGeneratedSourceDirectory(string projectDirectory, string? configuration)
    {
        var normalizedConfiguration = NormalizeConfiguration(configuration);
        return Path.Combine(projectDirectory, "obj", normalizedConfiguration, "raven", "generated");
    }

    public virtual string GetTargetFrameworkAttributeFileName(string projectName)
        => $"{projectName}.TargetFrameworkAttribute.g{RavenFileExtensions.Raven}";
}
