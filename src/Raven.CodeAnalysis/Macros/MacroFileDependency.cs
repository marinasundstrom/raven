using System.Collections.Immutable;
using System.IO;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

internal readonly record struct MacroFileDependency(
    string Path,
    bool Exists,
    long Length,
    long LastWriteTimeUtcTicks)
{
    public static MacroFileDependency Capture(string path)
    {
        try
        {
            var file = new FileInfo(path);
            file.Refresh();
            return file.Exists
                ? new MacroFileDependency(
                    path,
                    Exists: true,
                    file.Length,
                    file.LastWriteTimeUtc.Ticks)
                : new MacroFileDependency(path, Exists: false, Length: 0, LastWriteTimeUtcTicks: 0);
        }
        catch (Exception exception) when (
            exception is IOException or UnauthorizedAccessException)
        {
            return new MacroFileDependency(path, Exists: false, Length: 0, LastWriteTimeUtcTicks: 0);
        }
    }

    public bool IsCurrent()
        => Equals(Capture(Path));
}

/// <summary>
/// Describes the result of reading a file observed by a macro expansion.
/// </summary>
public readonly record struct MacroFileReadResult(
    string Path,
    string? Content,
    MacroFileReadStatus Status,
    string? Error)
{
    public static MacroFileReadResult Succeeded(string path, string content)
        => new(path, content, MacroFileReadStatus.Success, Error: null);

    public static MacroFileReadResult Missing(string path)
        => new(path, Content: null, MacroFileReadStatus.Missing, Error: null);

    public static MacroFileReadResult Failed(string path, string error)
        => new(path, Content: null, MacroFileReadStatus.Failed, error);
}

/// <summary>
/// Describes whether a macro-observed file was read successfully.
/// </summary>
public enum MacroFileReadStatus
{
    Success,
    Missing,
    Failed
}

internal static class MacroFileReader
{
    public static MacroFileReadResult Read(
        SyntaxNode syntax,
        string path,
        ImmutableArray<MacroFileDependency>.Builder dependencies)
    {
        ArgumentNullException.ThrowIfNull(path);

        string fullPath;
        try
        {
            var sourcePath = syntax.SyntaxTree?.FilePath;
            var basePath = string.IsNullOrWhiteSpace(sourcePath)
                ? Environment.CurrentDirectory
                : Path.GetDirectoryName(Path.GetFullPath(sourcePath))
                    ?? Environment.CurrentDirectory;
            fullPath = Path.GetFullPath(path, basePath);
        }
        catch (Exception exception) when (
            exception is ArgumentException or NotSupportedException or PathTooLongException)
        {
            return MacroFileReadResult.Failed(path, exception.Message);
        }

        var dependency = MacroFileDependency.Capture(fullPath);
        dependencies.Add(dependency);
        if (!dependency.Exists)
            return MacroFileReadResult.Missing(fullPath);

        try
        {
            return MacroFileReadResult.Succeeded(fullPath, File.ReadAllText(fullPath));
        }
        catch (Exception exception) when (
            exception is IOException or UnauthorizedAccessException)
        {
            return MacroFileReadResult.Failed(fullPath, exception.Message);
        }
    }
}
