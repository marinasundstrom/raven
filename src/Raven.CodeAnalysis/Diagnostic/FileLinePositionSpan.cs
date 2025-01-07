using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

public struct FileLinePositionSpan
{
    public FileLinePositionSpan(string path, LinePosition startLinePosition, LinePosition endLinePosition)
    {
        Path = path;
        StartLinePosition = startLinePosition;
        EndLinePosition = endLinePosition;
    }

    public string Path { get; }
    public LinePosition StartLinePosition { get; }
    public LinePosition EndLinePosition { get; }
}