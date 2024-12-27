using System.Diagnostics;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public abstract class Location : IComparable<Location>
{
    public static Location None { get; } = new NoLocation();

    public static Location Create(string path, TextSpan textSpan, LinePositionSpan lineSpan)
    {
        return new ExternalFileLocation(path, textSpan, lineSpan);
    }

    public static Location Create(SyntaxTree sourceTree, TextSpan sourceSpan)
    {
        return new SourceLocation(sourceTree, sourceSpan);
    }

    /*
    public Location(MetadataModule metadataModule)
    {
        MetadataModule = metadataModule;
    }
    */

    public bool IsInMetadata => this is MetadataLocation;

    public bool IsSource => this is SourceLocation or ExternalFileLocation;

    public LocationKind Kind { get; protected set; } = LocationKind.None;

    //public MetadataModule MetadataModule { get; };

    public TextSpan SourceSpan { get; protected set; }
    public SyntaxTree SourceTree { get; protected set; }

    protected virtual string GetDebuggerDisplay()
    {
        string result = this.GetType().Name;
        var pos = GetLineSpan();

        if (pos.Path != null)
        {
            // user-visible line and column counts are 1-based, but internally are 0-based.
            result += "(" + pos.Path + "@" + (pos.StartLinePosition.Line + 1) + ":" + (pos.StartLinePosition.Character + 1) + ")";
        }

        return result;
    }

    public virtual FileLinePositionSpan GetLineSpan()
    {
        return default;
    }

    // Implement the comparison logic
    public int CompareTo(Location? other)
    {
        if (other == null)
            return 1;

        var lineSpan = GetLineSpan();

        /*
        // Compare by Path
        int pathComparison = string.Compare(GetLineSpan().Path, other.GetLineSpan().Path, StringComparison.OrdinalIgnoreCase);
        if (pathComparison != 0)
            return pathComparison; */

        /*
                // Compare by Start Line
                int startLineComparison = GetLineSpan().StartLinePosition.Line.CompareTo(other.GetLineSpan().StartLinePosition.Line);
                if (startLineComparison != 0)
                    return startLineComparison;

                // Compare by Start Character
                int startCharComparison = GetLineSpan().StartLinePosition.Character.CompareTo(other.GetLineSpan().StartLinePosition.Character);
                if (startCharComparison != 0)
                    return startCharComparison;
        */
        // Compare by Span Start
        return SourceSpan.Start.CompareTo(other.SourceSpan.Start);
    }
}