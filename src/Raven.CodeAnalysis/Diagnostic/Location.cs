using System.Diagnostics;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public abstract class Location : IComparable<Location>, IEquatable<Location>
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

    //public IModuleSymbol MetadataModule { get; };

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

    public int CompareTo(Location? other)
    {
        if (other is null)
            return 1;

        // 1. Compare by file path (null-safe)
        var path1 = GetLineSpan().Path ?? "";
        var path2 = other.GetLineSpan().Path ?? "";

        int pathComparison = string.Compare(path1, path2, StringComparison.OrdinalIgnoreCase);
        if (pathComparison != 0)
            return pathComparison;

        // 2. Compare by line number
        int lineComparison = GetLineSpan().StartLinePosition.Line
            .CompareTo(other.GetLineSpan().StartLinePosition.Line);
        if (lineComparison != 0)
            return lineComparison;

        // 3. Compare by character position
        int charComparison = GetLineSpan().StartLinePosition.Character
            .CompareTo(other.GetLineSpan().StartLinePosition.Character);
        if (charComparison != 0)
            return charComparison;

        // 4. Compare by span start offset as tiebreaker
        return SourceSpan.Start.CompareTo(other.SourceSpan.Start);
    }

    public override bool Equals(object? obj)
        => Equals(obj as Location);

    public bool Equals(Location? other)
    {
        if (other is null)
            return false;

        // Quick check for reference equality
        if (ReferenceEquals(this, other))
            return true;

        // Must be of same runtime type
        if (GetType() != other.GetType())
            return false;

        // Compare spans and trees (simplest for source locations)
        return SourceSpan == other.SourceSpan &&
               Equals(SourceTree, other.SourceTree);
    }

    public override int GetHashCode()
    {
        unchecked
        {
            int hash = 17;

            hash = hash * 31 + SourceSpan.GetHashCode();
            hash = hash * 31 + (SourceTree?.GetHashCode() ?? 0);

            return hash;
        }
    }

    public static bool operator ==(Location? left, Location? right)
        => Equals(left, right);

    public static bool operator !=(Location? left, Location? right)
        => !Equals(left, right);
}