using System.Diagnostics;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public abstract class Location
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
}