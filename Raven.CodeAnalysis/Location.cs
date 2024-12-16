using System.Diagnostics;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public class Location
{
    public Location(TextSpan span)
    {
        Span = span;
    }

    public TextSpan Span { get; }

    public Location(int line, int column)
    {
        Line = line;
        Column = column;
    }

    public int Line { get; }
    public int Column { get; }

    protected virtual string GetDebuggerDisplay()
    {
        string result = this.GetType().Name;
        return "(" + "SourceFile" + "@" + (Line) + ":" + Column + ")";
        
        /*
        string result = this.GetType().Name;
        var pos = GetLineSpan();
        if (pos.Path != null)
        {
            // user-visible line and column counts are 1-based, but internally are 0-based.
            result += "(" + pos.Path + "@" + (pos.StartLinePosition.Line + 1) + ":" + (pos.StartLinePosition.Character + 1) + ")";
        }

        return result;*/
    }
    
    public virtual FileLinePositionSpan GetLineSpan()
    {
        return default(FileLinePositionSpan);
    }
}

public struct FileLinePositionSpan
{
}