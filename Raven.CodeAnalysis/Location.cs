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

    public string GetDebuggerDisplay() => "Foo";
}