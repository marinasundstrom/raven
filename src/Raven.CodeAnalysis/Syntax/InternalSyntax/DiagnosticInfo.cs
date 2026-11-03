using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal class DiagnosticInfo
{
    public DiagnosticDescriptor Descriptor { get; }
    public TextSpan Span { get; }
    public object?[] Args { get; }

    private readonly SourceText? _sourceText;
    private readonly string _path;

    private DiagnosticInfo(DiagnosticDescriptor descriptor, TextSpan span, object?[] args,
        SourceText? sourceText = null, string path = "")
    {
        Descriptor = descriptor;
        Span = span;
        Args = args;
        _sourceText = sourceText;
        _path = path;
    }

    internal DiagnosticInfo WithSourceText(SourceText sourceText, string path)
        => new(Descriptor, Span, Args, sourceText, path);

    internal Diagnostic ToDiagnostic(SyntaxTree? tree)
    {
        var location = Location.None;
        if (tree is not null)
        {
            location = tree.GetLocation(Span);
        }
        else if (_sourceText is not null)
        {
            var (startLine, startColumn) = _sourceText.GetLineAndColumn(Span.Start);
            var (endLine, endColumn) = _sourceText.GetLineAndColumn(Span.End);
            location = Location.Create(_path, Span, new LinePositionSpan(
                new LinePosition(startLine - 1, startColumn - 1),
                new LinePosition(endLine - 1, endColumn - 1)));
        }
        return Diagnostic.Create(Descriptor, location, Args);
    }

    public static DiagnosticInfo Create(DiagnosticDescriptor descriptor, TextSpan span, params object?[] args)
    {
        return new DiagnosticInfo(descriptor, span, args);
    }
}
