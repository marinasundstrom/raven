namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal class DiagnosticInfo
{
    public DiagnosticDescriptor Descriptor { get; }
    public TextSpan Span { get; }
    public object[] Args { get; }

    private DiagnosticInfo(DiagnosticDescriptor descriptor, TextSpan span, object[] args)
    {
        Descriptor = descriptor;
        Span = span;
        Args = args;
    }

    public static DiagnosticInfo Create(DiagnosticDescriptor descriptor, TextSpan span, params object[] args)
    {
        return new DiagnosticInfo(descriptor, span, args);
    }
}