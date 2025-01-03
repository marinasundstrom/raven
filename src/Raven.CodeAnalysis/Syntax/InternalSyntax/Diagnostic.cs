namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal class Diagnostic
{
    public DiagnosticDescriptor Descriptor { get; }
    public TextSpan Span { get; }
    public object[] Args { get; }

    private Diagnostic(DiagnosticDescriptor descriptor, TextSpan span, object[] args)
    {
        Descriptor = descriptor;
        Span = span;
        Args = args;
    }

    public static Diagnostic Create(DiagnosticDescriptor descriptor, TextSpan span, params object[] args)
    {
        return new Diagnostic(descriptor, span, args);
    }
}