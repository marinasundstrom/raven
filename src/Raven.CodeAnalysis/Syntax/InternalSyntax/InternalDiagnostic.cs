namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal class InternalDiagnostic
{
    public DiagnosticDescriptor Descriptor { get; }
    public TextSpan Span { get; }
    public object[] Args { get; }

    private InternalDiagnostic(DiagnosticDescriptor descriptor, TextSpan span, object[] args)
    {
        Descriptor = descriptor;
        Span = span;
        Args = args;
    }

    public static InternalDiagnostic Create(DiagnosticDescriptor descriptor, TextSpan span, params object[] args)
    {
        return new InternalDiagnostic(descriptor, span, args);
    }
}