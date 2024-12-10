using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public class Diagnostic
{
    public string Message { get; }
    public TextSpan Span { get; }
    public DiagnosticSeverity Severity { get; }

    public Diagnostic(string message, TextSpan span, DiagnosticSeverity severity)
    {
        Message = message;
        Span = span;
        Severity = severity;
    }

    public override string ToString() => $"{Severity}: {Message}";
}

public enum DiagnosticSeverity
{
    Hidden = 0,
    Info = 1,
    Warning = 2,
    Error = 3
}