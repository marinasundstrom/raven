using System;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

public sealed class MacroExpansionDiagnostic
{
    public MacroExpansionDiagnostic(
        DiagnosticSeverity severity,
        string message,
        Location? location = null,
        string? code = null)
    {
        if (string.IsNullOrWhiteSpace(message))
            throw new ArgumentException("Macro diagnostic message cannot be null or whitespace.", nameof(message));

        Severity = severity;
        Message = message;
        Location = location;
        Code = code;
    }

    public DiagnosticSeverity Severity { get; }

    public string Message { get; }

    public Location? Location { get; }

    public string? Code { get; }

    public static MacroExpansionDiagnostic Error(string message, Location? location = null, string? code = null)
        => new(DiagnosticSeverity.Error, message, location, code);

    public static MacroExpansionDiagnostic Warning(string message, Location? location = null, string? code = null)
        => new(DiagnosticSeverity.Warning, message, location, code);

    public static MacroExpansionDiagnostic Info(string message, Location? location = null, string? code = null)
        => new(DiagnosticSeverity.Info, message, location, code);

    public static MacroExpansionDiagnostic Hidden(string message, Location? location = null, string? code = null)
        => new(DiagnosticSeverity.Hidden, message, location, code);
}
