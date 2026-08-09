using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Provides diagnostics shared by all macro invocation contexts.
/// </summary>
public abstract class MacroContext
{
    private readonly ImmutableArray<Diagnostic>.Builder _diagnostics =
        ImmutableArray.CreateBuilder<Diagnostic>();
    private readonly ImmutableArray<MacroExpansionDiagnostic>.Builder _macroDiagnostics =
        ImmutableArray.CreateBuilder<MacroExpansionDiagnostic>();

    public abstract MacroExpansionDiagnostic CreateDiagnostic(
        string message,
        DiagnosticSeverity severity = DiagnosticSeverity.Error,
        SyntaxNode? syntax = null,
        string? code = null);

    public void ReportDiagnostic(
        string message,
        DiagnosticSeverity severity = DiagnosticSeverity.Error,
        SyntaxNode? syntax = null,
        string? code = null)
        => ReportDiagnostic(CreateDiagnostic(message, severity, syntax, code));

    public void ReportDiagnostic(Diagnostic diagnostic)
    {
        ArgumentNullException.ThrowIfNull(diagnostic);
        _diagnostics.Add(diagnostic);
    }

    public void ReportDiagnostics(IEnumerable<Diagnostic> diagnostics)
    {
        ArgumentNullException.ThrowIfNull(diagnostics);
        _diagnostics.AddRange(diagnostics);
    }

    public void ReportDiagnostic(MacroExpansionDiagnostic diagnostic)
    {
        ArgumentNullException.ThrowIfNull(diagnostic);
        _macroDiagnostics.Add(diagnostic);
    }

    public void ReportDiagnostics(IEnumerable<MacroExpansionDiagnostic> diagnostics)
    {
        ArgumentNullException.ThrowIfNull(diagnostics);
        _macroDiagnostics.AddRange(diagnostics);
    }

    internal ImmutableArray<Diagnostic> GetReportedDiagnostics()
        => _diagnostics.ToImmutable();

    internal ImmutableArray<MacroExpansionDiagnostic> GetReportedMacroDiagnostics()
        => _macroDiagnostics.ToImmutable();

    internal void AddReportedDiagnostics(MacroContext context)
    {
        ArgumentNullException.ThrowIfNull(context);
        _diagnostics.AddRange(context.GetReportedDiagnostics());
        _macroDiagnostics.AddRange(context.GetReportedMacroDiagnostics());
    }
}
