using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

public sealed class FreestandingMacroExpansionResult
{
    public static FreestandingMacroExpansionResult Empty { get; } = new();

    public ExpressionSyntax? Expression { get; set; }

    public ImmutableArray<MacroExpansionDiagnostic> MacroDiagnostics { get; set; } = ImmutableArray<MacroExpansionDiagnostic>.Empty;

    public ImmutableArray<Diagnostic> Diagnostics { get; set; } = ImmutableArray<Diagnostic>.Empty;
}
