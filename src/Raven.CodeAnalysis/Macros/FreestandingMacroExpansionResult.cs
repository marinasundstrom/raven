using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

public sealed class FreestandingMacroExpansionResult
{
    public static FreestandingMacroExpansionResult Empty { get; } = new();

    public static FreestandingMacroExpansionResult FromExpression(ExpressionSyntax expression)
    {
        ArgumentNullException.ThrowIfNull(expression);
        return new FreestandingMacroExpansionResult
        {
            Expression = expression
        };
    }

    public static FreestandingMacroExpansionResult FromExpression(
        ExpressionSyntax expression,
        ImmutableArray<Diagnostic> diagnostics)
    {
        ArgumentNullException.ThrowIfNull(expression);
        return new FreestandingMacroExpansionResult
        {
            Expression = expression,
            Diagnostics = Normalize(diagnostics)
        };
    }

    public static FreestandingMacroExpansionResult FromExpression(
        ExpressionSyntax expression,
        ImmutableArray<Diagnostic> diagnostics,
        ImmutableArray<MacroExpansionDiagnostic> macroDiagnostics)
    {
        ArgumentNullException.ThrowIfNull(expression);
        return new FreestandingMacroExpansionResult
        {
            Expression = expression,
            Diagnostics = Normalize(diagnostics),
            MacroDiagnostics = Normalize(macroDiagnostics)
        };
    }

    public static FreestandingMacroExpansionResult FromDiagnostic(
        MacroExpansionDiagnostic diagnostic)
    {
        ArgumentNullException.ThrowIfNull(diagnostic);
        return new FreestandingMacroExpansionResult
        {
            MacroDiagnostics = [diagnostic]
        };
    }

    public static FreestandingMacroExpansionResult FromDiagnostics(
        ImmutableArray<Diagnostic> diagnostics)
        => new()
        {
            Diagnostics = Normalize(diagnostics)
        };

    public static FreestandingMacroExpansionResult FromDiagnostics(
        ImmutableArray<MacroExpansionDiagnostic> macroDiagnostics)
        => new()
        {
            MacroDiagnostics = Normalize(macroDiagnostics)
        };

    public static FreestandingMacroExpansionResult FromDiagnostics(
        ImmutableArray<Diagnostic> diagnostics,
        ImmutableArray<MacroExpansionDiagnostic> macroDiagnostics)
        => new()
        {
            Diagnostics = Normalize(diagnostics),
            MacroDiagnostics = Normalize(macroDiagnostics)
        };

    public ExpressionSyntax? Expression { get; set; }

    public ImmutableArray<MacroExpansionDiagnostic> MacroDiagnostics { get; set; } = ImmutableArray<MacroExpansionDiagnostic>.Empty;

    public ImmutableArray<Diagnostic> Diagnostics { get; set; } = ImmutableArray<Diagnostic>.Empty;

    /// <summary>
    /// Gets or sets the ordinary Raven fragments contributed for editor tooling.
    /// </summary>
    public ImmutableArray<MacroFragmentRegion> FragmentRegions { get; set; } =
        ImmutableArray<MacroFragmentRegion>.Empty;

    internal ImmutableArray<MacroFileDependency> FileDependencies { get; set; } =
        ImmutableArray<MacroFileDependency>.Empty;

    private static ImmutableArray<T> Normalize<T>(ImmutableArray<T> values)
        => values.IsDefault ? ImmutableArray<T>.Empty : values;
}

internal sealed record FreestandingMacroExpansionCacheEntry(
    FreestandingMacroExpansionResult? Result)
{
    public bool IsCurrent()
        => Result is null ||
           Result.FileDependencies.All(static dependency => dependency.IsCurrent());
}
