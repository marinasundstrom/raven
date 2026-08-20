using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Provides diagnostics shared by all macro invocation contexts.
/// </summary>
public abstract class MacroContext
{
    private readonly SyntaxNode _invocationSyntax;
    private readonly ImmutableArray<Diagnostic>.Builder _diagnostics =
        ImmutableArray.CreateBuilder<Diagnostic>();
    private readonly ImmutableArray<MacroExpansionDiagnostic>.Builder _macroDiagnostics =
        ImmutableArray.CreateBuilder<MacroExpansionDiagnostic>();
    private HashSet<string>? _reservedNames;
    private Dictionary<string, int>? _nextUniqueNameSuffixes;

    protected MacroContext(SyntaxNode invocationSyntax)
    {
        _invocationSyntax = invocationSyntax ?? throw new ArgumentNullException(nameof(invocationSyntax));
    }

    /// <summary>
    /// Creates a deterministic identifier name that does not collide with an
    /// identifier authored in the invocation document or returned earlier by
    /// this context.
    /// </summary>
    /// <remarks>
    /// This helper prevents textual collisions for generated bindings. It does
    /// not select call-site or definition-site name lookup; macros should use
    /// authored syntax for intentional call-site references.
    /// </remarks>
    public string CreateUniqueName(string hint = "value")
    {
        ArgumentNullException.ThrowIfNull(hint);

        var normalizedHint = NormalizeNameHint(hint);
        var prefix = $"__macro_{normalizedHint}";
        _reservedNames ??= CollectReservedNames();
        _nextUniqueNameSuffixes ??= new Dictionary<string, int>(StringComparer.Ordinal);
        _nextUniqueNameSuffixes.TryGetValue(prefix, out var suffix);

        string candidate;
        do
        {
            candidate = $"{prefix}_{suffix++}";
        }
        while (!_reservedNames.Add(candidate));

        _nextUniqueNameSuffixes[prefix] = suffix;
        return candidate;
    }

    /// <summary>
    /// Creates identifier-name syntax with a deterministic name that does not
    /// collide with authored or previously generated identifiers.
    /// </summary>
    /// <remarks>
    /// This is a syntax-construction convenience over <see cref="CreateUniqueName"/>.
    /// It does not provide definition-site or call-site hygiene.
    /// </remarks>
    public IdentifierNameSyntax CreateUniqueIdentifier(string hint = "value")
        => SyntaxFactory.IdentifierName(CreateUniqueName(hint));

    /// <summary>
    /// Requires a syntax node to have the requested shape, reporting a macro
    /// diagnostic and returning <see langword="null"/> when it does not.
    /// </summary>
    public TSyntax? RequireSyntax<TSyntax>(
        SyntaxNode syntax,
        string? message = null,
        string? code = null)
        where TSyntax : SyntaxNode
    {
        ArgumentNullException.ThrowIfNull(syntax);

        if (syntax is TSyntax expectedSyntax)
            return expectedSyntax;

        var diagnosticMessage = message ?? $"Expected {typeof(TSyntax).Name}, but found {syntax.Kind}.";
        var sourceTree = _invocationSyntax.SyntaxTree;
        if (sourceTree is not null &&
            MacroSyntaxOrigin.TryGetSourceSpan(syntax, sourceTree, out var sourceSpan))
        {
            ReportDiagnostic(new MacroExpansionDiagnostic(
                DiagnosticSeverity.Error,
                diagnosticMessage,
                sourceTree.GetLocation(sourceSpan),
                code));
        }
        else
        {
            var diagnosticSyntax = ReferenceEquals(syntax.SyntaxTree, sourceTree)
                ? syntax
                : null;
            ReportDiagnostic(diagnosticMessage, syntax: diagnosticSyntax, code: code);
        }
        return null;
    }

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

    /// <summary>
    /// Reports all parser diagnostics carried by a macro syntax parse result.
    /// </summary>
    public void ReportDiagnostics<TSyntax>(MacroSyntaxParseResult<TSyntax> result)
        where TSyntax : SyntaxNode
    {
        ArgumentNullException.ThrowIfNull(result);
        ReportDiagnostics(result.Diagnostics);
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

    internal SyntaxNode InvocationSyntax => _invocationSyntax;

    internal ImmutableArray<MacroExpansionDiagnostic> GetReportedMacroDiagnostics()
        => _macroDiagnostics.ToImmutable();

    internal void AddReportedDiagnostics(MacroContext context)
    {
        ArgumentNullException.ThrowIfNull(context);
        _diagnostics.AddRange(context.GetReportedDiagnostics());
        _macroDiagnostics.AddRange(context.GetReportedMacroDiagnostics());
    }

    private HashSet<string> CollectReservedNames()
    {
        var reservedNames = new HashSet<string>(StringComparer.Ordinal);
        var source = _invocationSyntax.SyntaxTree?.GetText().ToString()
            ?? _invocationSyntax.ToFullString();

        for (var index = 0; index < source.Length; index++)
        {
            if (!SyntaxFacts.IsIdentifierStartCharacter(source[index]))
                continue;

            var start = index++;
            while (index < source.Length && SyntaxFacts.IsIdentifierPartCharacter(source[index]))
                index++;

            reservedNames.Add(source[start..index]);
            index--;
        }

        return reservedNames;
    }

    private static string NormalizeNameHint(string hint)
    {
        if (hint.Length == 0)
            return "value";

        var characters = hint.ToCharArray();
        for (var index = 0; index < characters.Length; index++)
        {
            if ((index == 0
                    ? SyntaxFacts.IsIdentifierStartCharacter(characters[index])
                    : SyntaxFacts.IsIdentifierPartCharacter(characters[index])) &&
                characters[index] != '$')
            {
                continue;
            }

            characters[index] = '_';
        }

        var normalized = new string(characters).Trim('_');
        return normalized.Length == 0 ? "value" : normalized;
    }
}
