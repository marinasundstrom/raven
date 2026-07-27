using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

public sealed class MacroSyntaxParseResult<TSyntax>
    where TSyntax : SyntaxNode
{
    public MacroSyntaxParseResult(
        TSyntax syntax,
        ImmutableArray<Diagnostic> diagnostics)
    {
        Syntax = syntax ?? throw new ArgumentNullException(nameof(syntax));
        Diagnostics = diagnostics.IsDefault
            ? ImmutableArray<Diagnostic>.Empty
            : diagnostics;
    }

    public TSyntax Syntax { get; }

    public ImmutableArray<Diagnostic> Diagnostics { get; }

    public bool HasErrors
        => Diagnostics.Any(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
}
