using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

public sealed class MacroSyntaxParseResult<TSyntax>
    where TSyntax : SyntaxNode
{
    internal MacroSyntaxParseResult(
        TSyntax syntax,
        TextSpan bodyRelativeSpan,
        int consumedBodyRelativeEnd,
        ImmutableArray<Diagnostic> diagnostics)
    {
        Syntax = syntax ?? throw new ArgumentNullException(nameof(syntax));
        BodyRelativeSpan = bodyRelativeSpan;
        ConsumedBodyRelativeEnd = consumedBodyRelativeEnd;
        Diagnostics = diagnostics.IsDefault
            ? ImmutableArray<Diagnostic>.Empty
            : diagnostics;
    }

    public TSyntax Syntax { get; }

    /// <summary>
    /// Gets the macro-body-relative span occupied by <see cref="Syntax"/>.
    /// </summary>
    public TextSpan BodyRelativeSpan { get; }

    internal int ConsumedBodyRelativeEnd { get; }

    public ImmutableArray<Diagnostic> Diagnostics { get; }

    public bool HasErrors
        => Diagnostics.Any(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
}
