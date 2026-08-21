using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Optionally provides completion items for positions owned by a token-tree macro DSL.
/// </summary>
public interface IMacroCompletionProvider
{
    /// <summary>
    /// Gets completion items at a position relative to the start of the macro body.
    /// </summary>
    ImmutableArray<MacroCompletionItem> GetCompletions(
        TokenTreeMacroContext context,
        int bodyRelativePosition);
}
