using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Describes a completion item contributed by a token-tree macro.
/// </summary>
/// <param name="DisplayText">The text shown in the completion list.</param>
/// <param name="InsertionText">The text inserted into the authored macro body.</param>
/// <param name="BodyRelativeReplacementSpan">The body-relative span replaced by the insertion.</param>
/// <param name="CursorOffset">The optional cursor offset within <paramref name="InsertionText"/>.</param>
/// <param name="Description">Optional completion documentation.</param>
/// <param name="Symbol">An optional ordinary Raven symbol represented by the item.</param>
public sealed record MacroCompletionItem(
    string DisplayText,
    string InsertionText,
    TextSpan BodyRelativeReplacementSpan,
    int? CursorOffset = null,
    string? Description = null,
    ISymbol? Symbol = null);
