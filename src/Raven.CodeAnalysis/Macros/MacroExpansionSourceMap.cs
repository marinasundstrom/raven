using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Maps a span in a macro's expanded Raven syntax to the authored span in the
/// token-tree body that produced it.
/// </summary>
public readonly record struct MacroExpansionSourceMap(
    TextSpan ExpandedSpan,
    TextSpan BodyRelativeSpan);
