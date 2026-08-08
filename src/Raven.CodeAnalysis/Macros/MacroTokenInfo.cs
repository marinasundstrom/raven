using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Describes a token surfaced from a token-tree macro body.
/// </summary>
public sealed class MacroTokenInfo
{
    internal MacroTokenInfo(
        SyntaxToken token,
        TextSpan bodyRelativeSpan,
        TextSpan span,
        MacroTokenClassification classification)
    {
        Token = token;
        BodyRelativeSpan = bodyRelativeSpan;
        Span = span;
        Classification = classification;
    }

    public SyntaxToken Token { get; }

    public int RawKind => Token.RawKind;

    public string Text => Token.Text;

    public TextSpan BodyRelativeSpan { get; }

    public TextSpan Span { get; }

    public MacroTokenClassification Classification { get; }
}
