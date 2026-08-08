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
        string? kindName,
        MacroTokenClassification classification)
    {
        Token = token;
        BodyRelativeSpan = bodyRelativeSpan;
        Span = span;
        KindName = kindName;
        Classification = classification;
    }

    public SyntaxToken Token { get; }

    public int RawKind => Token.RawKind;

    public string Text => Token.Text;

    /// <summary>
    /// Gets the provider-defined token kind name, or the Raven syntax kind name
    /// when the token uses a standard Raven kind.
    /// </summary>
    public string? KindName { get; }

    public TextSpan BodyRelativeSpan { get; }

    public TextSpan Span { get; }

    public MacroTokenClassification Classification { get; }
}
