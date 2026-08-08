using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Describes ordinary Raven semantics resolved at a position inside a macro fragment.
/// </summary>
public sealed class MacroFragmentSemanticInfo
{
    internal MacroFragmentSemanticInfo(
        MacroFragmentRegion region,
        TextSpan span,
        SymbolInfo symbolInfo,
        TypeInfo typeInfo,
        SyntaxNode syntax)
    {
        Region = region;
        Span = span;
        SymbolInfo = symbolInfo;
        TypeInfo = typeInfo;
        Syntax = syntax;
    }

    public MacroFragmentRegion Region { get; }

    /// <summary>Gets the authored source span to highlight for this semantic result.</summary>
    public TextSpan Span { get; }

    /// <summary>Gets the symbol resolved at the authored position.</summary>
    public SymbolInfo SymbolInfo { get; }

    /// <summary>Gets the type resolved at the authored position.</summary>
    public TypeInfo TypeInfo { get; }

    internal SyntaxNode Syntax { get; }
}
