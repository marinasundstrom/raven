using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Optionally resolves macro-body tokens that denote ordinary Raven symbols.
/// </summary>
public interface IMacroTokenSymbolProvider
{
    Symbol? GetTokenSymbol(
        TokenTreeMacroContext context,
        SyntaxToken token);
}
