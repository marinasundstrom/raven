using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Optionally classifies tokens produced for a token-tree macro body.
/// </summary>
public interface IMacroTokenClassifier
{
    MacroTokenClassification ClassifyToken(
        TokenTreeMacroContext context,
        SyntaxToken token);
}
