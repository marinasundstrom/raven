namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Optionally projects a token-tree macro body as a position-preserving
/// document in an embedded language.
/// </summary>
public interface IMacroEmbeddedLanguageProvider
{
    MacroEmbeddedLanguageProjection? GetEmbeddedLanguageProjection(
        TokenTreeMacroContext context);
}
