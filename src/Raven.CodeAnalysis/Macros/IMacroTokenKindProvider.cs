namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Optionally supplies stable names for macro-specific token kinds.
/// </summary>
public interface IMacroTokenKindProvider
{
    string? GetTokenKindName(int rawKind);
}
