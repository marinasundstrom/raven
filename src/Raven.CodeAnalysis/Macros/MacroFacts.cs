using System;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Provides compiler-owned classification for macro definitions.
/// </summary>
public static class MacroFacts
{
    /// <summary>
    /// Gets the macro category implied by the definition's single
    /// category-specific interface.
    /// </summary>
    /// <exception cref="ArgumentException">
    /// <paramref name="macro"/> does not implement exactly one supported macro
    /// category interface.
    /// </exception>
    public static MacroKind GetKind(IMacroDefinition macro)
    {
        ArgumentNullException.ThrowIfNull(macro);
        if (TryGetKind(macro, out var kind))
            return kind;

        throw new ArgumentException(
            "A macro definition must implement exactly one supported macro category interface.",
            nameof(macro));
    }

    /// <summary>
    /// Tries to get the macro category implied by the definition's
    /// category-specific interface.
    /// </summary>
    /// <returns>
    /// <see langword="true"/> when the definition implements exactly one
    /// supported macro category interface; otherwise, <see langword="false"/>.
    /// </returns>
    public static bool TryGetKind(IMacroDefinition macro, out MacroKind kind)
    {
        ArgumentNullException.ThrowIfNull(macro);

        var isAttached = macro is IAttachedDeclarationMacro;
        var isFreestanding = macro is IFreestandingExpressionMacro;
        var isTokenTree = macro is ITokenTreeExpressionMacro;
        if ((isAttached ? 1 : 0) + (isFreestanding ? 1 : 0) + (isTokenTree ? 1 : 0) != 1)
        {
            kind = default;
            return false;
        }

        kind = isAttached
            ? MacroKind.AttachedDeclaration
            : MacroKind.FreestandingExpression;
        return true;
    }
}
