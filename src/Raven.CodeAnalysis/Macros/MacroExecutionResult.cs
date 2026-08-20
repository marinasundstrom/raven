namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Normalized result returned by the erased macro execution ABI.
/// </summary>
public sealed class MacroExecutionResult
{
    private MacroExecutionResult(
        MacroExpansionResult? attachedResult,
        FreestandingMacroExpansionResult? freestandingResult)
    {
        AttachedResult = attachedResult;
        FreestandingResult = freestandingResult;
    }

    public MacroExpansionResult? AttachedResult { get; }

    public FreestandingMacroExpansionResult? FreestandingResult { get; }

    public static MacroExecutionResult Attached(MacroExpansionResult? result)
        => new(result ?? MacroExpansionResult.Empty, freestandingResult: null);

    public static MacroExecutionResult Freestanding(FreestandingMacroExpansionResult? result)
        => new(attachedResult: null, result ?? FreestandingMacroExpansionResult.Empty);
}
