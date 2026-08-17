namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Normalized result returned by the erased macro execution ABI.
/// </summary>
public sealed class MacroExecutionResult
{
    private MacroExecutionResult(
        MacroExpansionResult? attachedResult,
        InvocableMacroExpansionResult? invocableResult)
    {
        AttachedResult = attachedResult;
        InvocableResult = invocableResult;
    }

    public MacroExpansionResult? AttachedResult { get; }

    public InvocableMacroExpansionResult? InvocableResult { get; }

    public static MacroExecutionResult Attached(MacroExpansionResult? result)
        => new(result ?? MacroExpansionResult.Empty, invocableResult: null);

    public static MacroExecutionResult Invocable(InvocableMacroExpansionResult? result)
        => new(attachedResult: null, result ?? InvocableMacroExpansionResult.Empty);
}
