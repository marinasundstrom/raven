namespace Raven.CodeAnalysis;

public sealed class AsyncInvestigationOptions
{
    public static AsyncInvestigationOptions Disabled { get; } = new(false, "Step14", AsyncInvestigationPointerLabelScope.FieldOnly);

    private AsyncInvestigationOptions(bool isEnabled, string stepLabel, AsyncInvestigationPointerLabelScope pointerLabelScope)
    {
        IsEnabled = isEnabled;
        StepLabel = string.IsNullOrWhiteSpace(stepLabel) ? "Step14" : stepLabel;
        PointerLabelScope = pointerLabelScope;
    }

    public bool IsEnabled { get; }

    public string StepLabel { get; }

    public AsyncInvestigationPointerLabelScope PointerLabelScope { get; }

    public static AsyncInvestigationOptions Enable(
        string? stepLabel = null,
        AsyncInvestigationPointerLabelScope pointerLabelScope = AsyncInvestigationPointerLabelScope.FieldOnly)
        => new(true, stepLabel ?? "Step14", pointerLabelScope);

    public AsyncInvestigationOptions WithStepLabel(string? stepLabel)
        => new(IsEnabled, stepLabel ?? StepLabel, PointerLabelScope);

    public AsyncInvestigationOptions WithPointerLabelScope(AsyncInvestigationPointerLabelScope pointerLabelScope)
        => new(IsEnabled, StepLabel, pointerLabelScope);
}

public enum AsyncInvestigationPointerLabelScope
{
    FieldOnly,
    IncludeAsyncMethodName
}
