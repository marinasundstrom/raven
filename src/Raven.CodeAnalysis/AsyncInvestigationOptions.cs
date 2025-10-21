namespace Raven.CodeAnalysis;

public sealed class AsyncInvestigationOptions
{
    public static AsyncInvestigationOptions Disabled { get; } = new(false, "Step14");

    private AsyncInvestigationOptions(bool isEnabled, string stepLabel)
    {
        IsEnabled = isEnabled;
        StepLabel = string.IsNullOrWhiteSpace(stepLabel) ? "Step14" : stepLabel;
    }

    public bool IsEnabled { get; }

    public string StepLabel { get; }

    public static AsyncInvestigationOptions Enable(string? stepLabel = null)
        => new(true, stepLabel ?? "Step14");

    public AsyncInvestigationOptions WithStepLabel(string? stepLabel)
        => new(IsEnabled, stepLabel ?? StepLabel);
}
