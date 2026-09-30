namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private static readonly DiagnosticDescriptor s_invalidTargetCore = DiagnosticDescriptor.Create(
        "RAVT003", "Invalid target core configuration", "", "",
        "Target core configuration cannot be used: {0}.", "compiler", DiagnosticSeverity.Error, true);

    private static Diagnostic TargetCoreError(string reason)
        => Diagnostic.Create(s_invalidTargetCore, Location.None, reason);

    private Diagnostic? GetTargetOptionsDiagnostic()
        => _target.RuntimeContract.GetConfigurationError() is { } error ? TargetCoreError(error) : null;

    private Diagnostic? GetTargetCoreConfigurationDiagnostic()
        => _target.GetResolvedConfigurationError(this) is { } error ? TargetCoreError(error) : null;

    private bool TryResolveTargetEmitOptions(EmitOptions? requested, out EmitOptions? effective, out Diagnostic? diagnostic)
    {
        var error = _target.ResolveEmitOptions(this, requested, out effective);
        diagnostic = error is null ? null : TargetCoreError(error);
        return diagnostic is null;
    }
}
