namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private Diagnostic? GetTargetOptionsDiagnostic()
        => _target.GetConfigurationDiagnostic();

    private Diagnostic? GetTargetCoreConfigurationDiagnostic()
        => _target.GetResolvedConfigurationDiagnostic();
}
