using Raven.CodeAnalysis.Targets;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private static readonly DiagnosticDescriptor s_targetInitializationFailed = DiagnosticDescriptor.Create(
        "RAVT004", "Target initialization failed", "", "",
        "Cannot initialize compilation target: {0}", "compiler", DiagnosticSeverity.Error, true);

    private bool TryEnsureSetup(out Diagnostic? diagnostic)
    {
        diagnostic = GetTargetOptionsDiagnostic();
        if (diagnostic is not null)
            return false;

        try
        {
            EnsureSetup();
            diagnostic = null;
            return true;
        }
        catch (TargetInitializationException exception)
        {
            // Failed setup is not marked complete or shared with another snapshot.
            // Do not suppress/downgrade this fatal error and continue with partial state.
            diagnostic = Diagnostic.Create(s_targetInitializationFailed, Location.None, exception.Message);
            return false;
        }
    }
}
