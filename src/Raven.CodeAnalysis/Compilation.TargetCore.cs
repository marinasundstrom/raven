namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private static readonly DiagnosticDescriptor s_invalidTargetCore = DiagnosticDescriptor.Create(
        "RAVT003", "Invalid target core configuration", "", "",
        "Target core configuration cannot be used: {0}.", "compiler", DiagnosticSeverity.Error, true);

    private static Diagnostic TargetCoreError(string reason)
        => Diagnostic.Create(s_invalidTargetCore, Location.None, reason);

    private Diagnostic? GetTargetCoreConfigurationDiagnostic()
    {
        if (Options.TargetCoreAssemblyName is not { } name)
            return null;
        if (string.IsNullOrWhiteSpace(name) || Options.MetadataImportOptions?.CoreAssemblyName != name)
            return TargetCoreError("emission requires the same explicitly supplied metadata core assembly");
        return null;
    }

    private bool TryResolveTargetEmitOptions(EmitOptions? requested, out EmitOptions? effective, out Diagnostic? diagnostic)
    {
        effective = requested;
        diagnostic = GetTargetCoreConfigurationDiagnostic();
        if (diagnostic is not null)
            return false;
        if (Options.TargetCoreAssemblyName is null)
            return true;

        // Metadata setup has already resolved this assembly exclusively from the
        // supplied references. No host reflection load or runner-side selection.
        var identity = CoreAssembly.GetName();
        if (requested?.TargetCoreLibraryIdentity is { } explicitIdentity && explicitIdentity.FullName != identity.FullName)
        {
            diagnostic = TargetCoreError("explicit emission options conflict with the project's target core identity");
            return false;
        }
        effective = new EmitOptions(identity);
        return true;
    }
}
