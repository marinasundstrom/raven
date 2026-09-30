namespace Raven.CodeAnalysis.Targets;

// Shared diagnostic identity for target configuration and backend compatibility.
internal static class TargetDiagnostics
{
    private static readonly DiagnosticDescriptor s_unsupportedTargetPlatform = DiagnosticDescriptor.Create(
        "RAVT005", "Unsupported target platform", "", "",
        "Target platform '{0}' is not supported by this compiler.", "compiler", DiagnosticSeverity.Error, true);

    private static readonly DiagnosticDescriptor s_invalidTargetCore = DiagnosticDescriptor.Create(
        "RAVT003", "Invalid target core configuration", "", "",
        "Target core configuration cannot be used: {0}.", "compiler", DiagnosticSeverity.Error, true);

    internal static Diagnostic? InvalidConfiguration(string? error)
        => error is null ? null : Diagnostic.Create(s_invalidTargetCore, Location.None, error);

    internal static Diagnostic UnsupportedPlatform(TargetPlatform platform)
        => Diagnostic.Create(s_unsupportedTargetPlatform, Location.None, platform);
}
