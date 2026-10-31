namespace Raven.CodeAnalysis;

/// <summary>Selects the synchronous scoped-resource interface and its cleanup policy.</summary>
/// <param name="AssemblyName">Assembly owning the interface.</param>
/// <param name="InterfaceTypeName">Metadata name of a public interface with a parameterless Dispose method returning unit.</param>
/// <param name="UseExceptionHandling">Whether cleanup uses exception-safe finally regions; otherwise only ordinary scope exits clean up.</param>
/// <param name="AllowAsyncScopeExitCleanup">Whether the selected backend retains ordinary scopes across async suspension and lowers all completion exits through synchronous cleanup. Does not enable iterator or fault unwinding.</param>
public sealed record RuntimeDisposalContract(string AssemblyName, string InterfaceTypeName, bool UseExceptionHandling = true,
    bool AllowAsyncScopeExitCleanup = false);
