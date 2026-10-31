using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

public class EmitResult
{
    /// <summary>Creates an emission result containing backend diagnostics.</summary>
    /// <param name="success">Whether emission completed successfully.</param>
    /// <param name="diagnostics">Diagnostics produced during emission; default is treated as empty.</param>
    public EmitResult(bool success, ImmutableArray<Diagnostic> diagnostics)
    {
        Success = success;
        Diagnostics = diagnostics.IsDefault ? [] : diagnostics;
    }

    public bool Success { get; }
    public ImmutableArray<Diagnostic> Diagnostics { get; }
}
