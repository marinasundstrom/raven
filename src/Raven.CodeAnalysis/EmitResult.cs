using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

public class EmitResult
{
    internal EmitResult(bool success, ImmutableArray<Diagnostic> diagnostics)
    {
        Success = success;
        Diagnostics = diagnostics;
    }

    public bool Success { get; }
    public ImmutableArray<Diagnostic> Diagnostics { get; }
}