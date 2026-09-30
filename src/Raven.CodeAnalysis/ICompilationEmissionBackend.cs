using System.IO;

namespace Raven.CodeAnalysis;

/// <summary>Writes an artifact from a compilation prepared and validated by Compilation.Emit.</summary>
/// <remarks>
/// Select through EmitOptions.WithBackend. Selection changes artifact emission, not binding or
/// Runtime Contract configuration. Implementations must be immutable/reusable and create builder
/// state per call. Compilation owns setup, semantic diagnostics, macro preparation and resolved
/// target-contract checks. Return backend diagnostics only; do not call Compilation.Emit recursively.
/// Validate backend capabilities before writing. Streams remain caller-owned; I/O errors may
/// propagate. Implementations must explicitly reject unsupported debug output and artifact options.
/// </remarks>
public interface ICompilationEmissionBackend
{
    /// <summary>Emits the prepared compilation using this backend's builders and encoding.</summary>
    /// <param name="compilation">The validated compilation, possibly prepared from macro source.</param>
    /// <param name="output">Caller-owned artifact stream.</param>
    /// <param name="debugOutput">Optional caller-owned debug stream.</param>
    /// <param name="options">Artifact options selecting this backend.</param>
    /// <returns>Success and backend diagnostics, without repeating compiler diagnostics.</returns>
    EmitResult Emit(Compilation compilation, Stream output, Stream? debugOutput, EmitOptions options);
}
