using System.IO;

namespace Raven.CodeAnalysis.Targets;

// A target-owned, per-compilation emission service. Compilation completes setup,
// semantic validation, resolved target-contract validation and macro preparation
// before calling it. Implementations validate their artifact options before writing
// and return backend diagnostics only; they do not own semantic contract validation.
// Streams remain caller-owned. Stateful code generators are created per call.
internal interface ICompilationEmitter
{
    EmitResult Emit(Stream output, Stream? debugOutput, EmitOptions? options);
}
