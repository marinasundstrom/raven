using System.IO;

using Raven.CodeAnalysis.CodeGen;

namespace Raven.CodeAnalysis.Targets;

internal sealed class DotNetCompilationEmitter(Compilation compilation, DotNetCompilationTarget target) : ICompilationEmitter
{
    public EmitResult Emit(Stream output, Stream? debugOutput, EmitOptions? options)
    {
        if (target.ResolveEmitOptions(options, out var effectiveOptions) is { } diagnostic)
            return new EmitResult(false, [diagnostic]);

        new CodeGenerator(compilation, effectiveOptions).Emit(output, debugOutput);
        return new EmitResult(true, []);
    }
}
