using System.Collections.Immutable;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.CodeGen;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    public EmitResult Emit(Stream peStream, Stream? pdbStream = null)
        => Emit(peStream, pdbStream, null);

    internal EmitResult Emit(
        Stream peStream,
        Stream? pdbStream,
        ImmutableArray<Diagnostic>? diagnostics)
    {
        var effectiveDiagnostics = diagnostics ?? GetDiagnostics();

        if (effectiveDiagnostics.Any(x => x.Severity == DiagnosticSeverity.Error))
        {
            return new EmitResult(false, effectiveDiagnostics);
        }

        new CodeGenerator(this).Emit(peStream, pdbStream);

        return new EmitResult(true, effectiveDiagnostics);
    }
}
