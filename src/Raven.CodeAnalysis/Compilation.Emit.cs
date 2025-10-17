using System.Collections.Immutable;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.CodeGen;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    public EmitResult Emit(Stream peStream, Stream? pdbStream = null)
    {
        var diagnostics = GetDiagnostics();

        if (diagnostics.Any(x => x.Severity == DiagnosticSeverity.Error))
        {
            return new EmitResult(false, diagnostics);
        }

        ResetCodeGenerationDiagnostics();

        new CodeGenerator(this).Emit(peStream, pdbStream);

        var instrumentationDiagnostics = GetCodeGenerationDiagnostics()
            .Select(d => ApplyCompilationOptions(d))
            .Where(diagnostic => diagnostic is not null)
            .Cast<Diagnostic>()
            .ToImmutableArray();

        if (instrumentationDiagnostics.Length > 0)
            diagnostics = diagnostics.AddRange(instrumentationDiagnostics);

        return new EmitResult(true, diagnostics);
    }
}
