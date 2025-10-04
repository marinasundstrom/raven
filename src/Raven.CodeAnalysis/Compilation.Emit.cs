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

        new CodeGenerator(this).Emit(peStream, pdbStream);

        return new EmitResult(true, diagnostics);
    }
}
