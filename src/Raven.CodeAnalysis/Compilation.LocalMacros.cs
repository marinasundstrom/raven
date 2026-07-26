using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Macros;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private MacroReference? CompileLocalMacroPartition()
    {
        if (_macroSyntaxTrees.Length == 0)
            return null;

        var references = _references;
        var contractsAssemblyPath = typeof(IRavenMacroPlugin).Assembly.Location;
        if (!string.IsNullOrWhiteSpace(contractsAssemblyPath) &&
            !references
                .OfType<PortableExecutableReference>()
                .Any(reference => string.Equals(
                    reference.FilePath,
                    contractsAssemblyPath,
                    StringComparison.OrdinalIgnoreCase)))
        {
            references =
            [
                .. references,
                MetadataReference.CreateFromFile(contractsAssemblyPath)
            ];
        }

        var macroCompilation = new Compilation(
            $"{AssemblyName}.Macros",
            _macroSyntaxTrees,
            [],
            references,
            _macroReferences,
            Options.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));

        using var image = new MemoryStream();
        var emitResult = macroCompilation.Emit(image);
        _macroPartitionDiagnostics = emitResult.Diagnostics;
        if (!emitResult.Success)
            return null;

        return MacroReference.CreateFromImage(
            image.ToArray(),
            display: $"{AssemblyName} (local macro partition)");
    }
}
