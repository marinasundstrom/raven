using System.Collections.Immutable;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.CodeGen;
using Raven.CodeAnalysis.Macros;

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
        EnsureSetup();
        EnsureSourceDeclarationsComplete();

        var effectiveDiagnostics = diagnostics ?? GetDiagnostics();

        if (effectiveDiagnostics.Any(x => x.Severity == DiagnosticSeverity.Error))
        {
            return new EmitResult(false, effectiveDiagnostics);
        }

        if (_macroSyntaxTrees.Length > 0 &&
            _syntaxTrees.Any(LocalMacroSyntaxClassifier.IsCompilerPluginTree))
        {
            var pluginCompilation = CreateMacroPluginCompilation();
            var pluginDiagnostics = pluginCompilation.GetDiagnostics();
            effectiveDiagnostics = effectiveDiagnostics.AddRange(pluginDiagnostics);
            if (pluginDiagnostics.Any(static diagnostic =>
                    diagnostic.Severity == DiagnosticSeverity.Error))
            {
                return new EmitResult(false, effectiveDiagnostics);
            }

            new CodeGenerator(pluginCompilation).Emit(peStream, pdbStream);
            return new EmitResult(true, effectiveDiagnostics);
        }

        new CodeGenerator(this).Emit(peStream, pdbStream);

        return new EmitResult(true, effectiveDiagnostics);
    }

    private Compilation CreateMacroPluginCompilation()
    {
        var references = EnsureMacroContractsReference(_references);
        var signatureCompilation = new Compilation(
            $"{AssemblyName}.MacroSignatures",
            _macroSyntaxTrees,
            [],
            references,
            _macroReferences,
            Options.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var loweredMacroTrees = _macroSyntaxTrees
            .Select(tree => MacroLowering.Lower(
                tree,
                signatureCompilation.GetSemanticModel(tree)))
            .ToArray();

        return new Compilation(
            AssemblyName,
            _syntaxTrees.Concat(loweredMacroTrees).ToArray(),
            [],
            references,
            _macroReferences,
            Options.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
    }
}
