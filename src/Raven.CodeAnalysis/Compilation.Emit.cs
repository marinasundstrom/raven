using System.Collections.Immutable;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Macros;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    public EmitResult Emit(Stream peStream, Stream? pdbStream = null)
        => Emit(peStream, pdbStream, diagnostics: null, emitOptions: null);

    public EmitResult Emit(Stream peStream, Stream? pdbStream, EmitOptions emitOptions)
        => Emit(peStream, pdbStream, diagnostics: null, emitOptions);

    internal EmitResult Emit(
        Stream peStream,
        Stream? pdbStream,
        ImmutableArray<Diagnostic>? diagnostics)
        => Emit(peStream, pdbStream, diagnostics, emitOptions: null);

    internal EmitResult Emit(
        Stream peStream,
        Stream? pdbStream,
        ImmutableArray<Diagnostic>? diagnostics,
        EmitOptions? emitOptions)
    {
        if (!TryEnsureSetup(out var setupDiagnostic))
        {
            var failedDiagnostics = diagnostics ?? ImmutableArray<Diagnostic>.Empty;
            if (!failedDiagnostics.Contains(setupDiagnostic!))
                failedDiagnostics = failedDiagnostics.Add(setupDiagnostic!);
            return new EmitResult(false, failedDiagnostics);
        }
        EnsureSourceDeclarationsComplete();

        var effectiveDiagnostics = diagnostics ?? GetDiagnostics();

        if (effectiveDiagnostics.Any(x => x.Severity == DiagnosticSeverity.Error))
        {
            return new EmitResult(false, effectiveDiagnostics);
        }

        if (_macroSyntaxTrees.Length > 0 &&
            _syntaxTrees.Concat(_macroSyntaxTrees).Any(LocalMacroSyntaxClassifier.IsCompilerPluginTree))
        {
            var pluginCompilation = CreateMacroPluginCompilation();
            var pluginDiagnostics = pluginCompilation.GetDiagnostics();
            effectiveDiagnostics = effectiveDiagnostics.AddRange(pluginDiagnostics);
            if (pluginDiagnostics.Any(static diagnostic =>
                    diagnostic.Severity == DiagnosticSeverity.Error))
            {
                return new EmitResult(false, effectiveDiagnostics);
            }

            return pluginCompilation.EmitThroughTarget(peStream, pdbStream, effectiveDiagnostics, emitOptions);
        }

        return EmitThroughTarget(peStream, pdbStream, effectiveDiagnostics, emitOptions);
    }

    private EmitResult EmitThroughTarget(
        Stream output,
        Stream? debugOutput,
        ImmutableArray<Diagnostic> diagnostics,
        EmitOptions? options)
    {
        // Supplied semantic diagnostics do not establish target compatibility.
        // Validate the emitting compilation's resolved contract before entering
        // any backend, including when emitting a lowered macro-plugin compilation.
        if (GetTargetCoreConfigurationDiagnostic() is { } diagnostic)
            return new EmitResult(false, diagnostics.Add(diagnostic));

        try
        {
            var result = options?.Backend is { } backend
                ? backend.Emit(this, output, debugOutput, options)
                : _target.Emitter.Emit(output, debugOutput, options);
            return new EmitResult(result.Success, diagnostics.AddRange(result.Diagnostics));
        }
        catch (InvalidOperationException error) when (error.GetBaseException() is MissingSynthesizedRuntimeMemberException)
        {
            // The CLI code generator adds method context around body-construction errors.
            var missing = (MissingSynthesizedRuntimeMemberException)error.GetBaseException();
            return new EmitResult(false, diagnostics.Add(missing.Diagnostic));
        }
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
