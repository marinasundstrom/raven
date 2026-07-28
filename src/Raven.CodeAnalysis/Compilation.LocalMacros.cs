using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private static readonly DiagnosticDescriptor s_localMacroDependencyCycle = DiagnosticDescriptor.Create(
        "RAVM003",
        "Local macro dependency cycle",
        "Local macro implementations are compiled before consumer declarations.",
        string.Empty,
        "Local macro code cannot reference consumer declaration '{0}' because consumer binding depends on local macro activation. Move the dependency into the local macro partition or a referenced assembly.",
        "Macros",
        DiagnosticSeverity.Error);

    private Compilation? _macroPartitionCompilation;
    private Compilation? _macroSignatureCompilation;
    private LocalMacroPartitionArtifact? _localMacroPartitionArtifact;
    private bool _hasReusedLocalMacroPartitionArtifact;

    private MacroReference? CompileLocalMacroPartition()
    {
        if (_macroSyntaxTrees.Length == 0)
            return null;

        var references = EnsureMacroContractsReference(_references);
        _macroSignatureCompilation = new Compilation(
            $"{AssemblyName}.MacroSignatures",
            _macroSyntaxTrees,
            [],
            references,
            _macroReferences,
            Options.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var loweredMacroTrees = _macroSyntaxTrees
            .Select(tree => MacroFunctionLowering.Lower(
                tree,
                _macroSignatureCompilation.GetSemanticModel(tree)))
            .ToArray();
        _macroPartitionCompilation = new Compilation(
            $"{AssemblyName}.Macros",
            loweredMacroTrees,
            [],
            references,
            _macroReferences,
            Options.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));

        if (_localMacroPartitionArtifact is { } reusedArtifact)
        {
            _macroPartitionDiagnostics = RemapLocalMacroDiagnostics(reusedArtifact);
            if (_hasReusedLocalMacroPartitionArtifact)
                PerformanceInstrumentation.Macros.RecordLocalPartitionReuse();
            return reusedArtifact.Reference;
        }

        using var image = new MemoryStream();
        var emitResult = _macroPartitionCompilation.Emit(image);
        _macroPartitionDiagnostics = RewriteLocalMacroDependencyCycles(emitResult.Diagnostics);
        PerformanceInstrumentation.Macros.RecordLocalPartitionCompilation();

        var reference = emitResult.Success
            ? MacroReference.CreateFromImage(
                image.ToArray(),
                display: $"{AssemblyName} (local macro partition)")
            : null;
        _localMacroPartitionArtifact = new LocalMacroPartitionArtifact(
            reference,
            _macroPartitionDiagnostics,
            loweredMacroTrees);
        return reference;
    }

    private ImmutableArray<Diagnostic> RewriteLocalMacroDependencyCycles(
        ImmutableArray<Diagnostic> diagnostics)
    {
        if (_syntaxTrees.Length == 0 ||
            !diagnostics.Any(static diagnostic =>
                diagnostic.Id == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id))
        {
            return diagnostics;
        }

        var probeCompilation = new Compilation(
            $"{AssemblyName}.MacroDependencyProbe",
            _macroSyntaxTrees.Concat(_syntaxTrees).ToArray(),
            [],
            EnsureMacroContractsReference(_references),
            _macroReferences,
            Options.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var unresolvedProbeLocations = _macroSyntaxTrees
            .SelectMany(tree => probeCompilation.GetDiagnostics(tree))
            .Where(static diagnostic =>
                diagnostic.Id == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id)
            .Select(static diagnostic => new LocalMacroDiagnosticLocation(
                diagnostic.Location.SourceTree,
                diagnostic.Location.SourceSpan))
            .ToHashSet();

        var builder = ImmutableArray.CreateBuilder<Diagnostic>(diagnostics.Length);
        foreach (var diagnostic in diagnostics)
        {
            if (diagnostic.Id == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id &&
                diagnostic.Location.SourceTree is not null &&
                !unresolvedProbeLocations.Contains(new LocalMacroDiagnosticLocation(
                    diagnostic.Location.SourceTree,
                    diagnostic.Location.SourceSpan)))
            {
                builder.Add(Diagnostic.Create(
                    s_localMacroDependencyCycle,
                    diagnostic.Location,
                    diagnostic.GetMessageArgs()));
            }
            else
            {
                builder.Add(diagnostic);
            }
        }

        return builder.ToImmutable();
    }

    internal void TryReuseLocalMacroPartitionFrom(Compilation previousCompilation)
    {
        if (previousCompilation._localMacroPartitionArtifact is not { } artifact ||
            !HasEquivalentLocalMacroPartition(previousCompilation))
        {
            return;
        }

        _localMacroPartitionArtifact = artifact;
        _hasReusedLocalMacroPartitionArtifact = true;
    }

    private bool HasEquivalentLocalMacroPartition(Compilation previousCompilation)
    {
        if (_macroSyntaxTrees.Length == 0 ||
            _macroSyntaxTrees.Length != previousCompilation._macroSyntaxTrees.Length ||
            !string.Equals(AssemblyName, previousCompilation.AssemblyName, StringComparison.Ordinal) ||
            !ReferenceEquals(Options, previousCompilation.Options) ||
            !_references.SequenceEqual(previousCompilation._references) ||
            !_macroReferences.SequenceEqual(previousCompilation._macroReferences))
        {
            return false;
        }

        for (var index = 0; index < _macroSyntaxTrees.Length; index++)
        {
            if (!HasEquivalentMacroTree(
                    _macroSyntaxTrees[index],
                    previousCompilation._macroSyntaxTrees[index]))
            {
                return false;
            }
        }

        return true;
    }

    private static bool HasEquivalentMacroTree(SyntaxTree current, SyntaxTree previous)
    {
        if (ReferenceEquals(current, previous))
            return true;

        if (!string.Equals(current.FilePath, previous.FilePath, StringComparison.OrdinalIgnoreCase) ||
            !current.Options.IsEquivalentTo(previous.Options))
        {
            return false;
        }

        return GetMacroTreeSegments(current).SequenceEqual(GetMacroTreeSegments(previous));
    }

    private static ImmutableArray<LocalMacroTreeSegment> GetMacroTreeSegments(SyntaxTree syntaxTree)
    {
        var root = syntaxTree.GetRoot();
        var builder = ImmutableArray.CreateBuilder<LocalMacroTreeSegment>();

        AddSegments(root.Imports);
        AddSegments(root.Aliases);
        AddSegments(root.AttributeLists);
        AddSegments(root.Members);
        return builder.ToImmutable();

        void AddSegments<TNode>(SyntaxList<TNode> nodes)
            where TNode : SyntaxNode
        {
            foreach (var node in nodes)
                builder.Add(new LocalMacroTreeSegment(node.FullSpan.Start, node.ToFullString()));
        }
    }

    private ImmutableArray<Diagnostic> RemapLocalMacroDiagnostics(
        LocalMacroPartitionArtifact artifact)
    {
        if (artifact.Diagnostics.IsDefaultOrEmpty)
            return artifact.Diagnostics;

        var builder = ImmutableArray.CreateBuilder<Diagnostic>(artifact.Diagnostics.Length);
        foreach (var diagnostic in artifact.Diagnostics)
        {
            var location = diagnostic.Location;
            if (location.SourceTree is { } sourceTree)
            {
                var treeIndex = Array.IndexOf(artifact.SyntaxTrees, sourceTree);
                if (treeIndex >= 0 && treeIndex < _macroSyntaxTrees.Length)
                    location = _macroSyntaxTrees[treeIndex].GetLocation(location.SourceSpan);
            }

            builder.Add(new Diagnostic(
                diagnostic.Descriptor,
                location,
                diagnostic.GetMessageArgs(),
                diagnostic.Severity,
                diagnostic.IsSuppressed,
                diagnostic.Properties));
        }

        return builder.ToImmutable();
    }

    private static MetadataReference[] EnsureMacroContractsReference(
        MetadataReference[] references)
    {
        var contractsAssemblyPath = typeof(IMacroDefinition).Assembly.Location;
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

        return references;
    }

    private sealed record LocalMacroPartitionArtifact(
        MacroReference? Reference,
        ImmutableArray<Diagnostic> Diagnostics,
        SyntaxTree[] SyntaxTrees);

    private readonly record struct LocalMacroTreeSegment(int Start, string Text);

    private readonly record struct LocalMacroDiagnosticLocation(
        SyntaxTree? SyntaxTree,
        TextSpan Span);
}
