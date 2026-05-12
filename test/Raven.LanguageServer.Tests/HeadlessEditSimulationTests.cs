using System.Diagnostics;

using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;
using Raven.LanguageServer;

using LspDiagnostic = OmniSharp.Extensions.LanguageServer.Protocol.Models.Diagnostic;
using LspDiagnosticSeverity = OmniSharp.Extensions.LanguageServer.Protocol.Models.DiagnosticSeverity;

namespace Raven.Editor.Tests;

public sealed class HeadlessEditSimulationTests : IDisposable
{
    private readonly string _tempRoot = Path.Combine(Path.GetTempPath(), $"raven-ls-headless-edit-{Guid.NewGuid():N}");

    [Fact]
    public async Task BodyEdit_ReparsesBindsHoversAndReusesUnchangedCompilationTreesAsync()
    {
        await using var simulation = HeadlessEditSimulation.Create(_tempRoot, InitialMainText);
        var initial = await simulation.CaptureSnapshotAsync();

        var updatedText = ReplaceFirst(initial.SourceText, "baseValue * 2", "baseValue * 3");
        var result = await simulation.ApplyEditAndProbeAsync(
            updatedText,
            new HeadlessHoverProbe("changed local", "answer", ExpectedText: "answer", Occurrence: 2),
            new HeadlessHoverProbe("stable sibling", "item", ExpectedText: "item", Occurrence: 2));

        result.SyntaxRootMatchesText.ShouldBeTrue();
        result.Diagnostics.ShouldNotContain(diagnostic => diagnostic.Severity == LspDiagnosticSeverity.Error);
        result.SemanticModelMaterialized.ShouldBeTrue();
        result.EditedSyntaxTreeChanged.ShouldBeTrue();
        result.UnchangedSyntaxTreeReused.ShouldBeTrue();
        result.Probes.ShouldAllBe(probe => probe.HasHover);
        result.Probes.ShouldAllBe(probe => probe.ElapsedMs < 5_000);
    }

    [Fact]
    public async Task SignatureEdit_ReparsesBindsAndReturnsCurrentHoverSymbolsAsync()
    {
        await using var simulation = HeadlessEditSimulation.Create(_tempRoot, InitialMainText);
        var initial = await simulation.CaptureSnapshotAsync();

        var updatedText = ReplaceFirst(initial.SourceText, "func Compute(value: int) -> int", "func Compute(value: int, extra: int) -> int");
        updatedText = ReplaceFirst(updatedText, "value + 1", "value + extra");
        var result = await simulation.ApplyEditAndProbeAsync(
            updatedText,
            new HeadlessHoverProbe("new parameter", "extra", ExpectedText: "extra"),
            new HeadlessHoverProbe("stable sibling", "item", ExpectedText: "item", Occurrence: 2));

        result.SyntaxRootMatchesText.ShouldBeTrue();
        result.Diagnostics.ShouldNotContain(diagnostic => diagnostic.Severity == LspDiagnosticSeverity.Error);
        result.SemanticModelMaterialized.ShouldBeTrue();
        result.EditedSyntaxTreeChanged.ShouldBeTrue();
        result.UnchangedSyntaxTreeReused.ShouldBeTrue();
        result.Probes.ShouldAllBe(probe => probe.HasHover);
        result.Probes.ShouldAllBe(probe => probe.ElapsedMs < 5_000);
    }

    private const string InitialMainText =
        """
        class Runner {
            func Compute(value: int) -> int {
                val baseValue = value + 1
                val answer = baseValue * 2
                return answer
            }

            func Stable(item: int) -> int {
                return item
            }
        }
        """;

    public void Dispose()
    {
        if (Directory.Exists(_tempRoot))
            Directory.Delete(_tempRoot, recursive: true);
    }

    private static SourceText ReplaceFirst(SourceText sourceText, string oldText, string newText)
    {
        var text = sourceText.ToString();
        var start = text.IndexOf(oldText, StringComparison.Ordinal);
        start.ShouldBeGreaterThanOrEqualTo(0);

        return sourceText.Replace(new TextSpan(start, oldText.Length), newText);
    }

    private sealed class HeadlessEditSimulation : IAsyncDisposable
    {
        private const string StableText =
            """
            class Sibling {
                func Identity(value: int) -> int {
                    return value
                }
            }
            """;

        private readonly string _mainPath;
        private readonly string _stablePath;
        private readonly DocumentUri _mainUri;
        private readonly WorkspaceManager _manager;
        private readonly DocumentStore _store;
        private readonly HoverHandler _hoverHandler;

        private HeadlessEditSimulation(
            string root,
            string mainText)
        {
            Directory.CreateDirectory(root);

            var projectPath = Path.Combine(root, "App.rvnproj");
            File.WriteAllText(projectPath, """
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net10.0</TargetFramework>
                  </PropertyGroup>
                  <ItemGroup>
                    <RavenCompile Include="src/**/*.rvn" />
                  </ItemGroup>
                </Project>
                """);

            var sourceRoot = Path.Combine(root, "src");
            Directory.CreateDirectory(sourceRoot);
            _mainPath = Path.Combine(sourceRoot, "main.rvn");
            _stablePath = Path.Combine(sourceRoot, "stable.rvn");
            File.WriteAllText(_mainPath, mainText);
            File.WriteAllText(_stablePath, StableText);

            var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
            _manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
            _manager.Initialize(new InitializeParams
            {
                WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
                {
                    Name = "headless-edit",
                    Uri = DocumentUri.FromFileSystemPath(root)
                })
            });

            _store = new DocumentStore(_manager, NullLogger<DocumentStore>.Instance);
            _hoverHandler = new HoverHandler(_store, NullLogger<HoverHandler>.Instance);
            _mainUri = DocumentUri.FromFileSystemPath(_mainPath);

            _store.UpsertDocument(_mainUri, mainText);
            _store.UpsertDocument(DocumentUri.FromFileSystemPath(_stablePath), StableText);
        }

        public static HeadlessEditSimulation Create(string root, string mainText)
            => new(root, mainText);

        public async Task<HeadlessEditSnapshot> CaptureSnapshotAsync()
        {
            var context = await _store.GetAnalysisContextAsync(_mainUri, CancellationToken.None);
            context.ShouldNotBeNull();

            var semanticModel = await _store.GetSemanticModelAsync(_mainUri, CancellationToken.None);
            semanticModel.ShouldNotBeNull();

            var compilation = context.Value.Compilation;
            return new HeadlessEditSnapshot(
                context.Value.SourceText,
                context.Value.SyntaxTree,
                GetCompilationTree(compilation, _stablePath));
        }

        public async Task<HeadlessEditResult> ApplyEditAndProbeAsync(
            SourceText updatedText,
            params HeadlessHoverProbe[] probes)
        {
            var before = await CaptureSnapshotAsync();

            _store.UpsertDocument(_mainUri, updatedText);

            var contextStopwatch = Stopwatch.StartNew();
            var context = await _store.GetAnalysisContextAsync(_mainUri, CancellationToken.None);
            contextStopwatch.Stop();
            context.ShouldNotBeNull();

            var syntaxRootMatchesText = string.Equals(
                context.Value.SyntaxTree.GetRoot().ToFullString(),
                updatedText.ToString(),
                StringComparison.Ordinal);

            var semanticStopwatch = Stopwatch.StartNew();
            var semanticModel = await _store.GetSemanticModelAsync(_mainUri, CancellationToken.None);
            semanticStopwatch.Stop();

            var diagnosticsStopwatch = Stopwatch.StartNew();
            var diagnostics = await _store.GetDiagnosticsAsync(_mainUri, CancellationToken.None);
            diagnosticsStopwatch.Stop();

            var updatedCompilation = context.Value.Compilation;
            var updatedStableTree = GetCompilationTree(updatedCompilation, _stablePath);
            var hoverResults = new List<HeadlessHoverProbeResult>(probes.Length);

            foreach (var probe in probes)
                hoverResults.Add(await RunHoverProbeAsync(context.Value.SourceText, probe));

            return new HeadlessEditResult(
                syntaxRootMatchesText,
                semanticModel is not null,
                diagnostics,
                !ReferenceEquals(before.EditedSyntaxTree, context.Value.SyntaxTree),
                ReferenceEquals(before.UnchangedSyntaxTree, updatedStableTree),
                contextStopwatch.Elapsed.TotalMilliseconds,
                semanticStopwatch.Elapsed.TotalMilliseconds,
                diagnosticsStopwatch.Elapsed.TotalMilliseconds,
                hoverResults);
        }

        private async Task<HeadlessHoverProbeResult> RunHoverProbeAsync(SourceText sourceText, HeadlessHoverProbe probe)
        {
            var text = sourceText.ToString();
            var offset = IndexOfOccurrence(text, probe.SearchText, probe.Occurrence);
            offset.ShouldBeGreaterThanOrEqualTo(0);

            var position = PositionHelper.ToRange(
                sourceText,
                new TextSpan(offset + Math.Min(probe.CharacterOffset, probe.SearchText.Length), 0)).Start;
            var context = await _store.GetAnalysisContextAsync(_mainUri, CancellationToken.None);
            context.ShouldNotBeNull();

            var before = context.Value.Compilation.PerformanceInstrumentation.SemanticQuery.CaptureSnapshot();
            var stopwatch = Stopwatch.StartNew();
            var hover = await _hoverHandler.Handle(new HoverParams
            {
                TextDocument = new TextDocumentIdentifier(_mainUri),
                Position = position
            }, CancellationToken.None);
            stopwatch.Stop();
            var after = context.Value.Compilation.PerformanceInstrumentation.SemanticQuery.CaptureSnapshot();
            var delta = SemanticQueryInstrumentation.Subtract(after, before);

            var hoverText = hover?.Contents.MarkupContent?.Value ?? string.Empty;
            hover.ShouldNotBeNull();
            hoverText.ShouldContain(probe.ExpectedText);

            return new HeadlessHoverProbeResult(
                probe.Label,
                hover is not null,
                stopwatch.Elapsed.TotalMilliseconds,
                delta);
        }

        private static SyntaxTree GetCompilationTree(Compilation compilation, string path)
            => compilation.SyntaxTrees.Single(tree =>
                string.Equals(Path.GetFullPath(tree.FilePath), Path.GetFullPath(path), StringComparison.OrdinalIgnoreCase));

        private static int IndexOfOccurrence(string text, string searchText, int occurrence)
        {
            var index = -1;
            for (var i = 0; i < occurrence; i++)
            {
                index = text.IndexOf(searchText, index + 1, StringComparison.Ordinal);
                if (index < 0)
                    return -1;
            }

            return index;
        }

        public async ValueTask DisposeAsync()
        {
            await _manager.FlushPendingMacroConsumerRefreshesAsync();
        }
    }

    private sealed record HeadlessEditSnapshot(
        SourceText SourceText,
        SyntaxTree EditedSyntaxTree,
        SyntaxTree UnchangedSyntaxTree);

    private sealed record HeadlessEditResult(
        bool SyntaxRootMatchesText,
        bool SemanticModelMaterialized,
        IReadOnlyList<LspDiagnostic> Diagnostics,
        bool EditedSyntaxTreeChanged,
        bool UnchangedSyntaxTreeReused,
        double AnalysisContextMs,
        double SemanticModelMs,
        double DiagnosticsMs,
        IReadOnlyList<HeadlessHoverProbeResult> Probes);

    private sealed record HeadlessHoverProbe(
        string Label,
        string SearchText,
        string ExpectedText,
        int Occurrence = 1,
        int CharacterOffset = 1);

    private sealed record HeadlessHoverProbeResult(
        string Label,
        bool HasHover,
        double ElapsedMs,
        SemanticQueryInstrumentation.Snapshot SemanticDelta);
}
