using System.Diagnostics;

using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;
using Raven.LanguageServer;

using Xunit.Abstractions;

using LspDiagnostic = OmniSharp.Extensions.LanguageServer.Protocol.Models.Diagnostic;
using LspDiagnosticSeverity = OmniSharp.Extensions.LanguageServer.Protocol.Models.DiagnosticSeverity;

namespace Raven.Editor.Tests;

public sealed class HeadlessProjectEditMetricsTests : IDisposable
{
    private readonly ITestOutputHelper _output;
    private readonly string _tempRoot = Path.Combine(Path.GetTempPath(), $"raven-ls-project-edit-metrics-{Guid.NewGuid():N}");

    public HeadlessProjectEditMetricsTests(ITestOutputHelper output)
    {
        _output = output;
    }

    [Theory]
    [InlineData(2)]
    [InlineData(25)]
    public async Task BodyEdit_ReusesUnchangedTreesAcrossProjectSizesAsync(int stableFileCount)
    {
        await using var simulation = HeadlessProjectSimulation.Create(
            Path.Combine(_tempRoot, $"reuse-{stableFileCount}"),
            stableFileCount,
            runAnalyzers: false,
            _output);
        var initial = await simulation.CaptureSnapshotAsync();
        var updatedText = ReplaceFirst(initial.SourceText, "value + 1", "value + 2");

        var result = await simulation.ApplyEditAndMeasureAsync(updatedText);

        result.SyntaxRootMatchesText.ShouldBeTrue();
        result.ProjectTreeCount.ShouldBeGreaterThanOrEqualTo(stableFileCount + 1);
        result.EditedSyntaxTreeChanged.ShouldBeTrue();
        result.UnchangedTreeCount.ShouldBeGreaterThanOrEqualTo(stableFileCount);
        result.UnchangedTreeReuseCount.ShouldBe(result.UnchangedTreeCount);
        result.Diagnostics.ShouldNotContain(diagnostic => diagnostic.Severity == LspDiagnosticSeverity.Error);
        result.FirstHoverHadResult.ShouldBeTrue();
        result.FirstHoverMs.ShouldBeLessThan(5_000);
        result.DiagnosticsMs.ShouldBeLessThan(5_000);

        _output.WriteLine(
            $"projectSize={stableFileCount + 1} firstHover={result.FirstHoverMs:F1}ms context={result.AnalysisContextMs:F1}ms semantic={result.SemanticModelMs:F1}ms diagnostics={result.DiagnosticsMs:F1}ms unchangedReuse={result.UnchangedTreeReuseCount}/{result.UnchangedTreeCount}");
    }

    [Fact]
    public async Task LargeProjectBodyEdit_FirstHoverAfterEditStaysWithinInteractionBudgetAsync()
    {
        await using var simulation = HeadlessProjectSimulation.Create(
            Path.Combine(_tempRoot, "large-body-hover"),
            stableFileCount: 75,
            runAnalyzers: false,
            _output);
        var initial = await simulation.CaptureSnapshotAsync();
        var updatedText = ReplaceFirst(initial.SourceText, "value + 1", "value + 2");

        var result = await simulation.ApplyEditAndMeasureAsync(updatedText);

        _output.WriteLine(
            $"largeBody firstHover={result.FirstHoverMs:F1}ms context={result.AnalysisContextMs:F1}ms semantic={result.SemanticModelMs:F1}ms diagnostics={result.DiagnosticsMs:F1}ms");
        result.FirstHoverHadResult.ShouldBeTrue();
        result.FirstHoverMs.ShouldBeLessThan(1_000);
        result.DiagnosticsMs.ShouldBeLessThan(1_000);
    }

    [Fact]
    public async Task MediumProjectBodyEdit_DiagnosticsAfterEditStayWithinInteractionBudgetAsync()
    {
        await using var simulation = HeadlessProjectSimulation.Create(
            Path.Combine(_tempRoot, "medium-body-diagnostics"),
            stableFileCount: 25,
            runAnalyzers: false,
            _output);
        var initial = await simulation.CaptureSnapshotAsync();
        var updatedText = ReplaceFirst(initial.SourceText, "value + 1", "value + 2");

        var result = await simulation.ApplyEditAndMeasureAsync(updatedText);

        result.FirstHoverHadResult.ShouldBeTrue();
        result.FirstHoverMs.ShouldBeLessThan(1_000);
        result.DiagnosticsMs.ShouldBeLessThan(1_000);
    }

    [Theory]
    [InlineData(2)]
    [InlineData(25)]
    public async Task TopLevelEdit_ReusesUnchangedTreesAndStaysWithinInteractionBudgetAsync(int stableFileCount)
    {
        await using var simulation = HeadlessProjectSimulation.Create(
            Path.Combine(_tempRoot, $"top-level-{stableFileCount}"),
            stableFileCount,
            runAnalyzers: false,
            _output,
            mainText: TopLevelText);
        var initial = await simulation.CaptureSnapshotAsync();
        var updatedText = ReplaceFirst(initial.SourceText, "topValue + 1", "topValue + 2");

        var result = await simulation.ApplyEditAndMeasureAsync(updatedText, measureHover: false);

        result.SyntaxRootMatchesText.ShouldBeTrue();
        result.ProjectTreeCount.ShouldBeGreaterThanOrEqualTo(stableFileCount + 1);
        result.EditedSyntaxTreeChanged.ShouldBeTrue();
        result.UnchangedTreeCount.ShouldBeGreaterThanOrEqualTo(stableFileCount);
        result.UnchangedTreeReuseCount.ShouldBe(result.UnchangedTreeCount);
        var errorDiagnostics = result.Diagnostics
            .Where(static diagnostic => diagnostic.Severity == LspDiagnosticSeverity.Error)
            .Select(static diagnostic => $"{diagnostic.Code}: {diagnostic.Message}")
            .ToArray();
        errorDiagnostics.ShouldBeEmpty(string.Join(Environment.NewLine, errorDiagnostics));
        result.DiagnosticsMs.ShouldBeLessThan(5_000);

        _output.WriteLine(
            $"topLevel projectSize={stableFileCount + 1} context={result.AnalysisContextMs:F1}ms semantic={result.SemanticModelMs:F1}ms diagnostics={result.DiagnosticsMs:F1}ms unchangedReuse={result.UnchangedTreeReuseCount}/{result.UnchangedTreeCount}");
    }

    [Fact]
    public async Task LargeProjectTopLevelEdit_DiagnosticsAfterEditStayWithinInteractionBudgetAsync()
    {
        await using var simulation = HeadlessProjectSimulation.Create(
            Path.Combine(_tempRoot, "large-top-level-diagnostics"),
            stableFileCount: 75,
            runAnalyzers: false,
            _output,
            mainText: TopLevelText);
        var initial = await simulation.CaptureSnapshotAsync();
        var updatedText = ReplaceFirst(initial.SourceText, "topValue + 1", "topValue + 2");

        var result = await simulation.ApplyEditAndMeasureAsync(updatedText, measureHover: false);

        result.DiagnosticsMs.ShouldBeLessThan(1_000);
    }

    [Fact]
    public async Task TopLevelEdit_FirstLocalHoverAfterEditStaysWithinInteractionBudgetAsync()
    {
        await using var simulation = HeadlessProjectSimulation.Create(
            Path.Combine(_tempRoot, "top-level-hover"),
            stableFileCount: 25,
            runAnalyzers: false,
            _output,
            mainText: TopLevelText);
        var initial = await simulation.CaptureSnapshotAsync();
        var updatedText = ReplaceFirst(initial.SourceText, "topValue + 1", "topValue + 2");

        var result = await simulation.ApplyEditAndMeasureAsync(updatedText);

        result.FirstHoverHadResult.ShouldBeTrue();
        result.FirstHoverMs.ShouldBeLessThan(1_000);
    }

    [Fact]
    public async Task DiagnosticsMetrics_CaptureAnalyzerImpactWithRunAnalyzersToggleAsync()
    {
        await using var analyzersEnabled = HeadlessProjectSimulation.Create(
            Path.Combine(_tempRoot, "analyzers-enabled"),
            stableFileCount: 25,
            runAnalyzers: true,
            _output);
        await using var analyzersDisabled = HeadlessProjectSimulation.Create(
            Path.Combine(_tempRoot, "analyzers-disabled"),
            stableFileCount: 25,
            runAnalyzers: false,
            _output);

        var enabled = await analyzersEnabled.ComputeDiagnosticsAsync();
        var disabled = await analyzersDisabled.ComputeDiagnosticsAsync();

        enabled.Diagnostics.Any(HasUnusedVariableDiagnostic).ShouldBeTrue();
        disabled.Diagnostics.Any(HasUnusedVariableDiagnostic).ShouldBeFalse();
        enabled.Diagnostics.ShouldNotContain(diagnostic => diagnostic.Severity == LspDiagnosticSeverity.Error);
        disabled.Diagnostics.ShouldNotContain(diagnostic => diagnostic.Severity == LspDiagnosticSeverity.Error);
        enabled.DiagnosticsMs.ShouldBeLessThan(5_000);
        disabled.DiagnosticsMs.ShouldBeLessThan(5_000);

        _output.WriteLine(
            $"analyzers=on diagnostics={enabled.DiagnosticsMs:F1}ms count={enabled.Diagnostics.Count} unusedVariable={enabled.Diagnostics.Count(HasUnusedVariableDiagnostic)}");
        _output.WriteLine(
            $"analyzers=off diagnostics={disabled.DiagnosticsMs:F1}ms count={disabled.Diagnostics.Count} unusedVariable={disabled.Diagnostics.Count(HasUnusedVariableDiagnostic)}");
    }

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

    private static bool HasUnusedVariableDiagnostic(LspDiagnostic diagnostic)
        => string.Equals(diagnostic.Code?.String, UnusedVariableAnalyzer.DiagnosticId, StringComparison.Ordinal);

    private const string TopLevelText =
        """
        val topValue = 1
        val answer = topValue + 1
        answer
        """;

    private sealed class HeadlessProjectSimulation : IAsyncDisposable
    {
        private readonly string _mainPath;
        private readonly DocumentUri _mainUri;
        private readonly WorkspaceManager _manager;
        private readonly DocumentStore _store;
        private readonly HoverHandler _hoverHandler;

        private readonly string _mainText;

        private HeadlessProjectSimulation(string root, int stableFileCount, bool runAnalyzers, ITestOutputHelper output, string mainText)
        {
            _mainText = mainText;
            Directory.CreateDirectory(root);

            File.WriteAllText(Path.Combine(root, "App.rvnproj"), $"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net10.0</TargetFramework>
                    <OutputType>Exe</OutputType>
                    <RavenRunAnalyzers>{runAnalyzers.ToString().ToLowerInvariant()}</RavenRunAnalyzers>
                  </PropertyGroup>
                  <ItemGroup>
                    <RavenCompile Include="src/**/*.rvn" />
                  </ItemGroup>
                </Project>
                """);

            var sourceRoot = Path.Combine(root, "src");
            Directory.CreateDirectory(sourceRoot);

            _mainPath = Path.Combine(sourceRoot, "main.rvn");
            File.WriteAllText(_mainPath, _mainText);

            for (var i = 0; i < stableFileCount; i++)
            {
                File.WriteAllText(
                    Path.Combine(sourceRoot, $"stable{i:D3}.rvn"),
                    CreateStableText(i));
            }

            var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
            _manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
            _manager.Initialize(new InitializeParams
            {
                WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
                {
                    Name = Path.GetFileName(root),
                    Uri = DocumentUri.FromFileSystemPath(root)
                })
            });

            _store = new DocumentStore(_manager, new TestOutputLogger<DocumentStore>(output));
            _hoverHandler = new HoverHandler(_store, new TestOutputLogger<HoverHandler>(output));
            _mainUri = DocumentUri.FromFileSystemPath(_mainPath);
            _store.UpsertDocument(_mainUri, _mainText);
        }

        public static HeadlessProjectSimulation Create(string root, int stableFileCount, bool runAnalyzers, ITestOutputHelper output)
            => new(root, stableFileCount, runAnalyzers, output, MainText);

        public static HeadlessProjectSimulation Create(
            string root,
            int stableFileCount,
            bool runAnalyzers,
            ITestOutputHelper output,
            string mainText)
            => new(root, stableFileCount, runAnalyzers, output, mainText);

        public async Task<ProjectSnapshot> CaptureSnapshotAsync()
        {
            var context = await _store.GetAnalysisContextAsync(_mainUri, CancellationToken.None);
            context.ShouldNotBeNull();

            var semanticModel = await _store.GetSemanticModelAsync(_mainUri, CancellationToken.None);
            semanticModel.ShouldNotBeNull();
            _ = await _store.GetDiagnosticsAsync(_mainUri, CancellationToken.None);

            return new ProjectSnapshot(
                context.Value.SourceText,
                GetTreesByPath(context.Value.Compilation));
        }

        public async Task<ProjectEditMetrics> ApplyEditAndMeasureAsync(SourceText updatedText, bool measureHover = true)
        {
            var before = await CaptureSnapshotAsync();

            _store.UpsertDocument(_mainUri, updatedText, deferMacroConsumerRefresh: true);
            var firstHover = measureHover
                ? await RunHoverProbeAsync(updatedText)
                : new HoverProbeMetrics(0, HasHover: true);

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
            semanticModel.ShouldNotBeNull();

            var diagnostics = await ComputeDiagnosticsAsync(DocumentStore.DocumentDiagnosticsMode.Document);
            var afterTrees = GetTreesByPath(context.Value.Compilation);
            var mainPath = Path.GetFullPath(_mainPath);

            var unchangedTreeCount = 0;
            var unchangedTreeReuseCount = 0;
            foreach (var (path, beforeTree) in before.TreesByPath)
            {
                if (string.Equals(path, mainPath, StringComparison.OrdinalIgnoreCase))
                    continue;

                unchangedTreeCount++;
                if (afterTrees.TryGetValue(path, out var afterTree) && ReferenceEquals(beforeTree, afterTree))
                    unchangedTreeReuseCount++;
            }

            var editedSyntaxTreeChanged = before.TreesByPath.TryGetValue(mainPath, out var beforeMainTree) &&
                                          afterTrees.TryGetValue(mainPath, out var afterMainTree) &&
                                          !ReferenceEquals(beforeMainTree, afterMainTree);

            return new ProjectEditMetrics(
                syntaxRootMatchesText,
                afterTrees.Count,
                editedSyntaxTreeChanged,
                unchangedTreeCount,
                unchangedTreeReuseCount,
                firstHover.ElapsedMs,
                firstHover.HasHover,
                contextStopwatch.Elapsed.TotalMilliseconds,
                semanticStopwatch.Elapsed.TotalMilliseconds,
                diagnostics.DiagnosticsMs,
                diagnostics.Diagnostics);
        }

        private async Task<HoverProbeMetrics> RunHoverProbeAsync(SourceText sourceText)
        {
            var text = sourceText.ToString();
            var offset = IndexOfOccurrence(text, "answer", occurrence: 2);
            offset.ShouldBeGreaterThanOrEqualTo(0);

            var position = PositionHelper.ToRange(
                sourceText,
                new TextSpan(offset + 1, 0)).Start;

            var stopwatch = Stopwatch.StartNew();
            var hover = await _hoverHandler.Handle(new HoverParams
            {
                TextDocument = new TextDocumentIdentifier(_mainUri),
                Position = position
            }, CancellationToken.None);
            stopwatch.Stop();

            var hoverText = hover?.Contents.MarkupContent?.Value ?? string.Empty;
            hover.ShouldNotBeNull();
            hoverText.ShouldContain("answer");

            return new HoverProbeMetrics(stopwatch.Elapsed.TotalMilliseconds, hover is not null);
        }

        public async Task<ProjectDiagnosticsMetrics> ComputeDiagnosticsAsync(
            DocumentStore.DocumentDiagnosticsMode mode = DocumentStore.DocumentDiagnosticsMode.Full)
        {
            var stopwatch = Stopwatch.StartNew();
            var result = await _store.TryGetDiagnosticsAsync(
                _mainUri,
                mode,
                shouldSkipWork: null,
                CancellationToken.None);
            stopwatch.Stop();
            result.WasSkipped.ShouldBeFalse();

            return new ProjectDiagnosticsMetrics(
                stopwatch.Elapsed.TotalMilliseconds,
                result.Diagnostics);
        }

        public async ValueTask DisposeAsync()
        {
            await _manager.FlushPendingMacroConsumerRefreshesAsync();
        }

        private static Dictionary<string, SyntaxTree> GetTreesByPath(Compilation compilation)
            => compilation.SyntaxTrees
                .Where(static tree => !string.IsNullOrWhiteSpace(tree.FilePath))
                .GroupBy(static tree => Path.GetFullPath(tree.FilePath), StringComparer.OrdinalIgnoreCase)
                .ToDictionary(static group => group.Key, static group => group.First(), StringComparer.OrdinalIgnoreCase);

        private static string CreateStableText(int index)
            => $$"""
                class Stable{{index}} {
                    func Identity(value: int) -> int {
                        return value + {{index + 1}}
                    }
                }
                """;

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

        private const string MainText =
            """
            class Runner {
                func Compute(value: int) -> int {
                    val unusedValue = value
                    val answer = value + 1
                    return answer
                }
            }
            """;
    }

    private sealed record ProjectSnapshot(
        SourceText SourceText,
        IReadOnlyDictionary<string, SyntaxTree> TreesByPath);

    private sealed record ProjectEditMetrics(
        bool SyntaxRootMatchesText,
        int ProjectTreeCount,
        bool EditedSyntaxTreeChanged,
        int UnchangedTreeCount,
        int UnchangedTreeReuseCount,
        double FirstHoverMs,
        bool FirstHoverHadResult,
        double AnalysisContextMs,
        double SemanticModelMs,
        double DiagnosticsMs,
        IReadOnlyList<LspDiagnostic> Diagnostics);

    private sealed record HoverProbeMetrics(
        double ElapsedMs,
        bool HasHover);

    private sealed record ProjectDiagnosticsMetrics(
        double DiagnosticsMs,
        IReadOnlyList<LspDiagnostic> Diagnostics);

    private sealed class TestOutputLogger<T> : ILogger<T>
    {
        private readonly ITestOutputHelper _output;

        public TestOutputLogger(ITestOutputHelper output)
        {
            _output = output;
        }

        public IDisposable BeginScope<TState>(TState state)
            where TState : notnull
            => NullScope.Instance;

        public bool IsEnabled(LogLevel logLevel)
            => logLevel >= LogLevel.Information;

        public void Log<TState>(
            LogLevel logLevel,
            EventId eventId,
            TState state,
            Exception? exception,
            Func<TState, Exception?, string> formatter)
        {
            if (!IsEnabled(logLevel))
                return;

            _output.WriteLine($"{typeof(T).Name} {logLevel}: {formatter(state, exception)}");
        }

        private sealed class NullScope : IDisposable
        {
            public static readonly NullScope Instance = new();

            public void Dispose()
            {
            }
        }
    }
}
