using System.Collections.Immutable;
using System.Diagnostics;
using System.Globalization;
using System.Text.RegularExpressions;

using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;
using Raven.LanguageServer;

using LspDiagnosticSeverity = OmniSharp.Extensions.LanguageServer.Protocol.Models.DiagnosticSeverity;

var repoRoot = FindRepositoryRoot();
CultureInfo.CurrentCulture = CultureInfo.InvariantCulture;
CultureInfo.CurrentUICulture = CultureInfo.InvariantCulture;
var options = ParseOptions(args);
if (options.ListScenarios)
{
    foreach (var replayScenario in CreateReplayScenarios(repoRoot))
        Console.WriteLine($"{replayScenario.Name}: {replayScenario.Description}");

    return;
}

if (options.StressSuite)
{
    await RunStressSuiteAsync();
    return;
}

if (options.EditScenarioSuite)
{
    await RunEditScenarioSuiteAsync();
    return;
}

var scenario = options.ScenarioName is { Length: > 0 }
    ? ResolveReplayScenario(repoRoot, options.ScenarioName)
    : null;
var projectRoot = scenario?.ProjectRoot ??
    (options.Positionals.Count > 0
    ? Path.GetFullPath(options.Positionals[0])
    : Path.Combine(repoRoot, "samples", "projects", "efcore-vehicle-costs"));
var filePath = scenario?.FilePath ??
    (options.Positionals.Count > 1
    ? Path.GetFullPath(options.Positionals[1])
    : Path.Combine(projectRoot, "src", "Api", "Main.rvn"));

var text = File.ReadAllText(filePath);
var uri = DocumentUri.FromFileSystemPath(filePath);

var workspace = RavenWorkspace.Create(
    targetFramework: "net10.0",
    workspaceEventSink: new HeadlessWorkspaceEventSink());
var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
manager.Initialize(new InitializeParams
{
    WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
    {
        Name = Path.GetFileName(projectRoot),
        Uri = DocumentUri.FromFileSystemPath(projectRoot)
    })
});

var store = new DocumentStore(manager, new HeadlessConsoleLogger<DocumentStore>());
_ = store.UpsertDocument(uri, text);
var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None)
    ?? throw new InvalidOperationException($"No analysis context for '{filePath}'.");
var sourceText = context.SourceText;
var handler = new HoverHandler(store, new HeadlessConsoleLogger<HoverHandler>());
var inlayHandler = new InlayHintHandler(store, new HeadlessConsoleLogger<InlayHintHandler>());
var completionHandler = new CompletionHandler(store, new HeadlessConsoleLogger<CompletionHandler>());
var model = await store.GetSemanticModelAsync(uri, CancellationToken.None)
    ?? throw new InvalidOperationException($"No semantic model for '{filePath}'.");
var root = context.SyntaxTree.GetRoot();

if (scenario is not null)
{
    await RunReplayScenarioAsync(scenario);
    return;
}

if (options.ProjectSequence)
{
    await RunProjectSequenceAsync();
    return;
}

if (options.RandomHover)
{
    await RunRandomHoversAsync();
    return;
}

if (options.EditReplacements.Count > 0)
{
    await RunEditProbeAsync();
    return;
}

if (options.TypeImportText is { Length: > 0 })
{
    await RunTypeImportProbeAsync(options.TypeImportText);
    return;
}

if (options.ReplayPerformanceReport)
{
    await ReplayPerformanceReportAsync();
    return;
}

if (options.HoverPositions.Count > 0)
{
    foreach (var targetPosition in options.HoverPositions)
    {
        var hoverResult = await RunHoverAsync(targetPosition.Label, new Position(targetPosition.Line, targetPosition.Character));
        Console.WriteLine(FormatHoverResult(hoverResult));
    }

    return;
}

if (options.InlayRange is { } inlayRange)
{
    for (var i = 0; i < options.InlayRepeatCount; i++)
    {
        var result = await RunInlayAsync(inlayRange);
        Console.WriteLine(result);
    }

    if (options.PrintDiagnostics)
    {
        await RunDocumentDiagnosticsProbeAsync("after-inlay");
        await RunDocumentDiagnosticsProbeAsync("after-inlay-with-analyzers", DocumentStore.DiagnosticLane.DocumentWithAnalyzers);
    }

    return;
}

var hoverTargets = options.Positionals.Count > 2
    ? options.Positionals.Skip(2)
    : ["CreateBuilder", "AddDbContext", "UseNpgsql"];

foreach (var target in hoverTargets)
{
    var offset = text.IndexOf(target, StringComparison.Ordinal);
    if (offset < 0)
        continue;

    if (Environment.GetEnvironmentVariable("RAVEN_HEADLESS_DEBUG_ANCESTORS") == "1")
    {
        var token = root.FindToken(offset + Math.Min(3, target.Length));
        Console.WriteLine($"{target} token: {token.Kind} '{token.ValueText}'");
        foreach (var ancestor in token.Parent?.AncestorsAndSelf() ?? [])
            Console.WriteLine($"  {ancestor.Kind} {ancestor.GetType().Name} {ancestor.Span}");
    }

    var position = PositionHelper.ToRange(sourceText, new TextSpan(offset + Math.Min(3, target.Length), 0)).Start;
    var hoverResult = await RunHoverAsync(target, position);
    Console.WriteLine(FormatHoverResult(hoverResult));
}

foreach (var invocation in root.DescendantNodes().OfType<InvocationExpressionSyntax>())
{
    if (invocation.Expression is not MemberAccessExpressionSyntax { Name: SimpleNameSyntax name })
        continue;

    var before = context.Compilation.PerformanceInstrumentation.SemanticQuery.CaptureSnapshot();
    var sw = Stopwatch.StartNew();
    var symbolInfo = model.GetSymbolInfo(invocation);
    var candidates = symbolInfo.CandidateSymbols.OfType<IMethodSymbol>().ToImmutableArray();
    if (symbolInfo.Symbol is IMethodSymbol method)
        candidates = candidates.Insert(0, method);

    var hasCandidates = candidates.Length > 0;
    sw.Stop();
    var after = context.Compilation.PerformanceInstrumentation.SemanticQuery.CaptureSnapshot();
    var delta = SemanticQueryInstrumentation.Subtract(after, before);

    Console.WriteLine(
        $"candidates {name.Identifier.ValueText}: {sw.Elapsed.TotalMilliseconds:F1}ms has={hasCandidates} count={candidates.Length} [{SemanticQueryInstrumentation.FormatDelta(delta)}]");
}

async Task RunProjectSequenceAsync()
{
    var sequenceFiles = options.SequenceFiles.Count > 0
        ? options.SequenceFiles.Select(ResolveSequenceFile).ToArray()
        : Directory.Exists(Path.Combine(projectRoot, "src"))
            ? Directory.GetFiles(Path.Combine(projectRoot, "src"), "*.rvn", SearchOption.AllDirectories)
                .OrderBy(static path => path, StringComparer.OrdinalIgnoreCase)
                .ToArray()
            : Directory.GetFiles(projectRoot, "*.rvn", SearchOption.AllDirectories)
                .Where(static path =>
                    !path.Contains($"{Path.DirectorySeparatorChar}obj{Path.DirectorySeparatorChar}", StringComparison.OrdinalIgnoreCase) &&
                    !path.Contains($"{Path.DirectorySeparatorChar}tmp-", StringComparison.OrdinalIgnoreCase))
                .OrderBy(static path => path, StringComparer.OrdinalIgnoreCase)
                .ToArray();

    if (sequenceFiles.Length == 0)
    {
        Console.WriteLine($"project-sequence project={projectRoot} files=0");
        return;
    }

    var expandedSequence = Enumerable.Range(0, options.SequenceRepeatCount)
        .SelectMany(_ => sequenceFiles)
        .Concat(sequenceFiles.Take(1))
        .ToArray();

    Console.WriteLine(
        $"project-sequence project={projectRoot} files={sequenceFiles.Length} steps={expandedSequence.Length} repeat={options.SequenceRepeatCount}");

    var openedDocuments = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
    {
        Path.GetFullPath(filePath)
    };

    foreach (var sequenceFile in expandedSequence)
    {
        var normalizedSequenceFile = Path.GetFullPath(sequenceFile);
        var upsertDocument = options.SequenceReopenDocuments || openedDocuments.Add(normalizedSequenceFile);
        var loadMs = await SwitchDocumentAsync(sequenceFile, upsertDocument);
        var lineCount = sourceText.GetLineCount();

        var documentDiagnosticsStopwatch = Stopwatch.StartNew();
        var documentDiagnostics = await store.TryGetDiagnosticsAsync(
            uri,
            DocumentStore.DiagnosticLane.DocumentCompiler,
            shouldSkipWork: null,
            CancellationToken.None);
        documentDiagnosticsStopwatch.Stop();
        var errorCount = documentDiagnostics.Diagnostics.Count(static diagnostic => diagnostic.Severity == LspDiagnosticSeverity.Error);

        Console.WriteLine(
            $"project-sequence file={Path.GetRelativePath(projectRoot, filePath)} " +
            $"mode={(upsertDocument ? "open" : "switch")} " +
            $"load={loadMs:F1}ms documentDiagnostics={documentDiagnosticsStopwatch.Elapsed.TotalMilliseconds:F1}ms " +
            $"diagnostics={documentDiagnostics.Diagnostics.Count} errors={errorCount} lines={lineCount}");

        if (options.SequenceInlayProbes)
        {
            Console.WriteLine(await RunInlayAsync(new RangeTarget(0, 0, Math.Min(60, Math.Max(0, lineCount - 1)), 0, "visible:0-60")));
            if (lineCount > 60)
                Console.WriteLine(await RunInlayAsync(new RangeTarget(60, 0, Math.Min(120, Math.Max(0, lineCount - 1)), 0, "visible:60-120")));
        }

        foreach (var hoverTarget in SelectSequenceHoverTargets())
        {
            var hoverResult = await RunHoverAsync(hoverTarget.Label, hoverTarget.Position);
            var marker = hoverResult.Exception is not null
                ? "error"
                : !hoverResult.HasHover
                    ? "null"
                    : hoverResult.ElapsedMs >= options.SlowThresholdMs
                        ? "slow"
                        : "ok";
            Console.WriteLine($"{marker} {FormatHoverResult(hoverResult)}");
        }
    }

    string ResolveSequenceFile(string path)
    {
        if (Path.IsPathFullyQualified(path))
            return path;

        var projectRelative = Path.Combine(projectRoot, path);
        if (File.Exists(projectRelative))
            return projectRelative;

        return Path.GetFullPath(path);
    }
}

async Task RunReplayScenarioAsync(NamedReplayScenario replayScenario)
{
    Console.WriteLine(
        $"scenario name={replayScenario.Name} operation={replayScenario.Operation} project={replayScenario.ProjectRoot} file={replayScenario.FilePath}");

    switch (replayScenario.Operation)
    {
        case ReplayScenarioOperation.ProjectSequence:
            await RunProjectSequenceAsync();
            break;
        case ReplayScenarioOperation.DocumentDiagnostics:
            await RunDocumentDiagnosticsProbeAsync(replayScenario.Name);
            break;
        case ReplayScenarioOperation.HoverPositions:
            foreach (var targetPosition in replayScenario.HoverPositions)
            {
                var hoverResult = await RunHoverAsync(targetPosition.Label, new Position(targetPosition.Line, targetPosition.Character));
                var marker = hoverResult.Exception is not null
                    ? "error"
                    : !hoverResult.HasHover
                        ? "null"
                        : hoverResult.ElapsedMs >= options.SlowThresholdMs
                            ? "slow"
                            : "ok";
                Console.WriteLine($"{marker} {FormatHoverResult(hoverResult)}");
            }

            break;
        default:
            throw new InvalidOperationException($"Unsupported replay scenario operation '{replayScenario.Operation}'.");
    }
}

async Task RunDocumentDiagnosticsProbeAsync(
    string label,
    DocumentStore.DiagnosticLane lane = DocumentStore.DiagnosticLane.DocumentCompiler)
{
    var setupBefore = CaptureSetupSnapshot();
    var diagnosticBindingBefore = context.Compilation.PerformanceInstrumentation.DiagnosticBinding.CaptureSnapshot();
    var semanticBefore = context.Compilation.PerformanceInstrumentation.SemanticQuery.CaptureSnapshot();
    var stopwatch = Stopwatch.StartNew();
    var diagnostics = await store.TryGetDiagnosticsAsync(
        uri,
        lane,
        shouldSkipWork: null,
        CancellationToken.None);
    stopwatch.Stop();
    var setupDelta = CompilerSetupInstrumentation.Subtract(CaptureSetupSnapshot(), setupBefore);
    var diagnosticBindingDelta = DiagnosticBindingInstrumentation.Subtract(
        context.Compilation.PerformanceInstrumentation.DiagnosticBinding.CaptureSnapshot(),
        diagnosticBindingBefore);
    var semanticDelta = SemanticQueryInstrumentation.Subtract(
        context.Compilation.PerformanceInstrumentation.SemanticQuery.CaptureSnapshot(),
        semanticBefore);
    var errorCount = diagnostics.Diagnostics.Count(static diagnostic => diagnostic.Severity == LspDiagnosticSeverity.Error);

    Console.WriteLine(
        $"diagnostics {label}: {stopwatch.Elapsed.TotalMilliseconds:F1}ms " +
        $"lane={lane} " +
        $"count={diagnostics.Diagnostics.Count} errors={errorCount} " +
        $"setupDelta=[{CompilerSetupInstrumentation.FormatDelta(setupDelta)}] " +
        $"diagnosticBindingDelta=[{DiagnosticBindingInstrumentation.FormatDelta(diagnosticBindingDelta)}] " +
        $"semanticDelta=[{SemanticQueryInstrumentation.FormatDelta(semanticDelta)}]");

    foreach (var diagnostic in diagnostics.Diagnostics
        .Where(diagnostic => options.PrintDiagnostics || diagnostic.Severity == LspDiagnosticSeverity.Error)
        .Take(10))
    {
        Console.WriteLine("diagnostics " + FormatDiagnostic(diagnostic));
    }
}

async Task<double> SwitchDocumentAsync(string nextFilePath, bool upsertDocument = true)
{
    var stopwatch = Stopwatch.StartNew();
    filePath = Path.GetFullPath(nextFilePath);
    text = File.ReadAllText(filePath);
    uri = DocumentUri.FromFileSystemPath(filePath);
    if (upsertDocument)
        _ = store.UpsertDocument(uri, text);
    context = await store.GetAnalysisContextAsync(uri, CancellationToken.None)
        ?? throw new InvalidOperationException($"No analysis context for '{filePath}'.");
    sourceText = context.SourceText;
    model = await store.GetSemanticModelAsync(uri, CancellationToken.None)
        ?? throw new InvalidOperationException($"No semantic model for '{filePath}'.");
    root = context.SyntaxTree.GetRoot();
    stopwatch.Stop();
    return stopwatch.Elapsed.TotalMilliseconds;
}

IReadOnlyList<(string Label, Position Position)> SelectSequenceHoverTargets()
{
    var targets = new List<(string Label, Position Position)>();
    var seen = new HashSet<string>(StringComparer.Ordinal);

    foreach (var invocation in root.DescendantNodes().OfType<InvocationExpressionSyntax>())
    {
        if (targets.Count >= options.SequenceHoverCount)
            break;

        var name = invocation.Expression switch
        {
            MemberAccessExpressionSyntax { Name: SimpleNameSyntax memberName } => memberName,
            SimpleNameSyntax simpleName => simpleName,
            _ => null
        };

        if (name is null || !seen.Add(name.Identifier.ValueText))
            continue;

        var position = PositionHelper.ToRange(
            sourceText,
            new TextSpan(name.Identifier.SpanStart + Math.Min(3, name.Identifier.ValueText.Length), 0)).Start;
        targets.Add((name.Identifier.ValueText, position));
    }

    return targets;
}

async Task RunRandomHoversAsync()
{
    var tokens = root
        .DescendantTokens()
        .Where(static token => token.Kind == SyntaxKind.IdentifierToken &&
                               token.Parent is SimpleNameSyntax)
        .GroupBy(static token => token.Span)
        .Select(static group => group.First())
        .ToArray();

    var random = new Random(options.RandomSeed);
    var selectedTokens = tokens
        .OrderBy(_ => random.Next())
        .Take(Math.Min(options.RandomCount, tokens.Length))
        .ToArray();

    var results = new List<HoverResult>(selectedTokens.Length);
    Console.WriteLine(
        $"random-hover file={filePath} count={selectedTokens.Length}/{tokens.Length} seed={options.RandomSeed} slowMs={options.SlowThresholdMs:F1}");

    foreach (var token in selectedTokens)
    {
        var label = token.ValueText;
        var position = PositionHelper.ToRange(sourceText, new TextSpan(token.SpanStart + Math.Min(3, token.ValueText.Length), 0)).Start;
        var result = await RunHoverAsync(label, position);
        results.Add(result);

        var marker = result.Exception is not null
            ? "error"
            : !result.HasHover
            ? "null"
            : result.ElapsedMs >= options.SlowThresholdMs
            ? "slow"
            : "ok";
        Console.WriteLine(marker + " " + FormatHoverResult(result));
    }

    var slow = results
        .Where(result => result.ElapsedMs >= options.SlowThresholdMs || result.Exception is not null || !result.HasHover)
        .OrderByDescending(static result => result.ElapsedMs)
        .ToArray();
    var max = results.Count == 0 ? 0 : results.Max(static result => result.ElapsedMs);
    var avg = results.Count == 0 ? 0 : results.Average(static result => result.ElapsedMs);

    Console.WriteLine($"random-hover summary count={results.Count} avg={avg:F1}ms max={max:F1}ms slowOrNullOrError={slow.Length}");
    foreach (var result in slow.Take(10))
    {
        var status = result.Exception is not null
            ? "error"
            : result.HasHover
            ? "slow"
            : "null";
        Console.WriteLine($"  {status} {result.Label} {result.ElapsedMs:F1}ms {result.Line}:{result.Character} {result.Preview}");
    }
}

async Task ReplayPerformanceReportAsync()
{
    var reportPath = options.PerformanceReportPath is { Length: > 0 }
        ? Path.GetFullPath(options.PerformanceReportPath)
        : Path.Combine(repoRoot, "logs", "raven-lsp-performance.txt");
    if (!File.Exists(reportPath))
        throw new FileNotFoundException("Performance report not found.", reportPath);

    var targets = ReadHoverTargetsFromPerformanceReport(reportPath, filePath)
        .Take(options.ReplayCount)
        .ToArray();
    Console.WriteLine(
        $"replay-performance-report report={reportPath} file={filePath} count={targets.Length} slowMs={options.SlowThresholdMs:F1}");

    foreach (var target in targets)
    {
        var result = await RunHoverAsync(target.Label, new Position(target.Line, target.Character));
        var marker = result.Exception is not null
            ? "error"
            : !result.HasHover
            ? "null"
            : result.ElapsedMs >= options.SlowThresholdMs
            ? "slow"
            : "ok";
        Console.WriteLine(marker + " " + FormatHoverResult(result));
    }
}

async Task RunEditProbeAsync()
{
    var beforeContext = context;
    var beforeSourceText = sourceText;
    var beforeTreesByPath = beforeContext.Compilation.SyntaxTrees
        .Where(static tree => !string.IsNullOrWhiteSpace(tree.FilePath))
        .GroupBy(static tree => Path.GetFullPath(tree.FilePath), StringComparer.OrdinalIgnoreCase)
        .ToDictionary(static group => group.Key, static group => group.First(), StringComparer.OrdinalIgnoreCase);
    var updatedText = beforeSourceText;

    var warmupDocumentDiagnosticsSetupBefore = CaptureSetupSnapshot();
    var warmupDocumentDiagnosticsStopwatch = Stopwatch.StartNew();
    var warmupDocumentDiagnostics = await store.TryGetDiagnosticsAsync(
        uri,
        DocumentStore.DiagnosticLane.DocumentCompiler,
        shouldSkipWork: null,
        CancellationToken.None);
    warmupDocumentDiagnosticsStopwatch.Stop();
    var warmupDocumentDiagnosticsSetupDelta = CompilerSetupInstrumentation.Subtract(
        CaptureSetupSnapshot(),
        warmupDocumentDiagnosticsSetupBefore);

    Console.WriteLine($"edit-probe file={filePath} replacements={options.EditReplacements.Count}");
    Console.WriteLine(
        $"edit warmup documentDiagnostics={warmupDocumentDiagnosticsStopwatch.Elapsed.TotalMilliseconds:F1}ms " +
        $"documentDiagnosticsCount={warmupDocumentDiagnostics.Diagnostics.Count} " +
        $"setupDelta=[{CompilerSetupInstrumentation.FormatDelta(warmupDocumentDiagnosticsSetupDelta)}]");
    foreach (var replacement in options.EditReplacements)
    {
        var replaced = TryReplaceFirst(updatedText, replacement.OldText, replacement.NewText, out var nextText, out var span);
        if (!replaced)
        {
            Console.WriteLine($"edit error missing '{replacement.Label}'");
            return;
        }

        Console.WriteLine($"edit replace '{replacement.Label}' span={span} oldLength={replacement.OldText.Length} newLength={replacement.NewText.Length}");
        updatedText = nextText!;
    }

    var changeRanges = updatedText.GetChangeRanges(beforeSourceText);
    Console.WriteLine($"edit changeRanges={changeRanges.Count} " + string.Join("; ", changeRanges.Select(static range => range.ToString())));

    var updateSetupBefore = CaptureSetupSnapshot();
    var updateStopwatch = Stopwatch.StartNew();
    _ = store.UpsertDocument(uri, updatedText, deferMacroConsumerRefresh: true);
    updateStopwatch.Stop();
    var updateSetupDelta = CompilerSetupInstrumentation.Subtract(CaptureSetupSnapshot(), updateSetupBefore);

    if (options.EditWaitMilliseconds > 0)
        await Task.Delay(options.EditWaitMilliseconds);

    var analysisStopwatch = Stopwatch.StartNew();
    context = await store.GetAnalysisContextAsync(uri, CancellationToken.None)
        ?? throw new InvalidOperationException($"No analysis context after edit for '{filePath}'.");
    analysisStopwatch.Stop();

    sourceText = context.SourceText;
    root = context.SyntaxTree.GetRoot();
    var rootMatchesText = string.Equals(root.ToFullString(), updatedText.ToString(), StringComparison.Ordinal);
    var changedExecutableOwners = GetChangedExecutableOwners(context.Compilation, context.SyntaxTree);
    var semanticDiagnosticTransferBlocked = GetSemanticDiagnosticTransferBlocked(context.Compilation, context.SyntaxTree);

    var hoverResults = new List<(string Marker, HoverResult Result)>();
    var hoverTargets = options.EditHoverTargets.Count > 0
        ? options.EditHoverTargets
        : options.Positionals.Count > 2
            ? options.Positionals.Skip(2).ToArray()
            : ["CreateBuilder", "AddDbContext", "UseNpgsql"];

    foreach (var target in hoverTargets)
    {
        var offset = sourceText.ToString().IndexOf(target, StringComparison.Ordinal);
        if (offset < 0)
        {
            Console.WriteLine($"edit hover missing {target}");
            continue;
        }

        var position = PositionHelper.ToRange(sourceText, new TextSpan(offset + Math.Min(3, target.Length), 0)).Start;
        if (Environment.GetEnvironmentVariable("RAVEN_HEADLESS_DEBUG_SYMBOL_INFO") == "1")
        {
            model = await store.GetSemanticModelAsync(uri, CancellationToken.None)
                ?? throw new InvalidOperationException($"No semantic model after edit for '{filePath}'.");
            PrintInvocationSymbolInfoDebug(target, offset);
        }

        var hoverResult = await RunHoverAsync(target, position);
        var marker = hoverResult.Exception is not null
            ? "error"
            : !hoverResult.HasHover
            ? "null"
            : hoverResult.ElapsedMs >= options.SlowThresholdMs
            ? "slow"
            : "ok";
        hoverResults.Add((marker, hoverResult));

        var repeatHoverResult = await RunHoverAsync(target + ".repeat", position);
        var repeatMarker = repeatHoverResult.Exception is not null
            ? "error"
            : !repeatHoverResult.HasHover
            ? "null"
            : repeatHoverResult.ElapsedMs >= options.SlowThresholdMs
            ? "slow"
            : "ok";
        hoverResults.Add((repeatMarker, repeatHoverResult));
    }

    if (options.EditSkipDiagnostics)
    {
        foreach (var (marker, hoverResult) in hoverResults)
            Console.WriteLine(marker + " " + FormatHoverResult(hoverResult));

        return;
    }

    var semanticStopwatch = Stopwatch.StartNew();
    model = await store.GetSemanticModelAsync(uri, CancellationToken.None)
        ?? throw new InvalidOperationException($"No semantic model after edit for '{filePath}'.");
    semanticStopwatch.Stop();

    var syntaxDiagnosticsSetupBefore = CaptureSetupSnapshot();
    var syntaxDiagnosticsStopwatch = Stopwatch.StartNew();
    var syntaxDiagnostics = await store.TryGetDiagnosticsAsync(
        uri,
        DocumentStore.DiagnosticLane.Syntax,
        shouldSkipWork: null,
        CancellationToken.None);
    syntaxDiagnosticsStopwatch.Stop();
    var syntaxDiagnosticsSetupDelta = CompilerSetupInstrumentation.Subtract(
        CaptureSetupSnapshot(),
        syntaxDiagnosticsSetupBefore);

    var documentDiagnosticsSetupBefore = CaptureSetupSnapshot();
    var documentDiagnosticsStopwatch = Stopwatch.StartNew();
    var documentDiagnostics = await store.TryGetDiagnosticsAsync(
        uri,
        DocumentStore.DiagnosticLane.DocumentCompiler,
        shouldSkipWork: null,
        CancellationToken.None);
    documentDiagnosticsStopwatch.Stop();
    var documentDiagnosticsSetupDelta = CompilerSetupInstrumentation.Subtract(
        CaptureSetupSnapshot(),
        documentDiagnosticsSetupBefore);

    var fullDiagnosticsSetupBefore = CaptureSetupSnapshot();
    var fullDiagnosticsStopwatch = Stopwatch.StartNew();
    var fullDiagnostics = await store.GetDiagnosticsAsync(uri, CancellationToken.None);
    fullDiagnosticsStopwatch.Stop();
    var fullDiagnosticsSetupDelta = CompilerSetupInstrumentation.Subtract(
        CaptureSetupSnapshot(),
        fullDiagnosticsSetupBefore);

    var afterTreesByPath = context.Compilation.SyntaxTrees
        .Where(static tree => !string.IsNullOrWhiteSpace(tree.FilePath))
        .GroupBy(static tree => Path.GetFullPath(tree.FilePath), StringComparer.OrdinalIgnoreCase)
        .ToDictionary(static group => group.Key, static group => group.First(), StringComparer.OrdinalIgnoreCase);
    var currentPath = Path.GetFullPath(filePath);
    var unchangedTotal = 0;
    var unchangedReused = 0;
    foreach (var (path, beforeTree) in beforeTreesByPath)
    {
        if (string.Equals(path, currentPath, StringComparison.OrdinalIgnoreCase))
            continue;

        unchangedTotal++;
        if (afterTreesByPath.TryGetValue(path, out var afterTree) && ReferenceEquals(beforeTree, afterTree))
            unchangedReused++;
    }

    var editedTreeReused = beforeTreesByPath.TryGetValue(currentPath, out var editedBeforeTree) &&
                           afterTreesByPath.TryGetValue(currentPath, out var editedAfterTree) &&
                           ReferenceEquals(editedBeforeTree, editedAfterTree);
    var documentErrorCount = documentDiagnostics.Diagnostics.Count(static diagnostic => diagnostic.Severity == LspDiagnosticSeverity.Error);
    var fullErrorCount = fullDiagnostics.Count(static diagnostic => diagnostic.Severity == LspDiagnosticSeverity.Error);

    Console.WriteLine(
        $"edit summary update={updateStopwatch.Elapsed.TotalMilliseconds:F1}ms " +
        $"analysis={analysisStopwatch.Elapsed.TotalMilliseconds:F1}ms " +
        $"semantic={semanticStopwatch.Elapsed.TotalMilliseconds:F1}ms " +
        $"syntaxDiagnostics={syntaxDiagnosticsStopwatch.Elapsed.TotalMilliseconds:F1}ms " +
        $"syntaxDiagnosticsCount={syntaxDiagnostics.Diagnostics.Count} " +
        $"documentDiagnostics={documentDiagnosticsStopwatch.Elapsed.TotalMilliseconds:F1}ms " +
        $"documentDiagnosticsCount={documentDiagnostics.Diagnostics.Count} documentErrors={documentErrorCount} " +
        $"fullDiagnostics={fullDiagnosticsStopwatch.Elapsed.TotalMilliseconds:F1}ms " +
        $"fullDiagnosticsCount={fullDiagnostics.Count} fullErrors={fullErrorCount} " +
        $"rootMatchesText={rootMatchesText} editedTreeReused={editedTreeReused} " +
        $"changedExecutableOwners={changedExecutableOwners.Count} " +
        $"semanticDiagnosticTransferBlocked={semanticDiagnosticTransferBlocked} " +
        $"unchangedTreeReuse={unchangedReused}/{unchangedTotal}");
    foreach (var changedOwner in changedExecutableOwners.Take(10))
        Console.WriteLine($"edit changedOwner {changedOwner}");
    Console.WriteLine(
        $"edit setupDeltas update=[{CompilerSetupInstrumentation.FormatDelta(updateSetupDelta)}] " +
        $"syntaxDiagnostics=[{CompilerSetupInstrumentation.FormatDelta(syntaxDiagnosticsSetupDelta)}] " +
        $"documentDiagnostics=[{CompilerSetupInstrumentation.FormatDelta(documentDiagnosticsSetupDelta)}] " +
        $"fullDiagnostics=[{CompilerSetupInstrumentation.FormatDelta(fullDiagnosticsSetupDelta)}]");
    foreach (var (marker, hoverResult) in hoverResults)
        Console.WriteLine(marker + " " + FormatHoverResult(hoverResult));

    foreach (var diagnostic in documentDiagnostics.Diagnostics
        .Where(static diagnostic => diagnostic.Severity == LspDiagnosticSeverity.Error)
        .Take(10))
    {
        Console.WriteLine("edit documentError " + FormatDiagnostic(diagnostic));
    }

    foreach (var diagnostic in fullDiagnostics
        .Where(static diagnostic => diagnostic.Severity == LspDiagnosticSeverity.Error)
        .Take(10))
    {
        Console.WriteLine("edit fullError " + FormatDiagnostic(diagnostic));
    }

    if (options.PrintDiagnostics)
    {
        foreach (var diagnostic in fullDiagnostics.Take(80))
            Console.WriteLine("edit fullDiagnostic " + FormatDiagnostic(diagnostic));
    }
}

void PrintInvocationSymbolInfoDebug(string target, int offset)
{
    var token = root.FindToken(Math.Clamp(offset, 0, root.FullSpan.End));
    var invocation = token.Parent?
        .AncestorsAndSelf()
        .OfType<InvocationExpressionSyntax>()
        .FirstOrDefault(candidate => candidate.Expression.Span.Contains(token.Span) ||
                                     candidate.Expression.Span.End == token.SpanStart);
    if (invocation is null)
        return;

    var invocationInfo = model.GetSymbolInfo(invocation);
    var expressionInfo = model.GetSymbolInfo(invocation.Expression);
    var targetInfo = token.Parent is SyntaxNode targetNode
        ? model.GetSymbolInfo(targetNode)
        : default;

    Console.WriteLine(
        $"edit symbolInfo {target} args={invocation.ArgumentList.Arguments.Count} " +
        $"invocationSymbol={FormatSymbol(invocationInfo.Symbol)} " +
        $"invocationCandidates=[{FormatCandidates(invocationInfo.CandidateSymbols)}] " +
        $"expressionSymbol={FormatSymbol(expressionInfo.Symbol)} " +
        $"expressionCandidates=[{FormatCandidates(expressionInfo.CandidateSymbols)}] " +
        $"targetSymbol={FormatSymbol(targetInfo.Symbol)} " +
        $"targetCandidates=[{FormatCandidates(targetInfo.CandidateSymbols)}]");
}

async Task<HoverResult> RunHoverAsync(string label, Position position)
{
    var before = context.Compilation.PerformanceInstrumentation.SemanticQuery.CaptureSnapshot();
    var setupBefore = CaptureSetupSnapshot();
    var functionParameterBefore = context.Compilation.PerformanceInstrumentation.FunctionExpressionParameters.CaptureSnapshot();
    var sw = Stopwatch.StartNew();
    Hover? hover = null;
    Exception? exception = null;

    try
    {
        hover = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = position
        }, CancellationToken.None);
    }
    catch (Exception ex)
    {
        exception = ex;
    }

    sw.Stop();
    var after = context.Compilation.PerformanceInstrumentation.SemanticQuery.CaptureSnapshot();
    var delta = SemanticQueryInstrumentation.Subtract(after, before);
    var setupDelta = CompilerSetupInstrumentation.Subtract(CaptureSetupSnapshot(), setupBefore);
    var functionParameterDelta = FunctionExpressionParameterInstrumentation.Subtract(
        context.Compilation.PerformanceInstrumentation.FunctionExpressionParameters.CaptureSnapshot(),
        functionParameterBefore);
    var hoverLines = hover?.Contents.MarkupContent?.Value.Split('\n') ?? ["<null>"];
    var firstLine = exception is null
        ? string.Join(" | ", hoverLines.Take(3))
        : exception.GetType().Name + ": " + exception.Message;

    return new HoverResult(label, position.Line, position.Character, sw.Elapsed.TotalMilliseconds, hover is not null, exception, firstLine, delta, setupDelta, functionParameterDelta);
}

async Task<string> RunInlayAsync(RangeTarget range)
{
    var before = context.Compilation.PerformanceInstrumentation.SemanticQuery.CaptureSnapshot();
    var sw = Stopwatch.StartNew();
    var result = await inlayHandler.Handle(new InlayHintParams
    {
        TextDocument = new TextDocumentIdentifier(uri),
        Range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range
        {
            Start = new Position(range.StartLine, range.StartCharacter),
            End = new Position(range.EndLine, range.EndCharacter)
        }
    }, CancellationToken.None);
    sw.Stop();
    var after = context.Compilation.PerformanceInstrumentation.SemanticQuery.CaptureSnapshot();
    var delta = SemanticQueryInstrumentation.Subtract(after, before);
    var hints = result?.ToArray() ?? [];
    var tooltipCount = hints.Count(static hint => hint.Tooltip is not null);
    var preview = string.Join(
        "; ",
        hints.Take(20).Select(static hint =>
            $"{hint.Position.Line}:{hint.Position.Character} {hint.Label}"));

    return $"inlay {range.Label}: {sw.Elapsed.TotalMilliseconds:F1}ms hints={hints.Length} tooltips={tooltipCount} [{SemanticQueryInstrumentation.FormatDelta(delta)}] {preview}";
}

async Task RunTypeImportProbeAsync(string importText)
{
    if (!importText.EndsWith('\n'))
        importText += Environment.NewLine;

    var typedText = string.Empty;
    Console.WriteLine($"type-import project={projectRoot} file={filePath} text={importText.TrimEnd()}");

    for (var i = 0; i < importText.Length; i++)
    {
        typedText += importText[i];
        _ = store.UpsertDocument(uri, SourceText.From(typedText), deferMacroConsumerRefresh: true);
        context = await store.GetAnalysisContextAsync(uri, CancellationToken.None)
            ?? throw new InvalidOperationException($"No analysis context for '{filePath}'.");
        sourceText = context.SourceText;
        model = await store.GetSemanticModelAsync(uri, CancellationToken.None)
            ?? throw new InvalidOperationException($"No semantic model for '{filePath}'.");
        root = context.SyntaxTree.GetRoot();

        if (importText[i] != '.')
            continue;

        var position = PositionHelper.ToRange(sourceText, new TextSpan(typedText.Length, 0)).Start;
        var before = context.Compilation.PerformanceInstrumentation.SemanticQuery.CaptureSnapshot();
        var sw = Stopwatch.StartNew();
        var completionList = await completionHandler.Handle(new CompletionParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = position,
            Context = new CompletionContext
            {
                TriggerKind = CompletionTriggerKind.TriggerCharacter,
                TriggerCharacter = "."
            }
        }, CancellationToken.None);
        sw.Stop();
        var after = context.Compilation.PerformanceInstrumentation.SemanticQuery.CaptureSnapshot();
        var delta = SemanticQueryInstrumentation.Subtract(after, before);
        var items = completionList?.Items?.ToArray() ?? [];
        var labels = string.Join(", ", items.Take(12).Select(static item => item.Label));
        var lineText = typedText.Replace("\r", "\\r", StringComparison.Ordinal).Replace("\n", "\\n", StringComparison.Ordinal);

        Console.WriteLine(
            $"type-import completion step={i + 1} position={position.Line}:{position.Character} " +
            $"typed=\"{lineText}\" elapsed={sw.Elapsed.TotalMilliseconds:F1}ms items={items.Length} " +
            $"wildcard={items.Any(static item => item.Label == "*")} " +
            $"http={items.Any(static item => item.Label == "Http")} " +
            $"ipAddress={items.Any(static item => item.Label == "IPAddress")} " +
            $"[{SemanticQueryInstrumentation.FormatDelta(delta)}] labels=[{labels}]");
    }
}

async Task RunStressSuiteAsync()
{
    var sizes = options.StressSizes.Count > 0
        ? options.StressSizes
        : [2, 25, 75];
    var analyzerModes = options.StressIncludeAnalyzers
        ? [false, true]
        : new[] { false };
    var inlayModes = options.StressIncludeInlays
        ? [false, true]
        : new[] { false };
    var tempRoot = Path.Combine(Path.GetTempPath(), $"raven-lsp-stress-{Guid.NewGuid():N}");

    Console.WriteLine(
        $"stress-suite root={tempRoot} sizes=[{string.Join(",", sizes)}] " +
        $"analyzers=[{string.Join(",", analyzerModes.Select(static value => value ? "on" : "off"))}] " +
        $"inlays=[{string.Join(",", inlayModes.Select(static value => value ? "on" : "off"))}]");

    try
    {
        foreach (var size in sizes)
            foreach (var runAnalyzers in analyzerModes)
                foreach (var runInlays in inlayModes)
                    await RunStressScenarioAsync(tempRoot, size, runAnalyzers, runInlays);
    }
    finally
    {
        if (Environment.GetEnvironmentVariable("RAVEN_HEADLESS_KEEP_STRESS_PROJECTS") != "1" &&
            Directory.Exists(tempRoot))
        {
            Directory.Delete(tempRoot, recursive: true);
        }
    }
}

async Task RunEditScenarioSuiteAsync()
{
    var scenarios = CreateEditScenarios(repoRoot);
    Console.WriteLine($"edit-scenario-suite count={scenarios.Count} slowMs={options.SlowThresholdMs:F1}");

    foreach (var scenario in scenarios)
    {
        if (!File.Exists(scenario.FilePath))
        {
            Console.WriteLine($"edit-scenario skip name={scenario.Name} reason=missing-file file={scenario.FilePath}");
            continue;
        }

        await RunEditScenarioAsync(scenario, EditScenarioMode.WarmDocumentDiagnostics);
        await RunEditScenarioAsync(scenario, EditScenarioMode.ColdHoverThenDiagnostics);
    }
}

async Task RunEditScenarioAsync(EditScenario scenario, EditScenarioMode mode)
{
    var scenarioText = SourceText.From(File.ReadAllText(scenario.FilePath));
    var scenarioUri = DocumentUri.FromFileSystemPath(scenario.FilePath);
    var scenarioWorkspace = RavenWorkspace.Create(targetFramework: "net10.0");
    var scenarioManager = new WorkspaceManager(scenarioWorkspace, NullLogger<WorkspaceManager>.Instance);
    scenarioManager.Initialize(new InitializeParams
    {
        WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
        {
            Name = Path.GetFileName(scenario.ProjectRoot),
            Uri = DocumentUri.FromFileSystemPath(scenario.ProjectRoot)
        })
    });

    var scenarioStore = new DocumentStore(scenarioManager, NullLogger<DocumentStore>.Instance);
    var scenarioHover = new HoverHandler(scenarioStore, NullLogger<HoverHandler>.Instance);
    _ = scenarioStore.UpsertDocument(scenarioUri, scenarioText);

    var beforeContext = await scenarioStore.GetAnalysisContextAsync(scenarioUri, CancellationToken.None)
        ?? throw new InvalidOperationException($"No analysis context for '{scenario.FilePath}'.");
    var beforeTreesByPath = GetTreesByPath(beforeContext.Compilation);

    var warmupStopwatch = Stopwatch.StartNew();
    var warmupDiagnostics = await scenarioStore.TryGetDiagnosticsAsync(
        scenarioUri,
        DocumentStore.DiagnosticLane.DocumentCompiler,
        shouldSkipWork: null,
        CancellationToken.None);
    warmupStopwatch.Stop();

    var updatedText = scenarioText;
    foreach (var replacement in scenario.Replacements)
    {
        if (!TryReplaceFirst(updatedText, replacement.OldText, replacement.NewText, out var nextText, out _))
        {
            Console.WriteLine(
                $"edit-scenario error name={scenario.Name} reason=missing-replacement replacement=\"{replacement.Label}\"");
            return;
        }

        updatedText = nextText!;
    }

    var updateSetupBefore = CaptureScenarioSetupSnapshot(scenarioManager, scenarioUri, scenario.FilePath);
    var updateStopwatch = Stopwatch.StartNew();
    _ = scenarioStore.UpsertDocument(scenarioUri, updatedText, deferMacroConsumerRefresh: true);
    updateStopwatch.Stop();
    var updateSetupDelta = CompilerSetupInstrumentation.Subtract(
        CaptureScenarioSetupSnapshot(scenarioManager, scenarioUri, scenario.FilePath),
        updateSetupBefore);

    var analysisStopwatch = Stopwatch.StartNew();
    var afterContext = await scenarioStore.GetAnalysisContextAsync(scenarioUri, CancellationToken.None)
        ?? throw new InvalidOperationException($"No analysis context after edit for '{scenario.FilePath}'.");
    analysisStopwatch.Stop();

    var syntaxSetupBefore = CaptureScenarioSetupSnapshot(scenarioManager, scenarioUri, scenario.FilePath);
    var syntaxStopwatch = Stopwatch.StartNew();
    var syntaxDiagnostics = await scenarioStore.TryGetDiagnosticsAsync(
        scenarioUri,
        DocumentStore.DiagnosticLane.Syntax,
        shouldSkipWork: null,
        CancellationToken.None);
    syntaxStopwatch.Stop();
    var syntaxSetupDelta = CompilerSetupInstrumentation.Subtract(
        CaptureScenarioSetupSnapshot(scenarioManager, scenarioUri, scenario.FilePath),
        syntaxSetupBefore);

    double preHoverDocumentDiagnosticsMs = -1;
    int preHoverDocumentDiagnosticsCount = -1;
    int preHoverDocumentDiagnosticsErrors = -1;
    var preHoverDocumentSetupDelta = default(CompilerSetupInstrumentation.Snapshot);
    if (mode == EditScenarioMode.WarmDocumentDiagnostics)
    {
        var documentSetupBefore = CaptureScenarioSetupSnapshot(scenarioManager, scenarioUri, scenario.FilePath);
        var documentStopwatch = Stopwatch.StartNew();
        var documentDiagnostics = await scenarioStore.TryGetDiagnosticsAsync(
            scenarioUri,
            DocumentStore.DiagnosticLane.DocumentCompiler,
            shouldSkipWork: null,
            CancellationToken.None);
        documentStopwatch.Stop();
        preHoverDocumentSetupDelta = CompilerSetupInstrumentation.Subtract(
            CaptureScenarioSetupSnapshot(scenarioManager, scenarioUri, scenario.FilePath),
            documentSetupBefore);
        preHoverDocumentDiagnosticsMs = documentStopwatch.Elapsed.TotalMilliseconds;
        preHoverDocumentDiagnosticsCount = documentDiagnostics.Diagnostics.Count;
        preHoverDocumentDiagnosticsErrors = documentDiagnostics.Diagnostics.Count(static diagnostic => diagnostic.Severity == LspDiagnosticSeverity.Error);
    }

    var changedOwners = GetChangedExecutableOwners(afterContext.Compilation, afterContext.SyntaxTree);
    var afterTreesByPath = GetTreesByPath(afterContext.Compilation);
    var currentPath = Path.GetFullPath(scenario.FilePath);
    var unchangedTotal = 0;
    var unchangedReused = 0;
    foreach (var (path, beforeTree) in beforeTreesByPath)
    {
        if (string.Equals(path, currentPath, StringComparison.OrdinalIgnoreCase))
            continue;

        unchangedTotal++;
        if (afterTreesByPath.TryGetValue(path, out var afterTree) && ReferenceEquals(beforeTree, afterTree))
            unchangedReused++;
    }

    var syntaxErrors = syntaxDiagnostics.Diagnostics.Count(static diagnostic => diagnostic.Severity == LspDiagnosticSeverity.Error);
    Console.WriteLine(
        $"edit-scenario name={scenario.Name} mode={FormatEditScenarioMode(mode)} replacements={scenario.Replacements.Count} " +
        $"warmupDocumentDiagnostics={warmupStopwatch.Elapsed.TotalMilliseconds:F1}ms warmupCount={warmupDiagnostics.Diagnostics.Count} " +
        $"update={updateStopwatch.Elapsed.TotalMilliseconds:F1}ms analysis={analysisStopwatch.Elapsed.TotalMilliseconds:F1}ms " +
        $"syntaxDiagnostics={syntaxStopwatch.Elapsed.TotalMilliseconds:F1}ms syntaxCount={syntaxDiagnostics.Diagnostics.Count} syntaxErrors={syntaxErrors} " +
        $"preHoverDocumentDiagnostics={preHoverDocumentDiagnosticsMs:F1}ms preHoverDocumentCount={preHoverDocumentDiagnosticsCount} preHoverDocumentErrors={preHoverDocumentDiagnosticsErrors} " +
        $"changedOwners={changedOwners.Count} unchangedTreeReuse={unchangedReused}/{unchangedTotal} " +
        $"updateSetup=[{CompilerSetupInstrumentation.FormatDelta(updateSetupDelta)}] " +
        $"syntaxSetup=[{CompilerSetupInstrumentation.FormatDelta(syntaxSetupDelta)}] " +
        $"preHoverDocumentSetup=[{CompilerSetupInstrumentation.FormatDelta(preHoverDocumentSetupDelta)}]");

    foreach (var changedOwner in changedOwners.Take(6))
        Console.WriteLine($"edit-scenario changedOwner name={scenario.Name} mode={FormatEditScenarioMode(mode)} {changedOwner}");

    foreach (var hoverTarget in scenario.HoverTargets)
    {
        var firstHover = await RunEditScenarioHoverAsync(
            scenarioStore,
            scenarioHover,
            scenarioUri,
            updatedText,
            afterContext.Compilation,
            hoverTarget,
            "first");
        var repeatHover = await RunEditScenarioHoverAsync(
            scenarioStore,
            scenarioHover,
            scenarioUri,
            updatedText,
            afterContext.Compilation,
            hoverTarget,
            "repeat");

        Console.WriteLine(FormatEditScenarioHover(scenario.Name, mode, firstHover));
        Console.WriteLine(FormatEditScenarioHover(scenario.Name, mode, repeatHover));
    }

    if (mode == EditScenarioMode.ColdHoverThenDiagnostics)
    {
        var postHoverDocumentSetupBefore = CaptureScenarioSetupSnapshot(scenarioManager, scenarioUri, scenario.FilePath);
        var postHoverDocumentStopwatch = Stopwatch.StartNew();
        var postHoverDocumentDiagnostics = await scenarioStore.TryGetDiagnosticsAsync(
            scenarioUri,
            DocumentStore.DiagnosticLane.DocumentCompiler,
            shouldSkipWork: null,
            CancellationToken.None);
        postHoverDocumentStopwatch.Stop();
        var postHoverDocumentSetupDelta = CompilerSetupInstrumentation.Subtract(
            CaptureScenarioSetupSnapshot(scenarioManager, scenarioUri, scenario.FilePath),
            postHoverDocumentSetupBefore);
        var postHoverDocumentErrors = postHoverDocumentDiagnostics.Diagnostics.Count(static diagnostic => diagnostic.Severity == LspDiagnosticSeverity.Error);
        Console.WriteLine(
            $"edit-scenario-post-hover-diagnostics name={scenario.Name} mode={FormatEditScenarioMode(mode)} " +
            $"documentDiagnostics={postHoverDocumentStopwatch.Elapsed.TotalMilliseconds:F1}ms " +
            $"documentCount={postHoverDocumentDiagnostics.Diagnostics.Count} documentErrors={postHoverDocumentErrors} " +
            $"documentSetup=[{CompilerSetupInstrumentation.FormatDelta(postHoverDocumentSetupDelta)}]");
    }
}

async Task<HoverResult> RunEditScenarioHoverAsync(
    DocumentStore scenarioStore,
    HoverHandler scenarioHover,
    DocumentUri scenarioUri,
    SourceText scenarioText,
    Compilation scenarioCompilation,
    EditHoverTarget target,
    string label)
{
    var offset = scenarioText.ToString().IndexOf(target.SearchText, StringComparison.Ordinal);
    if (offset < 0)
        return new HoverResult(target.Label + "." + label, 0, 0, 0, HasHover: false, new InvalidOperationException($"Missing target '{target.SearchText}'."), "<missing>", default, default, default);

    var before = scenarioCompilation.PerformanceInstrumentation.SemanticQuery.CaptureSnapshot();
    var setupBefore = scenarioCompilation.PerformanceInstrumentation.Setup.CaptureSnapshot();
    var position = PositionHelper.ToRange(
        scenarioText,
        new TextSpan(offset + Math.Min(1, target.SearchText.Length), 0)).Start;
    var stopwatch = Stopwatch.StartNew();
    Hover? hover = null;
    Exception? exception = null;
    try
    {
        hover = await scenarioHover.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(scenarioUri),
            Position = position
        }, CancellationToken.None);
    }
    catch (Exception ex)
    {
        exception = ex;
    }

    stopwatch.Stop();
    var semanticDelta = SemanticQueryInstrumentation.Subtract(
        scenarioCompilation.PerformanceInstrumentation.SemanticQuery.CaptureSnapshot(),
        before);
    var setupDelta = CompilerSetupInstrumentation.Subtract(
        scenarioCompilation.PerformanceInstrumentation.Setup.CaptureSnapshot(),
        setupBefore);
    var hoverLines = hover?.Contents.MarkupContent?.Value.Split('\n') ?? ["<null>"];
    var firstLine = exception is null
        ? string.Join(" | ", hoverLines.Take(3))
        : exception.GetType().Name + ": " + exception.Message;

    return new HoverResult(
        target.Label + "." + label,
        position.Line,
        position.Character,
        stopwatch.Elapsed.TotalMilliseconds,
        hover is not null,
        exception,
        firstLine,
        semanticDelta,
        setupDelta,
        default);
}

async Task RunStressScenarioAsync(string tempRoot, int stableFileCount, bool runAnalyzers, bool runInlays)
{
    var scenarioRoot = Path.Combine(
        tempRoot,
        $"size-{stableFileCount}-analyzers-{(runAnalyzers ? "on" : "off")}-inlays-{(runInlays ? "on" : "off")}");
    var sourceRoot = Path.Combine(scenarioRoot, "src");
    Directory.CreateDirectory(sourceRoot);
    File.WriteAllText(Path.Combine(scenarioRoot, "App.rvnproj"), $$"""
        <Project Sdk="Microsoft.NET.Sdk">
          <PropertyGroup>
            <TargetFramework>net10.0</TargetFramework>
            <OutputType>Exe</OutputType>
            <RavenRunAnalyzers>{{runAnalyzers.ToString().ToLowerInvariant()}}</RavenRunAnalyzers>
          </PropertyGroup>
          <ItemGroup>
            <RavenCompile Include="src/**/*.rvn" />
          </ItemGroup>
        </Project>
        """);

    var mainPath = Path.Combine(sourceRoot, "main.rvn");
    var mainText = SourceText.From("""
        class Runner {
            func Compute(value: int) -> int {
                val unusedValue = value
                val answer = value + 1
                return answer
            }
        }
        """);
    File.WriteAllText(mainPath, mainText.ToString());

    for (var i = 0; i < stableFileCount; i++)
    {
        File.WriteAllText(
            Path.Combine(sourceRoot, $"stable{i:D3}.rvn"),
            $$"""
            class Stable{{i}} {
                func Identity(value: int) -> int {
                    return value + {{i + 1}}
                }
            }
            """);
    }

    var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
    var stressManager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
    stressManager.Initialize(new InitializeParams
    {
        WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
        {
            Name = Path.GetFileName(scenarioRoot),
            Uri = DocumentUri.FromFileSystemPath(scenarioRoot)
        })
    });

    var stressStore = new DocumentStore(stressManager, NullLogger<DocumentStore>.Instance);
    var stressHover = new HoverHandler(stressStore, NullLogger<HoverHandler>.Instance);
    var stressInlay = new InlayHintHandler(stressStore, NullLogger<InlayHintHandler>.Instance);
    var stressUri = DocumentUri.FromFileSystemPath(mainPath);
    _ = stressStore.UpsertDocument(stressUri, mainText);

    var warmContext = await stressStore.GetAnalysisContextAsync(stressUri, CancellationToken.None)
        ?? throw new InvalidOperationException($"No stress analysis context for '{mainPath}'.");
    var warmupStopwatch = Stopwatch.StartNew();
    var warmupDiagnostics = await stressStore.TryGetDiagnosticsAsync(
        stressUri,
        DocumentStore.DiagnosticLane.DocumentCompiler,
        shouldSkipWork: null,
        CancellationToken.None);
    warmupStopwatch.Stop();

    var updatedText = ReplaceStressText(mainText, "value + 1", "value + 2");
    var updateStopwatch = Stopwatch.StartNew();
    _ = stressStore.UpsertDocument(stressUri, updatedText, deferMacroConsumerRefresh: true);
    updateStopwatch.Stop();

    var analysisStopwatch = Stopwatch.StartNew();
    var contextAfterEdit = await stressStore.GetAnalysisContextAsync(stressUri, CancellationToken.None)
        ?? throw new InvalidOperationException($"No stress analysis context after edit for '{mainPath}'.");
    analysisStopwatch.Stop();

    var semanticStopwatch = Stopwatch.StartNew();
    _ = await stressStore.GetSemanticModelAsync(stressUri, CancellationToken.None)
        ?? throw new InvalidOperationException($"No stress semantic model after edit for '{mainPath}'.");
    semanticStopwatch.Stop();

    var firstHover = await RunStressHoverAsync(stressStore, stressHover, stressUri, updatedText, "answer", "first");
    var secondHover = await RunStressHoverAsync(stressStore, stressHover, stressUri, updatedText, "answer", "second");

    var documentDiagnosticsStopwatch = Stopwatch.StartNew();
    var documentDiagnostics = await stressStore.TryGetDiagnosticsAsync(
        stressUri,
        DocumentStore.DiagnosticLane.DocumentCompiler,
        shouldSkipWork: null,
        CancellationToken.None);
    documentDiagnosticsStopwatch.Stop();

    var projectDiagnosticsStopwatch = Stopwatch.StartNew();
    var projectDiagnostics = await stressStore.TryGetDiagnosticsAsync(
        stressUri,
        DocumentStore.DiagnosticLane.ProjectWithAnalyzers,
        shouldSkipWork: null,
        CancellationToken.None);
    projectDiagnosticsStopwatch.Stop();

    var inlayMs = 0d;
    var inlayCount = 0;
    if (runInlays)
    {
        var lineCount = Math.Max(1, contextAfterEdit.SourceText.GetLineCount());
        var inlayStopwatch = Stopwatch.StartNew();
        var inlayResult = await stressInlay.Handle(new InlayHintParams
        {
            TextDocument = new TextDocumentIdentifier(stressUri),
            Range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range
            {
                Start = new Position(0, 0),
                End = new Position(lineCount - 1, 0)
            }
        }, CancellationToken.None);
        inlayStopwatch.Stop();
        inlayMs = inlayStopwatch.Elapsed.TotalMilliseconds;
        inlayCount = inlayResult?.Count() ?? 0;
    }

    var documentErrors = documentDiagnostics.Diagnostics.Count(static diagnostic => diagnostic.Severity == LspDiagnosticSeverity.Error);
    var projectErrors = projectDiagnostics.Diagnostics.Count(static diagnostic => diagnostic.Severity == LspDiagnosticSeverity.Error);

    Console.WriteLine(
        $"stress scenario size={stableFileCount + 1} analyzers={(runAnalyzers ? "on" : "off")} inlays={(runInlays ? "on" : "off")} " +
        $"warmupDiagnostics={warmupStopwatch.Elapsed.TotalMilliseconds:F1}ms warmupCount={warmupDiagnostics.Diagnostics.Count} " +
        $"update={updateStopwatch.Elapsed.TotalMilliseconds:F1}ms analysis={analysisStopwatch.Elapsed.TotalMilliseconds:F1}ms " +
        $"semantic={semanticStopwatch.Elapsed.TotalMilliseconds:F1}ms " +
        $"firstHover={firstHover.ElapsedMs:F1}ms firstHoverHas={firstHover.HasHover} " +
        $"secondHover={secondHover.ElapsedMs:F1}ms secondHoverHas={secondHover.HasHover} " +
        $"documentDiagnostics={documentDiagnosticsStopwatch.Elapsed.TotalMilliseconds:F1}ms documentCount={documentDiagnostics.Diagnostics.Count} documentErrors={documentErrors} " +
        $"projectDiagnostics={projectDiagnosticsStopwatch.Elapsed.TotalMilliseconds:F1}ms projectCount={projectDiagnostics.Diagnostics.Count} projectErrors={projectErrors} " +
        $"inlay={inlayMs:F1}ms inlayCount={inlayCount} " +
        $"firstSemantic=[{SemanticQueryInstrumentation.FormatDelta(firstHover.SemanticDelta)}] " +
        $"secondSemantic=[{SemanticQueryInstrumentation.FormatDelta(secondHover.SemanticDelta)}]");

    _ = warmContext;
}

async Task<HoverResult> RunStressHoverAsync(
    DocumentStore stressStore,
    HoverHandler stressHover,
    DocumentUri stressUri,
    SourceText stressSourceText,
    string target,
    string label)
{
    var targetOffset = IndexOfStressOccurrence(stressSourceText.ToString(), target, occurrence: 2);
    if (targetOffset < 0)
        return new HoverResult(label, 0, 0, 0, HasHover: false, new InvalidOperationException($"Missing target '{target}'."), "<missing>", default, default, default);

    var stressContext = await stressStore.GetAnalysisContextAsync(stressUri, CancellationToken.None);
    if (stressContext is null)
        return new HoverResult(label, 0, 0, 0, HasHover: false, new InvalidOperationException("Missing stress context."), "<missing-context>", default, default, default);

    var before = stressContext.Value.Compilation.PerformanceInstrumentation.SemanticQuery.CaptureSnapshot();
    var position = PositionHelper.ToRange(
        stressSourceText,
        new TextSpan(targetOffset + Math.Min(1, target.Length), 0)).Start;
    var stopwatch = Stopwatch.StartNew();
    var hover = await stressHover.Handle(new HoverParams
    {
        TextDocument = new TextDocumentIdentifier(stressUri),
        Position = position
    }, CancellationToken.None);
    stopwatch.Stop();
    var after = stressContext.Value.Compilation.PerformanceInstrumentation.SemanticQuery.CaptureSnapshot();
    var delta = SemanticQueryInstrumentation.Subtract(after, before);
    var preview = hover?.Contents.MarkupContent?.Value.Split('\n').FirstOrDefault() ?? "<null>";

    return new HoverResult(label, position.Line, position.Character, stopwatch.Elapsed.TotalMilliseconds, hover is not null, null, preview, delta, default, default);
}

static SourceText ReplaceStressText(SourceText sourceText, string oldText, string newText)
{
    var text = sourceText.ToString();
    var start = text.IndexOf(oldText, StringComparison.Ordinal);
    if (start < 0)
        throw new InvalidOperationException($"Stress source did not contain '{oldText}'.");

    return sourceText.Replace(new TextSpan(start, oldText.Length), newText);
}

static int IndexOfStressOccurrence(string text, string searchText, int occurrence)
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

CompilerSetupInstrumentation.Snapshot CaptureSetupSnapshot()
{
    if (!manager.TryGetCompilation(uri, out var compilation) || compilation is null)
        throw new InvalidOperationException($"No compilation for '{filePath}'.");

    return compilation.PerformanceInstrumentation.Setup.CaptureSnapshot();
}

static CompilerSetupInstrumentation.Snapshot CaptureScenarioSetupSnapshot(
    WorkspaceManager workspaceManager,
    DocumentUri documentUri,
    string sourceFilePath)
{
    if (!workspaceManager.TryGetCompilation(documentUri, out var compilation) || compilation is null)
        throw new InvalidOperationException($"No compilation for '{sourceFilePath}'.");

    return compilation.PerformanceInstrumentation.Setup.CaptureSnapshot();
}

static Dictionary<string, SyntaxTree> GetTreesByPath(Compilation compilation)
    => compilation.SyntaxTrees
        .Where(static tree => !string.IsNullOrWhiteSpace(tree.FilePath))
        .GroupBy(static tree => Path.GetFullPath(tree.FilePath), StringComparer.OrdinalIgnoreCase)
        .ToDictionary(static group => group.Key, static group => group.First(), StringComparer.OrdinalIgnoreCase);

static IReadOnlyList<object> GetChangedExecutableOwners(Compilation compilation, SyntaxTree syntaxTree)
{
    var method = typeof(Compilation).GetMethod(
        "GetChangedExecutableOwnerDescriptorsForTesting",
        System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic);
    if (method?.Invoke(compilation, [syntaxTree]) is System.Collections.IEnumerable descriptors)
        return descriptors.Cast<object>().ToArray();

    return [];
}

static bool? GetSemanticDiagnosticTransferBlocked(Compilation compilation, SyntaxTree syntaxTree)
{
    var method = typeof(Compilation).GetMethod(
        "IsSemanticDiagnosticTransferBlocked",
        System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic);
    return method?.Invoke(compilation, [syntaxTree]) as bool?;
}

static bool TryReplaceFirst(SourceText sourceText, string oldText, string newText, out SourceText? updatedText, out TextSpan span)
{
    var text = sourceText.ToString();
    var start = text.IndexOf(oldText, StringComparison.Ordinal);
    if (start < 0)
    {
        updatedText = null;
        span = default;
        return false;
    }

    span = new TextSpan(start, oldText.Length);
    updatedText = sourceText.Replace(span, newText);
    return true;
}

static string FormatHoverResult(HoverResult result)
    => $"{result.Label} hover: {result.ElapsedMs:F1}ms {result.Line}:{result.Character} {result.Preview} " +
       $"[{SemanticQueryInstrumentation.FormatDelta(result.SemanticDelta)}] " +
       $"[{CompilerSetupInstrumentation.FormatDelta(result.SetupDelta)}] " +
       $"[{FunctionExpressionParameterInstrumentation.FormatDelta(result.FunctionExpressionParameterDelta)}]";

static string FormatEditScenarioHover(string scenarioName, EditScenarioMode mode, HoverResult result)
{
    var marker = result.Exception is not null
        ? "error"
        : !result.HasHover
            ? "null"
            : "ok";
    return $"edit-scenario-hover scenario={scenarioName} mode={FormatEditScenarioMode(mode)} marker={marker} {FormatHoverResult(result)}";
}

static string FormatEditScenarioMode(EditScenarioMode mode)
    => mode switch
    {
        EditScenarioMode.WarmDocumentDiagnostics => "warm-document-diagnostics",
        EditScenarioMode.ColdHoverThenDiagnostics => "cold-hover-then-diagnostics",
        _ => mode.ToString()
    };

static string FormatDiagnostic(OmniSharp.Extensions.LanguageServer.Protocol.Models.Diagnostic diagnostic)
{
    var code = diagnostic.Code?.ToString() ?? "<no-code>";
    var start = diagnostic.Range.Start;
    var end = diagnostic.Range.End;
    return $"{code} {start.Line}:{start.Character}-{end.Line}:{end.Character} {diagnostic.Message}";
}

static string FormatSymbol(ISymbol? symbol)
    => symbol is null
        ? "<null>"
        : $"{symbol.Kind}:{symbol.Name}/{GetSymbolParameterCount(symbol)}";

static string FormatCandidates(ImmutableArray<ISymbol> symbols)
    => symbols.IsDefaultOrEmpty
        ? string.Empty
        : string.Join(", ", symbols.Take(8).Select(FormatSymbol));

static string GetSymbolParameterCount(ISymbol symbol)
    => symbol switch
    {
        IMethodSymbol method => method.Parameters.Length.ToString(),
        INamedTypeSymbol type => "ctors:" + type.Constructors.Length,
        _ => "-"
    };

static string FindRepositoryRoot()
{
    var current = new DirectoryInfo(AppContext.BaseDirectory);
    while (current is not null)
    {
        if (File.Exists(Path.Combine(current.FullName, "Raven.sln")))
            return current.FullName;

        current = current.Parent;
    }

    throw new DirectoryNotFoundException("Could not locate Raven.sln.");
}

static HeadlessOptions ParseOptions(string[] args)
{
    var positionals = new List<string>();
    string? scenarioName = null;
    var listScenarios = false;
    var randomHover = false;
    var randomCount = 50;
    var randomSeed = 1729;
    var slowThresholdMs = 250.0;
    var hoverPositions = new List<PositionTarget>();
    var replayPerformanceReport = false;
    string? performanceReportPath = null;
    var replayCount = 50;
    var editReplacements = new List<EditReplacement>();
    var editHoverTargets = new List<string>();
    var editWaitMilliseconds = 0;
    var editSkipDiagnostics = false;
    RangeTarget? inlayRange = null;
    var inlayRepeatCount = 1;
    var projectSequence = false;
    var sequenceFiles = new List<string>();
    var sequenceRepeatCount = 1;
    var sequenceInlayProbes = true;
    var sequenceHoverCount = 5;
    var sequenceReopenDocuments = false;
    string? typeImportText = null;
    var stressSuite = false;
    var stressSizes = new List<int>();
    var stressIncludeAnalyzers = true;
    var stressIncludeInlays = true;
    var editScenarioSuite = false;
    var printDiagnostics = false;

    for (var i = 0; i < args.Length; i++)
    {
        switch (args[i])
        {
            case "--scenario" when i + 1 < args.Length:
                scenarioName = args[i + 1];
                i++;
                break;
            case "--list-scenarios":
                listScenarios = true;
                break;
            case "--random-hover":
                randomHover = true;
                break;
            case "--random-count" when i + 1 < args.Length && int.TryParse(args[i + 1], out var count):
                randomCount = Math.Max(1, count);
                i++;
                break;
            case "--random-seed" when i + 1 < args.Length && int.TryParse(args[i + 1], out var seed):
                randomSeed = seed;
                i++;
                break;
            case "--slow-ms" when i + 1 < args.Length && double.TryParse(args[i + 1], out var slowMs):
                slowThresholdMs = Math.Max(0, slowMs);
                i++;
                break;
            case "--replay-performance-report":
                replayPerformanceReport = true;
                break;
            case "--performance-report" when i + 1 < args.Length:
                replayPerformanceReport = true;
                performanceReportPath = args[i + 1];
                i++;
                break;
            case "--replay-count" when i + 1 < args.Length && int.TryParse(args[i + 1], out var parsedReplayCount):
                replayCount = Math.Max(1, parsedReplayCount);
                i++;
                break;
            case "--position" when i + 1 < args.Length && TryParsePosition(args[i + 1], out var targetPosition):
                hoverPositions.Add(targetPosition);
                i++;
                break;
            case "--inlay-range" when i + 1 < args.Length && TryParseRange(args[i + 1], out var targetRange):
                inlayRange = targetRange;
                i++;
                break;
            case "--inlay-repeat" when i + 1 < args.Length && int.TryParse(args[i + 1], out var parsedInlayRepeatCount):
                inlayRepeatCount = Math.Max(1, parsedInlayRepeatCount);
                i++;
                break;
            case "--project-sequence":
                projectSequence = true;
                break;
            case "--sequence-file" when i + 1 < args.Length:
                projectSequence = true;
                sequenceFiles.Add(args[i + 1]);
                i++;
                break;
            case "--sequence-repeat" when i + 1 < args.Length && int.TryParse(args[i + 1], out var parsedSequenceRepeatCount):
                sequenceRepeatCount = Math.Max(1, parsedSequenceRepeatCount);
                i++;
                break;
            case "--sequence-hover-count" when i + 1 < args.Length && int.TryParse(args[i + 1], out var parsedSequenceHoverCount):
                sequenceHoverCount = Math.Max(0, parsedSequenceHoverCount);
                i++;
                break;
            case "--no-sequence-inlays":
                sequenceInlayProbes = false;
                break;
            case "--sequence-reopen":
                sequenceReopenDocuments = true;
                break;
            case "--edit-replace" when i + 2 < args.Length:
                editReplacements.Add(new EditReplacement(args[i + 1], args[i + 2]));
                i += 2;
                break;
            case "--edit-hover" when i + 1 < args.Length:
                editHoverTargets.Add(args[i + 1]);
                i++;
                break;
            case "--edit-wait-ms" when i + 1 < args.Length && int.TryParse(args[i + 1], out var parsedEditWaitMilliseconds):
                editWaitMilliseconds = Math.Max(0, parsedEditWaitMilliseconds);
                i++;
                break;
            case "--edit-skip-diagnostics":
                editSkipDiagnostics = true;
                break;
            case "--type-import":
                typeImportText = i + 1 < args.Length && !args[i + 1].StartsWith("--", StringComparison.Ordinal)
                    ? args[++i]
                    : "import System.Net.";
                break;
            case "--stress-suite":
                stressSuite = true;
                break;
            case "--stress-size" when i + 1 < args.Length && int.TryParse(args[i + 1], out var parsedStressSize):
                stressSuite = true;
                stressSizes.Add(Math.Max(0, parsedStressSize));
                i++;
                break;
            case "--stress-no-analyzers":
                stressSuite = true;
                stressIncludeAnalyzers = false;
                break;
            case "--stress-no-inlays":
                stressSuite = true;
                stressIncludeInlays = false;
                break;
            case "--edit-scenario-suite":
                editScenarioSuite = true;
                break;
            case "--print-diagnostics":
                printDiagnostics = true;
                break;
            default:
                positionals.Add(args[i]);
                break;
        }
    }

    return new HeadlessOptions(
        positionals,
        scenarioName,
        listScenarios,
        randomHover,
        randomCount,
        randomSeed,
        slowThresholdMs,
        hoverPositions,
        replayPerformanceReport,
        performanceReportPath,
        replayCount,
        editReplacements,
        editHoverTargets,
        editWaitMilliseconds,
        editSkipDiagnostics,
        inlayRange,
        inlayRepeatCount,
        projectSequence,
        sequenceFiles,
        sequenceRepeatCount,
        sequenceInlayProbes,
        sequenceHoverCount,
        sequenceReopenDocuments,
        typeImportText,
        stressSuite,
        stressSizes,
        stressIncludeAnalyzers,
        stressIncludeInlays,
        editScenarioSuite,
        printDiagnostics);
}

static NamedReplayScenario ResolveReplayScenario(string repoRoot, string name)
{
    var scenario = CreateReplayScenarios(repoRoot)
        .FirstOrDefault(candidate => string.Equals(candidate.Name, name, StringComparison.OrdinalIgnoreCase));
    return scenario ?? throw new ArgumentException(
        $"Unknown replay scenario '{name}'. Use --list-scenarios to see available scenarios.",
        nameof(name));
}

static IReadOnlyList<NamedReplayScenario> CreateReplayScenarios(string repoRoot)
{
    var samplesRoot = Path.Combine(repoRoot, "samples", "projects");

    string Project(string name)
        => Path.Combine(samplesRoot, name);

    string Source(string projectName, params string[] path)
        => Path.Combine([Project(projectName), .. path]);

    return
    [
        new(
            "efcore-expression-trees-project",
            "Project replay for IQueryable extension methods, pipe expressions, and expression lambdas.",
            Project("efcore-expression-trees"),
            Source("efcore-expression-trees", "src", "main.rvn"),
            ReplayScenarioOperation.ProjectSequence,
            []),
        new(
            "efcore-expression-trees-lambda-hover",
            "Single cold hover over a lambda parameter inside IQueryable extension method resolution.",
            Project("efcore-expression-trees"),
            Source("efcore-expression-trees", "src", "main.rvn"),
            ReplayScenarioOperation.HoverPositions,
            [new PositionTarget(23, 12, "onlyActiveAdults.lambda.user")]),
        new(
            "efcore-expression-trees-diagnostics",
            "Single cold compiler-diagnostics pass for the IQueryable expression-tree sample.",
            Project("efcore-expression-trees"),
            Source("efcore-expression-trees", "src", "main.rvn"),
            ReplayScenarioOperation.DocumentDiagnostics,
            []),
        new(
            "vehicle-costs-project",
            "Multi-file project replay for EF Core, minimal API, extension methods, and lambdas.",
            Project("efcore-vehicle-costs"),
            Source("efcore-vehicle-costs", "src", "Api", "Main.rvn"),
            ReplayScenarioOperation.ProjectSequence,
            []),
        new(
            "vehicle-costs-lambda-hover",
            "Single cold hover over the first EF Core lambda parameter in the minimal API sample.",
            Project("efcore-vehicle-costs"),
            Source("efcore-vehicle-costs", "src", "Api", "Main.rvn"),
            ReplayScenarioOperation.HoverPositions,
            [new PositionTarget(31, 32, "vehicles.include.lambda.vehicle")]),
        new(
            "vehicle-costs-diagnostics",
            "Single cold compiler-diagnostics pass for the EF Core minimal API main file.",
            Project("efcore-vehicle-costs"),
            Source("efcore-vehicle-costs", "src", "Api", "Main.rvn"),
            ReplayScenarioOperation.DocumentDiagnostics,
            []),
        new(
            "fulfillment-workflow-project",
            "Project replay for large result-propagation and workflow code.",
            Project("fulfillment-workflow"),
            Source("fulfillment-workflow", "src", "main.rvn"),
            ReplayScenarioOperation.ProjectSequence,
            []),
        new(
            "fulfillment-workflow-propagation-hover",
            "Single cold hover near a result-propagation expression in the fulfillment workflow sample.",
            Project("fulfillment-workflow"),
            Source("fulfillment-workflow", "src", "main.rvn"),
            ReplayScenarioOperation.HoverPositions,
            [new PositionTarget(10, 28, "batchResult.propagation")]),
        new(
            "fulfillment-workflow-diagnostics",
            "Single cold compiler-diagnostics pass for the fulfillment workflow sample.",
            Project("fulfillment-workflow"),
            Source("fulfillment-workflow", "src", "main.rvn"),
            ReplayScenarioOperation.DocumentDiagnostics,
            []),
        new(
            "top-level-members-project",
            "Project replay for top-level members, imports, and namespace-member lookup.",
            Project("top-level-members"),
            Source("top-level-members", "src", "Members.rvn"),
            ReplayScenarioOperation.ProjectSequence,
            []),
        new(
            "top-level-members-diagnostics",
            "Single cold compiler-diagnostics pass for top-level member binding.",
            Project("top-level-members"),
            Source("top-level-members", "src", "Members.rvn"),
            ReplayScenarioOperation.DocumentDiagnostics,
            [])
    ];
}

static IReadOnlyList<EditScenario> CreateEditScenarios(string repoRoot)
{
    var samplesRoot = Path.Combine(repoRoot, "samples", "projects");

    string Project(string name)
        => Path.Combine(samplesRoot, name);

    string Source(string projectName, params string[] path)
        => Path.Combine([Project(projectName), .. path]);

    return
    [
        new(
            "mock-ui-argument-name-remove",
            "Remove an explicit argument name from a top-level DSL invocation.",
            Project("mock-ui-builder-dsl"),
            Source("mock-ui-builder-dsl", "src", "Main.rvn"),
            [new EditReplacement("Window(title: \"Tasks\")", "Window(\"Tasks\")")],
            [new("Window(", "Window"), new("Label(\"Inbox\")", "Label"), new("viewModel", "viewModel")]),
        new(
            "mock-ui-argument-name-add-combined",
            "Add explicit argument names to multiple DSL invocations in one edit batch.",
            Project("mock-ui-builder-dsl"),
            Source("mock-ui-builder-dsl", "src", "Main.rvn"),
            [
                new EditReplacement("Label(\"Inbox\")", "Label(text: \"Inbox\")"),
                new EditReplacement("Button(\"Add\")", "Button(text: \"Add\")")
            ],
            [new("Label(text:", "Label"), new("Button(text:", "Button"), new("viewModel", "viewModel")]),
        new(
            "expression-trees-body-edit",
            "Edit a method body and then query IQueryable extension and lambda hovers.",
            Project("efcore-expression-trees"),
            Source("efcore-expression-trees", "src", "main.rvn"),
            [new EditReplacement("Console.WriteLine(\"Matched users count: ${names.Count.ToString()}\")", "Console.WriteLine(value: \"Matched users count: ${names.Count.ToString()}\")")],
            [new("OrderBy(", "OrderBy"), new("Select(", "Select"), new("user => user.Name", "lambda-user")]),
        new(
            "vehicle-costs-body-edit",
            "Edit a minimal API route body and then query EF Core extension and lambda hovers.",
            Project("efcore-vehicle-costs"),
            Source("efcore-vehicle-costs", "src", "Api", "Main.rvn"),
            [new EditReplacement("app.MapGet(\"/vehicles\",", "app.MapGet(\"/vehicles/\",")],
            [new("OrderBy(", "OrderBy"), new("vehicle => vehicle.RegistrationNumber", "lambda-vehicle"), new("Vehicles", "Vehicles")]),
        new(
            "fulfillment-named-argument",
            "Add an explicit argument name inside the result-propagation workflow.",
            Project("fulfillment-workflow"),
            Source("fulfillment-workflow", "src", "main.rvn"),
            [new EditReplacement("PrintPlan(plan)", "PrintPlan(plan: plan)")],
            [new("PrintPlan(", "PrintPlan"), new("planResult?", "planResult"), new("plan: plan", "plan")]),
        new(
            "fulfillment-combined-body-edits",
            "Apply multiple body edits in the workflow sample to model batched editor changes.",
            Project("fulfillment-workflow"),
            Source("fulfillment-workflow", "src", "main.rvn"),
            [
                new EditReplacement("PrintPlan(plan)", "PrintPlan(plan: plan)"),
                new EditReplacement("WriteLine(\"== Raven Fulfillment Workflow ==\")", "WriteLine(value: \"== Raven Fulfillment Workflow ==\")")
            ],
            [new("PrintPlan(", "PrintPlan"), new("plan: plan", "plan")])
    ];
}

static bool TryParsePosition(string text, out PositionTarget position)
{
    position = default;
    var parts = text.Split(':', 2);
    if (parts.Length != 2 ||
        !int.TryParse(parts[0], out var line) ||
        !int.TryParse(parts[1], out var character))
    {
        return false;
    }

    position = new PositionTarget(line, character, text);
    return true;
}

static bool TryParseRange(string text, out RangeTarget range)
{
    range = default;
    var parts = text.Split('-', 2);
    if (parts.Length != 2 ||
        !TryParsePosition(parts[0], out var start) ||
        !TryParsePosition(parts[1], out var end))
    {
        return false;
    }

    range = new RangeTarget(start.Line, start.Character, end.Line, end.Character, text);
    return true;
}

static IReadOnlyList<PositionTarget> ReadHoverTargetsFromPerformanceReport(string reportPath, string sourceFilePath)
{
    var sourceUri = DocumentUri.FromFileSystemPath(sourceFilePath).ToString();
    var targets = new List<PositionTarget>();
    var inHoverOperation = false;

    foreach (var line in File.ReadLines(reportPath))
    {
        if (line.StartsWith("Operation: ", StringComparison.Ordinal))
        {
            inHoverOperation = string.Equals(line.Trim(), "Operation: hover", StringComparison.Ordinal);
            continue;
        }

        if (!inHoverOperation ||
            (!line.Contains(sourceUri, StringComparison.Ordinal) &&
             !line.Contains(sourceFilePath, StringComparison.Ordinal)))
        {
            continue;
        }

        var match = Regex.Match(line, @"(?<line>\d+):(?<character>\d+)\b");
        if (!match.Success)
            continue;

        var sourceLine = int.Parse(match.Groups["line"].Value);
        var sourceCharacter = int.Parse(match.Groups["character"].Value);
        targets.Add(new PositionTarget(sourceLine, sourceCharacter, $"{sourceLine}:{sourceCharacter}"));
    }

    return targets
        .GroupBy(static target => (target.Line, target.Character))
        .Select(static group => group.First())
        .ToArray();
}

internal sealed record HeadlessOptions(
    IReadOnlyList<string> Positionals,
    string? ScenarioName,
    bool ListScenarios,
    bool RandomHover,
    int RandomCount,
    int RandomSeed,
    double SlowThresholdMs,
    IReadOnlyList<PositionTarget> HoverPositions,
    bool ReplayPerformanceReport,
    string? PerformanceReportPath,
    int ReplayCount,
    IReadOnlyList<EditReplacement> EditReplacements,
    IReadOnlyList<string> EditHoverTargets,
    int EditWaitMilliseconds,
    bool EditSkipDiagnostics,
    RangeTarget? InlayRange,
    int InlayRepeatCount,
    bool ProjectSequence,
    IReadOnlyList<string> SequenceFiles,
    int SequenceRepeatCount,
    bool SequenceInlayProbes,
    int SequenceHoverCount,
    bool SequenceReopenDocuments,
    string? TypeImportText,
    bool StressSuite,
    IReadOnlyList<int> StressSizes,
    bool StressIncludeAnalyzers,
    bool StressIncludeInlays,
    bool EditScenarioSuite,
    bool PrintDiagnostics);

internal enum ReplayScenarioOperation
{
    ProjectSequence,
    DocumentDiagnostics,
    HoverPositions
}

internal sealed record NamedReplayScenario(
    string Name,
    string Description,
    string ProjectRoot,
    string FilePath,
    ReplayScenarioOperation Operation,
    IReadOnlyList<PositionTarget> HoverPositions);

internal sealed record EditScenario(
    string Name,
    string Description,
    string ProjectRoot,
    string FilePath,
    IReadOnlyList<EditReplacement> Replacements,
    IReadOnlyList<EditHoverTarget> HoverTargets);

internal enum EditScenarioMode
{
    WarmDocumentDiagnostics,
    ColdHoverThenDiagnostics
}

internal readonly record struct EditHoverTarget(
    string SearchText,
    string Label);

internal readonly record struct EditReplacement(
    string OldText,
    string NewText)
{
    public string Label
    {
        get
        {
            var firstLine = OldText.Split('\n', 2)[0];
            return firstLine.Length <= 60 ? firstLine : firstLine[..60] + "...";
        }
    }
}

internal readonly record struct PositionTarget(
    int Line,
    int Character,
    string Label);

internal readonly record struct RangeTarget(
    int StartLine,
    int StartCharacter,
    int EndLine,
    int EndCharacter,
    string Label);

internal sealed record HoverResult(
    string Label,
    int Line,
    int Character,
    double ElapsedMs,
    bool HasHover,
    Exception? Exception,
    string Preview,
    SemanticQueryInstrumentation.Snapshot SemanticDelta,
    CompilerSetupInstrumentation.Snapshot SetupDelta,
    FunctionExpressionParameterInstrumentation.Snapshot FunctionExpressionParameterDelta);

internal sealed class HeadlessConsoleLogger<T> : ILogger<T>
{
    public IDisposable BeginScope<TState>(TState state)
        where TState : notnull
        => NoopScope.Instance;

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

        var message = formatter(state, exception);
        if (string.IsNullOrWhiteSpace(message))
            return;

        Console.WriteLine($"{logLevel}: {typeof(T).Name}: {message}");
        if (exception is not null)
            Console.WriteLine(exception);
    }

    private sealed class NoopScope : IDisposable
    {
        public static readonly NoopScope Instance = new();

        public void Dispose()
        {
        }
    }
}

internal sealed class HeadlessWorkspaceEventSink : IWorkspaceEventSink
{
    public void Report(WorkspaceEvent workspaceEvent)
    {
        if (workspaceEvent.ElapsedMilliseconds < 50)
            return;

        Console.WriteLine(
            $"workspace {workspaceEvent.Operation}: {workspaceEvent.ElapsedMilliseconds:F1}ms " +
            $"project={workspaceEvent.ProjectName ?? "<none>"} " +
            $"document={workspaceEvent.DocumentPath ?? "<none>"} " +
            $"{workspaceEvent.Detail}");
    }
}
