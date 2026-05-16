using System.Collections.Immutable;
using System.Diagnostics;
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
var options = ParseOptions(args);
var projectRoot = options.Positionals.Count > 0
    ? Path.GetFullPath(options.Positionals[0])
    : Path.Combine(repoRoot, "samples", "projects", "efcore-vehicle-costs");
var filePath = options.Positionals.Count > 1
    ? Path.GetFullPath(options.Positionals[1])
    : Path.Combine(projectRoot, "src", "Api", "Main.rvn");

var text = File.ReadAllText(filePath);
var uri = DocumentUri.FromFileSystemPath(filePath);

var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
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
var model = await store.GetSemanticModelAsync(uri, CancellationToken.None)
    ?? throw new InvalidOperationException($"No semantic model for '{filePath}'.");
var root = context.SyntaxTree.GetRoot();

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
    var hoverLines = hover?.Contents.MarkupContent?.Value.Split('\n') ?? ["<null>"];
    var firstLine = exception is null
        ? string.Join(" | ", hoverLines.Take(3))
        : exception.GetType().Name + ": " + exception.Message;

    return new HoverResult(label, position.Line, position.Character, sw.Elapsed.TotalMilliseconds, hover is not null, exception, firstLine, delta);
}

CompilerSetupInstrumentation.Snapshot CaptureSetupSnapshot()
{
    if (!manager.TryGetCompilation(uri, out var compilation) || compilation is null)
        throw new InvalidOperationException($"No compilation for '{filePath}'.");

    return compilation.PerformanceInstrumentation.Setup.CaptureSnapshot();
}

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
    => $"{result.Label} hover: {result.ElapsedMs:F1}ms {result.Line}:{result.Character} {result.Preview} [{SemanticQueryInstrumentation.FormatDelta(result.SemanticDelta)}]";

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

    for (var i = 0; i < args.Length; i++)
    {
        switch (args[i])
        {
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
            case "--edit-replace" when i + 2 < args.Length:
                editReplacements.Add(new EditReplacement(args[i + 1], args[i + 2]));
                i += 2;
                break;
            case "--edit-hover" when i + 1 < args.Length:
                editHoverTargets.Add(args[i + 1]);
                i++;
                break;
            default:
                positionals.Add(args[i]);
                break;
        }
    }

    return new HeadlessOptions(
        positionals,
        randomHover,
        randomCount,
        randomSeed,
        slowThresholdMs,
        hoverPositions,
        replayPerformanceReport,
        performanceReportPath,
        replayCount,
        editReplacements,
        editHoverTargets);
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
    bool RandomHover,
    int RandomCount,
    int RandomSeed,
    double SlowThresholdMs,
    IReadOnlyList<PositionTarget> HoverPositions,
    bool ReplayPerformanceReport,
    string? PerformanceReportPath,
    int ReplayCount,
    IReadOnlyList<EditReplacement> EditReplacements,
    IReadOnlyList<string> EditHoverTargets);

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

internal sealed record HoverResult(
    string Label,
    int Line,
    int Character,
    double ElapsedMs,
    bool HasHover,
    Exception? Exception,
    string Preview,
    SemanticQueryInstrumentation.Snapshot SemanticDelta);

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
