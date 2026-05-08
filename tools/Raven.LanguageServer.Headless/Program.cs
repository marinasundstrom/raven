using System.Diagnostics;
using System.Collections.Immutable;
using System.Text.RegularExpressions;

using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;
using Raven.LanguageServer;

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

var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
_ = store.UpsertDocument(uri, text);
var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None)
    ?? throw new InvalidOperationException($"No analysis context for '{filePath}'.");
var sourceText = context.SourceText;
var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);
var model = await store.GetSemanticModelAsync(uri, CancellationToken.None)
    ?? throw new InvalidOperationException($"No semantic model for '{filePath}'.");
var root = context.SyntaxTree.GetRoot();

if (options.RandomHover)
{
    await RunRandomHoversAsync();
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

static string FormatHoverResult(HoverResult result)
    => $"{result.Label} hover: {result.ElapsedMs:F1}ms {result.Line}:{result.Character} {result.Preview} [{SemanticQueryInstrumentation.FormatDelta(result.SemanticDelta)}]";

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
        replayCount);
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
    int ReplayCount);

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
