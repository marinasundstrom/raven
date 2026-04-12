using System.Collections.Concurrent;
using System.Diagnostics;
using System.Text;

using OmniSharp.Extensions.LanguageServer.Protocol;

namespace Raven.LanguageServer;

internal static class LanguageServerPerformanceInstrumentation
{
#if RAVEN_INSTRUMENTATION
    private static readonly ConcurrentDictionary<string, OperationAggregate> s_operations = new(StringComparer.Ordinal);
    private static readonly ConcurrentDictionary<string, DocumentEditStamp> s_latestDocumentEdits = new(StringComparer.Ordinal);
    private static readonly object s_reportSync = new();
    private static string? s_reportPath;
    private static long s_lastFlushTimestamp;
    private const int FlushIntervalMilliseconds = 1000;
#endif

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal static void Reset()
    {
#if RAVEN_INSTRUMENTATION
        s_operations.Clear();
        s_latestDocumentEdits.Clear();
        lock (s_reportSync)
            s_lastFlushTimestamp = 0;
#endif
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal static void ConfigureReportPath(string reportPath)
    {
#if RAVEN_INSTRUMENTATION
        lock (s_reportSync)
            s_reportPath = reportPath;
#endif
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal static void RecordDocumentEdit(DocumentUri uri, int? version, string source)
    {
#if RAVEN_INSTRUMENTATION
        s_latestDocumentEdits[uri.ToString()] = new DocumentEditStamp(Stopwatch.GetTimestamp(), source);
        TryFlushToDisk();
#endif
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal static void RecordOperation(
        string operation,
        DocumentUri? uri,
        int? version,
        double totalMs,
        bool cacheHit = false,
        int? resultCount = null,
        params StageTiming[] stages)
    {
#if RAVEN_INSTRUMENTATION
        double? firstLookupAfterEditMs = null;
        string? editSource = null;

        if (uri is not null && IsLookupOperation(operation))
        {
            if (s_latestDocumentEdits.TryGetValue(uri.ToString(), out var editStamp) &&
                editStamp.TryCaptureFirstLookup(out var elapsedMs))
            {
                firstLookupAfterEditMs = elapsedMs;
                editSource = editStamp.Source;
            }
        }

        var aggregate = s_operations.GetOrAdd(operation, static name => new OperationAggregate(name));
        aggregate.Record(totalMs, stages, cacheHit, resultCount, firstLookupAfterEditMs, editSource);
        TryFlushToDisk();
#endif
    }

    internal static string CreateReport()
    {
#if RAVEN_INSTRUMENTATION
        var builder = new StringBuilder();
        builder.AppendLine("Raven Language Server Performance Report");
        builder.AppendLine($"Generated: {DateTimeOffset.UtcNow:O}");

        foreach (var aggregate in s_operations.Values.OrderByDescending(static entry => entry.TotalMilliseconds))
            builder.Append(aggregate.FormatReportSection());

        if (s_operations.IsEmpty)
            builder.AppendLine("No language server performance data recorded.");

        return builder.ToString();
#else
        return "Raven language server performance instrumentation disabled.";
#endif
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal static void FlushToDisk()
    {
#if RAVEN_INSTRUMENTATION
        var reportPath = GetReportPath();
        if (string.IsNullOrWhiteSpace(reportPath))
            return;

        var report = CreateReport();
        Directory.CreateDirectory(Path.GetDirectoryName(reportPath)!);
        File.WriteAllText(reportPath, report);
#endif
    }

    private static bool IsLookupOperation(string operation)
        => string.Equals(operation, "hover", StringComparison.Ordinal) ||
           string.Equals(operation, "completion", StringComparison.Ordinal) ||
           string.Equals(operation, "definition", StringComparison.Ordinal) ||
           string.Equals(operation, "signatureHelp", StringComparison.Ordinal) ||
           string.Equals(operation, "references", StringComparison.Ordinal) ||
           string.Equals(operation, "documentSymbols", StringComparison.Ordinal);

#if RAVEN_INSTRUMENTATION
    private static void TryFlushToDisk()
    {
        var reportPath = GetReportPath();
        if (string.IsNullOrWhiteSpace(reportPath))
            return;

        var now = Stopwatch.GetTimestamp();
        lock (s_reportSync)
        {
            if (s_lastFlushTimestamp != 0 &&
                Stopwatch.GetElapsedTime(s_lastFlushTimestamp, now).TotalMilliseconds < FlushIntervalMilliseconds)
            {
                return;
            }

            s_lastFlushTimestamp = now;
        }

        var report = CreateReport();
        Directory.CreateDirectory(Path.GetDirectoryName(reportPath)!);
        File.WriteAllText(reportPath, report);
    }

    private static string? GetReportPath()
    {
        lock (s_reportSync)
            return s_reportPath;
    }

    internal readonly record struct StageTiming(string Name, double ElapsedMs);

    private sealed class DocumentEditStamp
    {
        private readonly long _timestamp;
        private int _firstLookupCaptured;

        public DocumentEditStamp(long timestamp, string source)
        {
            _timestamp = timestamp;
            Source = source;
        }

        public string Source { get; }

        public bool TryCaptureFirstLookup(out double elapsedMs)
        {
            elapsedMs = 0;

            if (Interlocked.Exchange(ref _firstLookupCaptured, 1) != 0)
                return false;

            elapsedMs = Stopwatch.GetElapsedTime(_timestamp).TotalMilliseconds;
            return true;
        }
    }

    private sealed class OperationAggregate
    {
        private readonly object _sync = new();
        private readonly Dictionary<string, StageAggregate> _stages = new(StringComparer.Ordinal);
        private readonly Dictionary<string, int> _firstLookupSources = new(StringComparer.Ordinal);

        private int _count;
        private int _cacheHits;
        private int _resultCountSamples;
        private int _resultCountTotal;
        private int _firstLookupCount;
        private double _totalMilliseconds;
        private double _maxMilliseconds;
        private double _firstLookupAfterEditTotalMilliseconds;
        private double _firstLookupAfterEditMaxMilliseconds;

        public OperationAggregate(string name)
        {
            Name = name;
        }

        public string Name { get; }

        public double TotalMilliseconds
        {
            get
            {
                lock (_sync)
                    return _totalMilliseconds;
            }
        }

        public void Record(
            double totalMs,
            StageTiming[] stages,
            bool cacheHit,
            int? resultCount,
            double? firstLookupAfterEditMs,
            string? editSource)
        {
            lock (_sync)
            {
                _count++;
                _totalMilliseconds += totalMs;
                _maxMilliseconds = Math.Max(_maxMilliseconds, totalMs);

                if (cacheHit)
                    _cacheHits++;

                if (resultCount is { } count)
                {
                    _resultCountSamples++;
                    _resultCountTotal += count;
                }

                if (firstLookupAfterEditMs is { } lookupMs)
                {
                    _firstLookupCount++;
                    _firstLookupAfterEditTotalMilliseconds += lookupMs;
                    _firstLookupAfterEditMaxMilliseconds = Math.Max(_firstLookupAfterEditMaxMilliseconds, lookupMs);

                    if (!string.IsNullOrWhiteSpace(editSource))
                        _firstLookupSources[editSource!] = _firstLookupSources.GetValueOrDefault(editSource!) + 1;
                }

                foreach (var stage in stages)
                {
                    if (!_stages.TryGetValue(stage.Name, out var aggregate))
                    {
                        aggregate = new StageAggregate();
                        _stages[stage.Name] = aggregate;
                    }

                    aggregate.Count++;
                    aggregate.TotalMilliseconds += stage.ElapsedMs;
                    aggregate.MaxMilliseconds = Math.Max(aggregate.MaxMilliseconds, stage.ElapsedMs);
                }
            }
        }

        public string FormatReportSection()
        {
            lock (_sync)
            {
                var builder = new StringBuilder();
                builder.AppendLine($"Operation: {Name}");
                builder.AppendLine($"  Count: {_count}");
                builder.AppendLine($"  TotalMs: {_totalMilliseconds:F1}");
                builder.AppendLine($"  AvgMs: {(_count == 0 ? 0 : _totalMilliseconds / _count):F1}");
                builder.AppendLine($"  MaxMs: {_maxMilliseconds:F1}");

                if (_cacheHits > 0)
                    builder.AppendLine($"  CacheHits: {_cacheHits}");

                if (_resultCountSamples > 0)
                    builder.AppendLine($"  AvgResultCount: {(double)_resultCountTotal / _resultCountSamples:F1}");

                if (_firstLookupCount > 0)
                {
                    builder.AppendLine($"  FirstLookupAfterEditCount: {_firstLookupCount}");
                    builder.AppendLine($"  FirstLookupAfterEditAvgMs: {_firstLookupAfterEditTotalMilliseconds / _firstLookupCount:F1}");
                    builder.AppendLine($"  FirstLookupAfterEditMaxMs: {_firstLookupAfterEditMaxMilliseconds:F1}");

                    if (_firstLookupSources.Count > 0)
                    {
                        builder.AppendLine("  FirstLookupAfterEditSources:");
                        foreach (var source in _firstLookupSources.OrderByDescending(static pair => pair.Value))
                            builder.AppendLine($"    {source.Key}: {source.Value}");
                    }
                }

                if (_stages.Count > 0)
                {
                    builder.AppendLine("  Stages:");
                    foreach (var stage in _stages.OrderByDescending(static pair => pair.Value.TotalMilliseconds))
                    {
                        builder.AppendLine(
                            $"    {stage.Key}: count={stage.Value.Count} total={stage.Value.TotalMilliseconds:F1}ms avg={stage.Value.TotalMilliseconds / stage.Value.Count:F1}ms max={stage.Value.MaxMilliseconds:F1}ms");
                    }
                }

                return builder.AppendLine().ToString();
            }
        }
    }

    private sealed class StageAggregate
    {
        public int Count { get; set; }

        public double TotalMilliseconds { get; set; }

        public double MaxMilliseconds { get; set; }
    }
#else
    internal readonly record struct StageTiming(string Name, double ElapsedMs);
#endif
}
