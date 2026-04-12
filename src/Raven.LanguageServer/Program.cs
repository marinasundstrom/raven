using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;

using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Server;

using Raven.CodeAnalysis;

using OmniLanguageServer = OmniSharp.Extensions.LanguageServer.Server.LanguageServer;

namespace Raven.LanguageServer;

internal static class Program
{
    static async Task Main(string[] args)
    {
        var logPath = ResolveLogPath();
        var performanceReportPath = Path.Combine(Path.GetDirectoryName(logPath) ?? Directory.GetCurrentDirectory(), "raven-lsp-performance.txt");
        LanguageServerPerformanceInstrumentation.ConfigureReportPath(performanceReportPath);
        var fileLoggerProvider = new RavenFileLoggerProvider(logPath);
        WriteStartupMarker(fileLoggerProvider, logPath, args);
        AppDomain.CurrentDomain.ProcessExit += (_, _) => WriteProcessExitMarker(fileLoggerProvider, "AppDomain.ProcessExit");

        var server = await OmniLanguageServer.From(options =>
        {
            options
            .WithInput(Console.OpenStandardInput())
            .WithOutput(Console.OpenStandardOutput())
            .WithConfigurationItem(new ConfigurationItem
            {
                Section = "raven"
            })
            .WithServices(services =>
            {
                services.AddSingleton(_ => RavenWorkspace.Create());
                services.AddSingleton<WorkspaceManager>();
                services.AddSingleton<DocumentStore>();
                services.AddSingleton<HoverHandler>();
            })
            .OnInitialize((server, request, _) =>
            {
                var workspaceManager = server.Services.GetRequiredService<WorkspaceManager>();
                workspaceManager.Initialize(request);
                return Task.CompletedTask;
            })
            .WithHandler<RavenTextDocumentSyncHandler>()
            .WithHandler<DidChangeWatchedFilesHandler>()
            .WithHandler<CompletionHandler>()
            .WithHandler<SignatureHelpHandler>()
            .WithHandler<DefinitionHandler>()
            .WithHandler<ReferencesHandler>()
            .WithHandler<RenameHandler>()
            .WithHandler<HoverHandler>()
            .WithHandler<SemanticTokensHandler>()
            .WithHandler<DocumentSymbolHandler>()
            .WithHandler<WorkspaceSymbolsHandler>()
            .WithHandler<CodeActionHandler>()
            .ConfigureLogging(logging =>
            {
                logging.AddLanguageProtocolLogging()
                    .AddProvider(fileLoggerProvider)
                    .SetMinimumLevel(LogLevel.Debug);
            });
        }).ConfigureAwait(false);

        try
        {
            await server.WaitForExit.ConfigureAwait(false);
        }
        finally
        {
            WritePerformanceReport(fileLoggerProvider, logPath);
            WriteProcessExitMarker(fileLoggerProvider, "Main.Finally");
        }
    }

    private static void WriteStartupMarker(RavenFileLoggerProvider loggerProvider, string logPath, string[] args)
    {
        var process = Environment.ProcessId;
        var cwd = Directory.GetCurrentDirectory();
        var baseDirectory = AppContext.BaseDirectory;
        var assemblyPath = Environment.ProcessPath ?? "<unknown>";
        var commandLine = Environment.CommandLine;
        var formattedArgs = args.Length == 0 ? "<none>" : string.Join(" ", args);

        loggerProvider.WriteLine(
            $"{DateTimeOffset.UtcNow:O} [Information] Raven.LanguageServer.Startup: " +
            $"Starting Raven language server. pid={process} cwd={cwd} baseDirectory={baseDirectory} " +
            $"processPath={assemblyPath} logPath={logPath} args={formattedArgs} commandLine={commandLine}");
    }

    private static void WriteProcessExitMarker(RavenFileLoggerProvider loggerProvider, string source)
    {
        loggerProvider.WriteLine(
            $"{DateTimeOffset.UtcNow:O} [Information] Raven.LanguageServer.Shutdown: " +
            $"Raven language server exiting. pid={Environment.ProcessId} source={source}");
    }

    private static void WritePerformanceReport(RavenFileLoggerProvider loggerProvider, string logPath)
    {
#if RAVEN_INSTRUMENTATION
        try
        {
            var logsDirectory = Path.GetDirectoryName(logPath) ?? Directory.GetCurrentDirectory();
            var reportPath = Path.Combine(logsDirectory, "raven-lsp-performance.txt");
            LanguageServerPerformanceInstrumentation.FlushToDisk();
            loggerProvider.WriteLine(
                $"{DateTimeOffset.UtcNow:O} [Information] Raven.LanguageServer.Performance: Wrote performance report to {reportPath}");
        }
        catch (Exception ex)
        {
            loggerProvider.WriteLine(
                $"{DateTimeOffset.UtcNow:O} [Warning] Raven.LanguageServer.Performance: Failed to write performance report.{Environment.NewLine}{ex}");
        }
#endif
    }

    private static string ResolveLogPath()
    {
        var roots = new[]
        {
            TryFindRepositoryRoot(Directory.GetCurrentDirectory()),
            TryFindRepositoryRoot(AppContext.BaseDirectory)
        };

        var root = roots.FirstOrDefault(static path => !string.IsNullOrWhiteSpace(path))
            ?? Directory.GetCurrentDirectory();

        var logsDirectory = Path.Combine(root, "logs");
        Directory.CreateDirectory(logsDirectory);
        return Path.Combine(logsDirectory, "raven-lsp.log");
    }

    private static string? TryFindRepositoryRoot(string startPath)
    {
        if (string.IsNullOrWhiteSpace(startPath))
            return null;

        var directory = Directory.Exists(startPath)
            ? new DirectoryInfo(startPath)
            : new DirectoryInfo(Path.GetDirectoryName(startPath)!);

        while (directory is not null)
        {
            if (File.Exists(Path.Combine(directory.FullName, "Raven.sln")))
                return directory.FullName;

            directory = directory.Parent;
        }

        return null;
    }
}

internal sealed class RavenFileLoggerProvider : ILoggerProvider
{
    private readonly StreamWriter _writer;
    private readonly TextWriter _errorWriter;
    private readonly object _sync = new();
    private bool _disposed;

    public RavenFileLoggerProvider(string path)
    {
        var stream = new FileStream(path, FileMode.Append, FileAccess.Write, FileShare.ReadWrite);
        _writer = new StreamWriter(stream) { AutoFlush = true };
        _errorWriter = Console.Error;
    }

    public ILogger CreateLogger(string categoryName)
        => new RavenFileLogger(categoryName, this);

    internal void WriteLine(string line)
    {
        lock (_sync)
        {
            if (_disposed)
                return;

            _writer.WriteLine(line);

            try
            {
                _errorWriter.WriteLine(line);
                _errorWriter.Flush();
            }
            catch
            {
                // Best-effort mirroring for VS Code output; file logging remains authoritative.
            }
        }
    }

    public void Dispose()
    {
        lock (_sync)
        {
            if (_disposed)
                return;

            _disposed = true;
            _writer.Dispose();
        }
    }
}

internal sealed class RavenFileLogger : ILogger
{
    private readonly string _category;
    private readonly RavenFileLoggerProvider _provider;

    public RavenFileLogger(string category, RavenFileLoggerProvider provider)
    {
        _category = category;
        _provider = provider;
    }

    public IDisposable BeginScope<TState>(TState state) where TState : notnull
        => NullScope.Instance;

    public bool IsEnabled(LogLevel logLevel)
        => logLevel != LogLevel.None;

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
        var timestamp = DateTimeOffset.UtcNow.ToString("O");
        var line = $"{timestamp} [{logLevel}] {_category}: {message}";

        if (exception is not null)
            line = $"{line}{Environment.NewLine}{exception}";

        _provider.WriteLine(line);
    }

    private sealed class NullScope : IDisposable
    {
        public static readonly NullScope Instance = new();
        public void Dispose()
        {
        }
    }
}
