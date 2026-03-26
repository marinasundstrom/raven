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
            })
            .OnInitialize((server, request, _) =>
            {
                var workspaceManager = server.Services.GetRequiredService<WorkspaceManager>();
                workspaceManager.Initialize(request);
                return Task.CompletedTask;
            })
            .WithHandler<RavenTextDocumentSyncHandler>()
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
                    .AddProvider(new RavenFileLoggerProvider(logPath))
                    .SetMinimumLevel(LogLevel.Debug);
            });
        }).ConfigureAwait(false);

        await server.WaitForExit.ConfigureAwait(false);
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
    private readonly object _sync = new();
    private bool _disposed;

    public RavenFileLoggerProvider(string path)
    {
        var stream = new FileStream(path, FileMode.Append, FileAccess.Write, FileShare.ReadWrite);
        _writer = new StreamWriter(stream) { AutoFlush = true };
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
