using MediatR;

using Microsoft.Extensions.Logging;

using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Protocol.Server;
using OmniSharp.Extensions.LanguageServer.Protocol.Server.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Workspace;

using LspDiagnostic = OmniSharp.Extensions.LanguageServer.Protocol.Models.Diagnostic;
using LspDocumentUri = OmniSharp.Extensions.LanguageServer.Protocol.DocumentUri;
using LspFileSystemWatcher = OmniSharp.Extensions.LanguageServer.Protocol.Models.FileSystemWatcher;

namespace Raven.LanguageServer;

internal sealed class DidChangeWatchedFilesHandler : DidChangeWatchedFilesHandlerBase
{
    private readonly WorkspaceManager _workspaceManager;
    private readonly DocumentStore _documents;
    private readonly ILanguageServerFacade _languageServer;
    private readonly ILogger<DidChangeWatchedFilesHandler> _logger;

    public DidChangeWatchedFilesHandler(
        WorkspaceManager workspaceManager,
        DocumentStore documents,
        ILanguageServerFacade languageServer,
        ILogger<DidChangeWatchedFilesHandler> logger)
    {
        _workspaceManager = workspaceManager;
        _documents = documents;
        _languageServer = languageServer;
        _logger = logger;
    }

    public override async Task<Unit> Handle(DidChangeWatchedFilesParams request, CancellationToken cancellationToken)
    {
        try
        {
            var openDocumentsToRefresh = new HashSet<LspDocumentUri>(
                await _workspaceManager.ReloadForWatchedFilesAsync(request.Changes).ConfigureAwait(false));

            foreach (var uri in _workspaceManager.ApplyEditorConfigDiagnosticOptionsForWatchedFileChanges(request.Changes))
                openDocumentsToRefresh.Add(uri);

            if (openDocumentsToRefresh.Count > 0)
                await PublishDiagnosticsForOpenDocumentsAsync(openDocumentsToRefresh.ToArray(), cancellationToken).ConfigureAwait(false);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Failed to process watched file changes.");
        }

        return await Unit.Task;
    }

    private async Task PublishDiagnosticsForOpenDocumentsAsync(
        IReadOnlyList<LspDocumentUri> documentUris,
        CancellationToken cancellationToken)
    {
        foreach (var uri in documentUris)
        {
            var result = await _documents.TryGetDocumentWithAnalyzersDiagnosticsAsync(
                uri,
                shouldSkipWork: null,
                cancellationToken).ConfigureAwait(false);
            if (result.WasSkipped)
                continue;

            _languageServer.TextDocument.PublishDiagnostics(new PublishDiagnosticsParams
            {
                Uri = uri,
                Diagnostics = result.Diagnostics.Count > 0
                    ? new Container<LspDiagnostic>(result.Diagnostics)
                    : new Container<LspDiagnostic>(),
                Version = null
            });
        }
    }

    protected override DidChangeWatchedFilesRegistrationOptions CreateRegistrationOptions(DidChangeWatchedFilesCapability capability, ClientCapabilities clientCapabilities)
        => new()
        {
            Watchers = new Container<LspFileSystemWatcher>(
                new LspFileSystemWatcher
                {
                    GlobPattern = new GlobPattern("**/*.{rvn,rav,rvnproj,csproj,fsproj}"),
                    Kind = WatchKind.Create | WatchKind.Change | WatchKind.Delete
                },
                new LspFileSystemWatcher
                {
                    GlobPattern = new GlobPattern("**/.editorconfig"),
                    Kind = WatchKind.Create | WatchKind.Change | WatchKind.Delete
                })
        };
}
