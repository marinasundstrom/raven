using MediatR;

using Microsoft.Extensions.Logging;

using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Protocol.Server.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Workspace;

namespace Raven.LanguageServer;

internal sealed class DidChangeWatchedFilesHandler : DidChangeWatchedFilesHandlerBase
{
    private readonly WorkspaceManager _workspaceManager;
    private readonly ILogger<DidChangeWatchedFilesHandler> _logger;

    public DidChangeWatchedFilesHandler(WorkspaceManager workspaceManager, ILogger<DidChangeWatchedFilesHandler> logger)
    {
        _workspaceManager = workspaceManager;
        _logger = logger;
    }

    public override Task<Unit> Handle(DidChangeWatchedFilesParams request, CancellationToken cancellationToken)
    {
        try
        {
            _workspaceManager.ReloadForWatchedFiles(request.Changes);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Failed to process watched file changes.");
        }

        return Unit.Task;
    }

    protected override DidChangeWatchedFilesRegistrationOptions CreateRegistrationOptions(DidChangeWatchedFilesCapability capability, ClientCapabilities clientCapabilities)
        => new()
        {
            Watchers = new Container<OmniSharp.Extensions.LanguageServer.Protocol.Models.FileSystemWatcher>(
                new OmniSharp.Extensions.LanguageServer.Protocol.Models.FileSystemWatcher
                {
                    GlobPattern = new GlobPattern("**/*.{rvn,rav,rvnproj,csproj,fsproj}"),
                    Kind = WatchKind.Create | WatchKind.Change | WatchKind.Delete
                })
        };
}
