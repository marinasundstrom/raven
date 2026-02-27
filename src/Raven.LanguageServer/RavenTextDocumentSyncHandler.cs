using System.Linq;

using MediatR;

using Microsoft.Extensions.Logging;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Protocol.Server;

using SaveOptions = OmniSharp.Extensions.LanguageServer.Protocol.Server.Capabilities.SaveOptions;
using TextDocumentSelector = OmniSharp.Extensions.LanguageServer.Protocol.Models.TextDocumentSelector;
using TextDocumentSyncKind = OmniSharp.Extensions.LanguageServer.Protocol.Server.Capabilities.TextDocumentSyncKind;

namespace Raven.LanguageServer;

internal sealed class RavenTextDocumentSyncHandler : TextDocumentSyncHandlerBase
{
    private readonly DocumentStore _documents;
    private readonly ILanguageServerFacade _languageServer;
    private readonly ILogger<RavenTextDocumentSyncHandler> _logger;

    public RavenTextDocumentSyncHandler(DocumentStore documents, ILanguageServerFacade languageServer, ILogger<RavenTextDocumentSyncHandler> logger)
    {
        _documents = documents;
        _languageServer = languageServer;
        _logger = logger;
    }

    public override TextDocumentAttributes GetTextDocumentAttributes(DocumentUri uri)
        => new(uri, "raven");

    public override Task<Unit> Handle(DidOpenTextDocumentParams notification, CancellationToken cancellationToken)
    {
        try
        {
            _documents.UpsertDocument(notification.TextDocument.Uri, notification.TextDocument.Text);
            return PublishDiagnosticsAsync(notification.TextDocument.Uri, cancellationToken);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "DidOpen handling failed for {Uri}.", notification.TextDocument.Uri);
            return Task.FromResult(Unit.Value);
        }
    }

    public override Task<Unit> Handle(DidChangeTextDocumentParams notification, CancellationToken cancellationToken)
    {
        try
        {
            var change = notification.ContentChanges.LastOrDefault();
            var text = change?.Text ?? string.Empty;
            _documents.UpsertDocument(notification.TextDocument.Uri, text);
            return PublishDiagnosticsAsync(notification.TextDocument.Uri, cancellationToken);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "DidChange handling failed for {Uri}.", notification.TextDocument.Uri);
            return Task.FromResult(Unit.Value);
        }
    }

    public override Task<Unit> Handle(DidCloseTextDocumentParams notification, CancellationToken cancellationToken)
    {
        try
        {
            _documents.RemoveDocument(notification.TextDocument.Uri);
            _languageServer.TextDocument.PublishDiagnostics(new PublishDiagnosticsParams
            {
                Uri = notification.TextDocument.Uri,
                Diagnostics = new Container<Diagnostic>()
            });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "DidClose handling failed for {Uri}.", notification.TextDocument.Uri);
        }

        return Task.FromResult(Unit.Value);
    }

    public override Task<Unit> Handle(DidSaveTextDocumentParams notification, CancellationToken cancellationToken)
        => PublishDiagnosticsAsync(notification.TextDocument.Uri, cancellationToken);

    protected override TextDocumentSyncRegistrationOptions CreateRegistrationOptions(TextSynchronizationCapability capability, ClientCapabilities clientCapabilities)
        => new()
        {
            DocumentSelector = TextDocumentSelector.ForLanguage("raven"),
            Change = TextDocumentSyncKind.Full,
            Save = new SaveOptions
            {
                IncludeText = true
            }
        };

    private async Task<Unit> PublishDiagnosticsAsync(DocumentUri uri, CancellationToken cancellationToken)
    {
        try
        {
            var diagnostics = await _documents.GetDiagnosticsAsync(uri, cancellationToken).ConfigureAwait(false);
            _languageServer.TextDocument.PublishDiagnostics(new PublishDiagnosticsParams
            {
                Uri = uri,
                Diagnostics = new Container<Diagnostic>(diagnostics)
            });
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            return Unit.Value;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Publishing diagnostics failed for {Uri}.", uri);
        }

        return Unit.Value;
    }
}
