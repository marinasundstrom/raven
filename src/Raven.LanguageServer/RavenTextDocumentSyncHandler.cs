using System.Collections.Concurrent;
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
    private const int DiagnosticsDebounceMilliseconds = 250;

    private readonly DocumentStore _documents;
    private readonly ILanguageServerFacade _languageServer;
    private readonly ILogger<RavenTextDocumentSyncHandler> _logger;
    private readonly ConcurrentDictionary<DocumentUri, CancellationTokenSource> _pendingDiagnostics = new();

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
            return ScheduleDiagnosticsPublishAsync(notification.TextDocument.Uri, cancellationToken);
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
            CancelPendingDiagnostics(notification.TextDocument.Uri);
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

    private Task<Unit> ScheduleDiagnosticsPublishAsync(DocumentUri uri, CancellationToken cancellationToken)
    {
        var source = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken);
        var current = _pendingDiagnostics.AddOrUpdate(
            uri,
            source,
            (_, previous) =>
            {
                previous.Cancel();
                previous.Dispose();
                return source;
            });

        if (!ReferenceEquals(current, source))
        {
            source.Dispose();
            return Task.FromResult(Unit.Value);
        }

        _ = Task.Run(async () =>
        {
            try
            {
                await Task.Delay(DiagnosticsDebounceMilliseconds, source.Token).ConfigureAwait(false);
                await PublishDiagnosticsAsync(uri, source.Token).ConfigureAwait(false);
            }
            catch (OperationCanceledException)
            {
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Debounced diagnostics publish failed for {Uri}.", uri);
            }
            finally
            {
                if (_pendingDiagnostics.TryGetValue(uri, out var active) && ReferenceEquals(active, source))
                    _pendingDiagnostics.TryRemove(uri, out _);

                source.Dispose();
            }
        }, CancellationToken.None);

        return Task.FromResult(Unit.Value);
    }

    private void CancelPendingDiagnostics(DocumentUri uri)
    {
        if (_pendingDiagnostics.TryRemove(uri, out var pending))
        {
            pending.Cancel();
            pending.Dispose();
        }
    }

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
