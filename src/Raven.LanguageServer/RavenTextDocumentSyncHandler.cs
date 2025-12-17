using System.Linq;
using MediatR;
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

    public RavenTextDocumentSyncHandler(DocumentStore documents, ILanguageServerFacade languageServer)
    {
        _documents = documents;
        _languageServer = languageServer;
    }

    public override TextDocumentAttributes GetTextDocumentAttributes(DocumentUri uri)
        => new(uri, "raven");

    public override Task<Unit> Handle(DidOpenTextDocumentParams notification, CancellationToken cancellationToken)
    {
        _documents.UpsertDocument(notification.TextDocument.Uri, notification.TextDocument.Text);
        return PublishDiagnosticsAsync(notification.TextDocument.Uri, cancellationToken);
    }

    public override Task<Unit> Handle(DidChangeTextDocumentParams notification, CancellationToken cancellationToken)
    {
        var change = notification.ContentChanges.LastOrDefault();
        var text = change.Text ?? string.Empty;
        _documents.UpsertDocument(notification.TextDocument.Uri, text);
        return PublishDiagnosticsAsync(notification.TextDocument.Uri, cancellationToken);
    }

    public override Task<Unit> Handle(DidCloseTextDocumentParams notification, CancellationToken cancellationToken)
    {
        _languageServer.TextDocument.PublishDiagnostics(new PublishDiagnosticsParams
        {
            Uri = notification.TextDocument.Uri,
            Diagnostics = new Container<Diagnostic>()
        });

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
        var diagnostics = await _documents.GetDiagnosticsAsync(uri, cancellationToken).ConfigureAwait(false);
        _languageServer.TextDocument.PublishDiagnostics(new PublishDiagnosticsParams
        {
            Uri = uri,
            Diagnostics = new Container<Diagnostic>(diagnostics)
        });

        return Unit.Value;
    }
}
