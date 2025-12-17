using System.Linq;
using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Text;
using RavenCompletionItem = Raven.CodeAnalysis.CompletionItem;

namespace Raven.LanguageServer;

internal sealed class CompletionHandler : ICompletionHandler
{
    private readonly DocumentStore _documents;
    private readonly CompletionService _completionService = new();

    public CompletionHandler(DocumentStore documents)
    {
        _documents = documents;
    }

    public CompletionRegistrationOptions GetRegistrationOptions(CompletionCapability capability, ClientCapabilities clientCapabilities)
        => new()
        {
            DocumentSelector = DocumentSelector.ForLanguage("raven"),
            TriggerCharacters = new Container<string>(".", ":", "(")
        };

    public async Task<CompletionList> Handle(CompletionParams request, CancellationToken cancellationToken)
    {
        if (!_documents.TryGetDocument(request.TextDocument.Uri, out var document))
            return new CompletionList();

        var syntaxTree = await document.GetSyntaxTreeAsync(cancellationToken).ConfigureAwait(false);
        if (syntaxTree is null)
            return new CompletionList();

        var text = await document.GetTextAsync(cancellationToken).ConfigureAwait(false);
        var position = PositionHelper.ToOffset(text, request.Position);
        var compilation = _documents.GetCompilation();

        var items = _completionService.GetCompletions(compilation, syntaxTree, position)
            .Select(item => ToLspCompletion(item, text))
            .ToList();

        return new CompletionList(items, isIncomplete: false);
    }

    public void SetCapability(CompletionCapability capability)
    {
    }

    private static CompletionItem ToLspCompletion(RavenCompletionItem item, SourceText text)
    {
        var range = PositionHelper.ToRange(text, item.ReplacementSpan);
        return new CompletionItem
        {
            Label = item.DisplayText,
            Detail = item.Description,
            InsertText = item.InsertionText,
            TextEdit = new TextEditOrInsertReplaceEdit(new TextEdit
            {
                NewText = item.InsertionText,
                Range = range
            }),
            InsertTextFormat = InsertTextFormat.PlainText
        };
    }
}
