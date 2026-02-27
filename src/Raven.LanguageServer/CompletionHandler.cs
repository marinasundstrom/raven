using System.Linq;

using Microsoft.Extensions.Logging;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using LspCompletionItem = OmniSharp.Extensions.LanguageServer.Protocol.Models.CompletionItem;
using RavenCompletionItem = Raven.CodeAnalysis.CompletionItem;
using TextDocumentSelector = OmniSharp.Extensions.LanguageServer.Protocol.Models.TextDocumentSelector;

namespace Raven.LanguageServer;

internal sealed class CompletionHandler : ICompletionHandler
{
    private readonly DocumentStore _documents;
    private readonly CompletionService _completionService = new();
    private readonly ILogger<CompletionHandler> _logger;

    public CompletionHandler(DocumentStore documents, ILogger<CompletionHandler> logger)
    {
        _documents = documents;
        _logger = logger;
    }

    public CompletionRegistrationOptions GetRegistrationOptions(CompletionCapability capability, ClientCapabilities clientCapabilities)
        => new()
        {
            DocumentSelector = TextDocumentSelector.ForLanguage("raven"),
            TriggerCharacters = new Container<string>(".", ":", "(")
        };

    public async Task<CompletionList> Handle(CompletionParams request, CancellationToken cancellationToken)
    {
        try
        {
            if (!_documents.TryGetDocument(request.TextDocument.Uri, out var document))
                return new CompletionList();

            var syntaxTree = await document.GetSyntaxTreeAsync(cancellationToken).ConfigureAwait(false);
            if (syntaxTree is null)
                return new CompletionList();

            var text = await document.GetTextAsync(cancellationToken).ConfigureAwait(false);
            var position = PositionHelper.ToOffset(text, request.Position);
            if (!_documents.TryGetCompilation(request.TextDocument.Uri, out var compilation) || compilation is null)
                return new CompletionList();

            var items = (await _completionService.GetCompletionsAsync(compilation, syntaxTree, position, cancellationToken).ConfigureAwait(false))
                .Select(item => ToLspCompletion(item, text))
                .ToList();

            return new CompletionList(items, isIncomplete: false);
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            return new CompletionList();
        }
        catch (Exception ex)
        {
            _logger.LogError(
                ex,
                "Completion request failed for {Uri} at {Line}:{Character}.",
                request.TextDocument.Uri,
                request.Position.Line,
                request.Position.Character);
            return new CompletionList();
        }
    }

    public void SetCapability(CompletionCapability capability)
    {
    }

    private static LspCompletionItem ToLspCompletion(RavenCompletionItem item, SourceText text)
    {
        var range = PositionHelper.ToRange(text, item.ReplacementSpan);
        return new LspCompletionItem
        {
            Label = item.DisplayText,
            Detail = item.Description,
            Kind = MapCompletionItemKind(item),
            SortText = GetSortText(item),
            InsertText = item.InsertionText,
            TextEdit = new TextEditOrInsertReplaceEdit(new TextEdit
            {
                NewText = item.InsertionText,
                Range = range
            }),
            InsertTextFormat = InsertTextFormat.PlainText
        };
    }

    private static CompletionItemKind MapCompletionItemKind(RavenCompletionItem item)
    {
        if (item.Symbol is { } symbol)
        {
            return symbol switch
            {
                IMethodSymbol method when method.MethodKind == MethodKind.Constructor => CompletionItemKind.Constructor,
                IMethodSymbol => CompletionItemKind.Method,
                IPropertySymbol => CompletionItemKind.Property,
                IFieldSymbol => CompletionItemKind.Field,
                IParameterSymbol => CompletionItemKind.Variable,
                ILocalSymbol => CompletionItemKind.Variable,
                IEventSymbol => CompletionItemKind.Event,
                INamespaceSymbol => CompletionItemKind.Module,
                ITypeParameterSymbol => CompletionItemKind.TypeParameter,
                INamedTypeSymbol namedType => namedType.TypeKind switch
                {
                    TypeKind.Class => CompletionItemKind.Class,
                    TypeKind.Interface => CompletionItemKind.Interface,
                    TypeKind.Struct => CompletionItemKind.Struct,
                    TypeKind.Enum => CompletionItemKind.Enum,
                    TypeKind.Delegate => CompletionItemKind.Function,
                    _ => CompletionItemKind.Class
                },
                _ => CompletionItemKind.Text
            };
        }

        if (SyntaxFacts.TryParseKeyword(item.DisplayText, out _))
            return CompletionItemKind.Keyword;

        return CompletionItemKind.Text;
    }

    private static string GetSortText(RavenCompletionItem item)
    {
        var rank = item.Symbol switch
        {
            IFieldSymbol => 10,
            IPropertySymbol => 10,
            IEventSymbol => 10,
            ILocalSymbol => 15,
            IParameterSymbol => 15,
            IMethodSymbol { IsExtensionMethod: false } => 20,
            IMethodSymbol { IsExtensionMethod: true } => 40,
            ITypeSymbol => 50,
            INamespaceSymbol => 60,
            null when SyntaxFacts.TryParseKeyword(item.DisplayText, out _) => 70,
            _ => 80
        };

        return $"{rank:D2}_{item.DisplayText}";
    }
}
