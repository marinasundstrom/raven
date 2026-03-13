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
            using var _ = await _documents.EnterCompilerAccessAsync(cancellationToken).ConfigureAwait(false);
            _logger.LogDebug(
                "Completion request for {Uri} at {Line}:{Character}. Trigger={TriggerKind}/{TriggerCharacter}",
                request.TextDocument.Uri,
                request.Position.Line,
                request.Position.Character,
                request.Context?.TriggerKind,
                request.Context?.TriggerCharacter);

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
                .Select(item => CompletionItemMapper.ToLspCompletion(item, text))
                .ToList();

            _logger.LogDebug(
                "Completion produced {Count} items for {Uri}.",
                items.Count,
                request.TextDocument.Uri);

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

    internal static CompletionItemKind MapCompletionItemKind(RavenCompletionItem item)
    {
        if (item.Symbol is { } symbol)
        {
            return symbol switch
            {
                IMethodSymbol method when method.MethodKind == MethodKind.Constructor => CompletionItemKind.Constructor,
                IMethodSymbol method when method.IsExtensionMethod => CompletionItemKind.Function,
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

    internal static string GetSortText(RavenCompletionItem item)
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

internal static class CompletionItemMapper
{
    private static readonly Container<string> s_defaultCommitCharacters = new(".", "(", ")", ",", ";");

    public static LspCompletionItem ToLspCompletion(RavenCompletionItem item, SourceText text)
    {
        var range = PositionHelper.ToRange(text, item.ReplacementSpan);
        var useSnippet = TryBuildSnippetText(item.InsertionText, item.CursorOffset, out var newText);

        return new LspCompletionItem
        {
            Label = item.DisplayText,
            FilterText = item.DisplayText,
            Detail = item.Description,
            LabelDetails = TryCreateLabelDetails(item.Symbol),
            Kind = CompletionHandler.MapCompletionItemKind(item),
            SortText = CompletionHandler.GetSortText(item),
            CommitCharacters = s_defaultCommitCharacters,
            InsertText = newText,
            TextEdit = new TextEditOrInsertReplaceEdit(new TextEdit
            {
                NewText = newText,
                Range = range
            }),
            InsertTextFormat = useSnippet
                ? InsertTextFormat.Snippet
                : InsertTextFormat.PlainText
        };
    }

    private static CompletionItemLabelDetails? TryCreateLabelDetails(ISymbol? symbol)
    {
        return symbol switch
        {
            IMethodSymbol { IsExtensionMethod: true } => new CompletionItemLabelDetails
            {
                Detail = " (extension)"
            },
            IPropertySymbol property when property.IsExtensionProperty() => new CompletionItemLabelDetails
            {
                Detail = " (extension)"
            },
            _ => null
        };
    }

    private static bool TryBuildSnippetText(string insertionText, int? cursorOffset, out string newText)
    {
        var insertionLength = insertionText.Length;
        if (cursorOffset is null)
        {
            newText = insertionText;
            return false;
        }

        var clampedCursorOffset = Math.Clamp(cursorOffset.Value, 0, insertionLength);
        if (clampedCursorOffset == insertionLength)
        {
            newText = insertionText;
            return false;
        }

        var beforeCursor = insertionText[..clampedCursorOffset];
        var afterCursor = insertionText[clampedCursorOffset..];
        newText = EscapeSnippetText(beforeCursor) + "$0" + EscapeSnippetText(afterCursor);
        return true;
    }

    private static string EscapeSnippetText(string value)
    {
        if (string.IsNullOrEmpty(value))
            return value;

        return value
            .Replace("\\", "\\\\", StringComparison.Ordinal)
            .Replace("$", "\\$", StringComparison.Ordinal)
            .Replace("}", "\\}", StringComparison.Ordinal);
    }
}
