using System.Diagnostics;
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
            TriggerCharacters = new Container<string>(".", ":", "#", "[")
        };

    public async Task<CompletionList> Handle(CompletionParams request, CancellationToken cancellationToken)
    {
        var totalStopwatch = Stopwatch.StartNew();
        var gateWaitStopwatch = Stopwatch.StartNew();
        double gateWaitMs = 0;
        double analysisContextMs = 0;
        double semanticModelMs = 0;
        double providerMs = 0;
        double completionMaterializationMs = 0;
        int itemCount = 0;
        var usedFallback = false;
        string? failureType = null;

        try
        {
            using var _ = await _documents.EnterDocumentSemanticAccessAsync(request.TextDocument.Uri, cancellationToken, "completion").ConfigureAwait(false);
            gateWaitMs = gateWaitStopwatch.Elapsed.TotalMilliseconds;
            _logger.LogDebug(
                "Completion request for {Uri} at {Line}:{Character}. Trigger={TriggerKind}/{TriggerCharacter}",
                request.TextDocument.Uri,
                request.Position.Line,
                request.Position.Character,
                request.Context?.TriggerKind,
                request.Context?.TriggerCharacter);

            var stageStopwatch = Stopwatch.StartNew();
            var context = await _documents.GetAnalysisContextAsync(request.TextDocument.Uri, cancellationToken).ConfigureAwait(false);
            analysisContextMs = stageStopwatch.Elapsed.TotalMilliseconds;
            if (context is null)
                return new CompletionList();
            var syntaxTree = context.Value.SyntaxTree;
            var text = context.Value.SourceText;
            var position = Math.Clamp(PositionHelper.ToOffset(text, request.Position), 0, syntaxTree.GetRoot(cancellationToken).FullSpan.End);

            stageStopwatch.Restart();
            var semanticModel = await _documents.GetSemanticModelAsync(request.TextDocument.Uri, cancellationToken).ConfigureAwait(false);
            semanticModelMs = stageStopwatch.Elapsed.TotalMilliseconds;
            if (semanticModel is null)
                return new CompletionList();

            stageStopwatch.Restart();
            var completion = await _completionService.GetCompletionsWithMetricsAsync(semanticModel, position, cancellationToken).ConfigureAwait(false);
            semanticModelMs += completion.SemanticModelMs;
            providerMs = completion.ProviderMs;
            usedFallback = completion.UsedFallback;
            failureType = completion.FailureType;

            stageStopwatch.Restart();
            var items = completion.Items
                .Select(item => CompletionItemMapper.ToLspCompletion(item, text))
                .ToList();
            completionMaterializationMs = stageStopwatch.Elapsed.TotalMilliseconds;
            itemCount = items.Count;

            _logger.LogDebug(
                "Completion produced {Count} items for {Uri}. fallback={Fallback} failureType={FailureType}",
                items.Count,
                request.TextDocument.Uri,
                usedFallback,
                failureType ?? "<none>");

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
        finally
        {
            totalStopwatch.Stop();
            LanguageServerPerformanceInstrumentation.RecordOperation(
                "completion",
                request.TextDocument.Uri,
                null,
                totalStopwatch.Elapsed.TotalMilliseconds,
                resultCount: itemCount,
                stages:
                [
                    new LanguageServerPerformanceInstrumentation.StageTiming("gateWait", gateWaitMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("analysisContext", analysisContextMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("semanticModel", semanticModelMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("provider", providerMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("materializeItems", completionMaterializationMs)
                ]);
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
        var containerDescription = TryGetContainerDescription(symbol);

        return symbol switch
        {
            IMethodSymbol { IsExtensionMethod: true } => new CompletionItemLabelDetails
            {
                Detail = " (extension)",
                Description = containerDescription
            },
            IPropertySymbol property when property.IsExtensionProperty => new CompletionItemLabelDetails
            {
                Detail = " (extension)",
                Description = containerDescription
            },
            _ when containerDescription is not null => new CompletionItemLabelDetails
            {
                Description = containerDescription
            },
            _ => null
        };
    }

    private static string? TryGetContainerDescription(ISymbol? symbol)
    {
        if (symbol is null)
            return null;

        var containing = symbol.ContainingSymbol;
        if (containing is null || containing.Kind == Raven.CodeAnalysis.SymbolKind.Assembly)
            return null;

        if (containing is INamespaceSymbol namespaceSymbol)
            return GetNamespaceDisplayString(namespaceSymbol);

        return containing.ToDisplayString(SymbolDisplayFormat.RavenSignatureFormat
            .WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces)
            .WithKindOptions(SymbolDisplayKindOptions.None));
    }

    private static string? GetNamespaceDisplayString(INamespaceSymbol namespaceSymbol)
    {
        if (namespaceSymbol.IsGlobalNamespace)
            return null;

        var parts = new List<string>();
        for (var current = namespaceSymbol; current is not null && !current.IsGlobalNamespace; current = current.ContainingNamespace)
        {
            if (!string.IsNullOrEmpty(current.Name))
                parts.Add(current.Name);
        }

        if (parts.Count == 0)
            return null;

        parts.Reverse();
        return string.Join(".", parts);
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
