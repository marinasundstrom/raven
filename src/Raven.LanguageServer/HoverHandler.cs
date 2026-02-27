using Microsoft.Extensions.Logging;

using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Documentation;

using TextDocumentSelector = OmniSharp.Extensions.LanguageServer.Protocol.Models.TextDocumentSelector;

namespace Raven.LanguageServer;

internal sealed class HoverHandler : IHoverHandler
{
    private readonly DocumentStore _documents;
    private readonly ILogger<HoverHandler> _logger;

    public HoverHandler(DocumentStore documents, ILogger<HoverHandler> logger)
    {
        _documents = documents;
        _logger = logger;
    }

    public HoverRegistrationOptions GetRegistrationOptions(HoverCapability capability, ClientCapabilities clientCapabilities)
        => new()
        {
            DocumentSelector = TextDocumentSelector.ForLanguage("raven")
        };

    public void SetCapability(HoverCapability capability)
    {
    }

    public async Task<Hover?> Handle(HoverParams request, CancellationToken cancellationToken)
    {
        try
        {
            if (!_documents.TryGetDocument(request.TextDocument.Uri, out var document))
                return null;

            var syntaxTree = await document.GetSyntaxTreeAsync(cancellationToken).ConfigureAwait(false);
            if (syntaxTree is null)
                return null;

            if (!_documents.TryGetCompilation(request.TextDocument.Uri, out var compilation) || compilation is null)
                return null;

            var sourceText = await document.GetTextAsync(cancellationToken).ConfigureAwait(false);
            var semanticModel = compilation.GetSemanticModel(syntaxTree);
            var root = syntaxTree.GetRoot(cancellationToken);
            var offset = PositionHelper.ToOffset(sourceText, request.Position);

            var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, offset);
            if (resolution is null)
                return null;

            var symbol = resolution.Value.Symbol;
            var signature = symbol.ToDisplayString(SymbolDisplayFormat.RavenTooltipFormat);
            var containing = symbol.ContainingSymbol?.ToDisplayString(
                SymbolDisplayFormat.RavenSignatureFormat.WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly));
            var documentation = symbol.GetDocumentationComment();
            var hoverText = BuildHoverText(signature, symbol.Kind.ToString(), containing, documentation);

            return new Hover
            {
                Contents = new MarkedStringsOrMarkupContent(new MarkupContent
                {
                    Kind = MarkupKind.Markdown,
                    Value = hoverText
                }),
                Range = PositionHelper.ToRange(sourceText, resolution.Value.Node.Span)
            };
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            return null;
        }
        catch (Exception ex)
        {
            _logger.LogError(
                ex,
                "Hover request failed for {Uri} at {Line}:{Character}.",
                request.TextDocument.Uri,
                request.Position.Line,
                request.Position.Character);
            return null;
        }
    }

    private static string BuildHoverText(
        string signature,
        string kind,
        string? containing,
        DocumentationComment? documentation)
    {
        var docsText = FormatDocumentation(documentation);
        var contextText = !string.IsNullOrWhiteSpace(containing)
            ? $"{kind} in `{containing}`"
            : kind;

        if (!string.IsNullOrWhiteSpace(containing))
            return string.IsNullOrWhiteSpace(docsText)
                ? $"```raven\n{signature}\n```\n\n{contextText}"
                : $"```raven\n{signature}\n```\n\n{contextText}\n\n---\n\n{docsText}";

        return string.IsNullOrWhiteSpace(docsText)
            ? $"```raven\n{signature}\n```\n\n{contextText}"
            : $"```raven\n{signature}\n```\n\n{contextText}\n\n---\n\n{docsText}";
    }

    private static string? FormatDocumentation(DocumentationComment? documentation)
    {
        if (documentation is null)
            return null;

        if (string.IsNullOrWhiteSpace(documentation.Content))
            return null;

        return documentation.Format switch
        {
            DocumentationFormat.Markdown => documentation.Content.Trim(),
            DocumentationFormat.Xml => $"```xml\n{documentation.Content.Trim()}\n```",
            _ => documentation.Content.Trim()
        };
    }
}
