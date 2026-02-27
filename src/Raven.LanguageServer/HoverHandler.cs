using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;

using TextDocumentSelector = OmniSharp.Extensions.LanguageServer.Protocol.Models.TextDocumentSelector;

namespace Raven.LanguageServer;

internal sealed class HoverHandler : IHoverHandler
{
    private readonly DocumentStore _documents;

    public HoverHandler(DocumentStore documents)
    {
        _documents = documents;
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
        var containing = symbol.ContainingSymbol?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
        var hoverText = BuildHoverText(signature, symbol.Kind.ToString(), containing);

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

    private static string BuildHoverText(string signature, string kind, string? containing)
    {
        if (!string.IsNullOrWhiteSpace(containing))
            return $"```raven\n{signature}\n```\n\n{kind} in `{containing}`";

        return $"```raven\n{signature}\n```\n\n{kind}";
    }
}
