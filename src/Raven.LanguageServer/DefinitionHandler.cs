using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using LspLocation = OmniSharp.Extensions.LanguageServer.Protocol.Models.Location;
using TextDocumentSelector = OmniSharp.Extensions.LanguageServer.Protocol.Models.TextDocumentSelector;

namespace Raven.LanguageServer;

internal sealed class DefinitionHandler : IDefinitionHandler
{
    private readonly DocumentStore _documents;

    public DefinitionHandler(DocumentStore documents)
    {
        _documents = documents;
    }

    public DefinitionRegistrationOptions GetRegistrationOptions(DefinitionCapability capability, ClientCapabilities clientCapabilities)
        => new()
        {
            DocumentSelector = TextDocumentSelector.ForLanguage("raven")
        };

    public void SetCapability(DefinitionCapability capability)
    {
    }

    public async Task<LocationOrLocationLinks?> Handle(DefinitionParams request, CancellationToken cancellationToken)
    {
        if (!_documents.TryGetDocument(request.TextDocument.Uri, out var document))
            return new LocationOrLocationLinks();

        var syntaxTree = await document.GetSyntaxTreeAsync(cancellationToken).ConfigureAwait(false);
        if (syntaxTree is null)
            return new LocationOrLocationLinks();

        if (!_documents.TryGetCompilation(request.TextDocument.Uri, out var compilation) || compilation is null)
            return new LocationOrLocationLinks();

        var sourceText = await document.GetTextAsync(cancellationToken).ConfigureAwait(false);
        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot(cancellationToken);
        var offset = PositionHelper.ToOffset(sourceText, request.Position);

        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, offset);
        if (resolution is null)
            return new LocationOrLocationLinks();

        var locations = BuildLocations(resolution.Value.Symbol)
            .Select(location => (LocationOrLocationLink)location)
            .ToArray();

        return new LocationOrLocationLinks(locations);
    }

    private static IEnumerable<LspLocation> BuildLocations(ISymbol symbol)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);

        foreach (var syntaxReference in symbol.DeclaringSyntaxReferences)
        {
            if (TryCreateLocation(syntaxReference.SyntaxTree, syntaxReference.Span, out var location) &&
                seen.Add($"{location.Uri}|{location.Range.Start.Line}:{location.Range.Start.Character}:{location.Range.End.Line}:{location.Range.End.Character}"))
            {
                yield return location;
            }
        }

        foreach (var location in symbol.Locations)
        {
            if (!location.IsInSource || location.SourceTree is null)
                continue;

            if (TryCreateLocation(location.SourceTree, location.SourceSpan, out var mappedLocation) &&
                seen.Add($"{mappedLocation.Uri}|{mappedLocation.Range.Start.Line}:{mappedLocation.Range.Start.Character}:{mappedLocation.Range.End.Line}:{mappedLocation.Range.End.Character}"))
            {
                yield return mappedLocation;
            }
        }
    }

    private static bool TryCreateLocation(SyntaxTree syntaxTree, TextSpan span, out LspLocation location)
    {
        var path = syntaxTree.FilePath;

        if (string.IsNullOrWhiteSpace(path) || path == "file")
        {
            location = null!;
            return false;
        }

        var uri = DocumentUri.FromFileSystemPath(path);

        var text = syntaxTree.GetText();
        if (text is null)
        {
            location = null!;
            return false;
        }

        location = new LspLocation
        {
            Uri = uri,
            Range = PositionHelper.ToRange(text, span)
        };
        return true;
    }
}
