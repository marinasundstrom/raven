using Microsoft.Extensions.Logging;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using TextDocumentSelector = OmniSharp.Extensions.LanguageServer.Protocol.Models.TextDocumentSelector;

namespace Raven.LanguageServer;

internal sealed class DefinitionHandler : IDefinitionHandler
{
    private readonly DocumentStore _documents;
    private readonly ILogger<DefinitionHandler> _logger;

    public DefinitionHandler(DocumentStore documents, ILogger<DefinitionHandler> logger)
    {
        _documents = documents;
        _logger = logger;
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
        try
        {
            using var _ = await _documents.EnterCompilerAccessAsync(cancellationToken).ConfigureAwait(false);
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

            if (TryResolveMacroDefinition(document.Project, root, offset, sourceText, out var macroLinks))
                return new LocationOrLocationLinks(macroLinks);

            var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, offset);
            if (resolution is null)
                return new LocationOrLocationLinks();

            var links = DefinitionLocationMapper.BuildLocationLinks(
                    resolution.Value.Symbol,
                    sourceText,
                    resolution.Value.Node.Span)
                .Select(location => (LocationOrLocationLink)location)
                .ToArray();

            return new LocationOrLocationLinks(links);
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            return new LocationOrLocationLinks();
        }
        catch (Exception ex)
        {
            _logger.LogError(
                ex,
                "Definition request failed for {Uri} at {Line}:{Character}.",
                request.TextDocument.Uri,
                request.Position.Line,
                request.Position.Character);
            return new LocationOrLocationLinks();
        }
    }

    private static bool TryResolveMacroDefinition(
        Project project,
        SyntaxNode root,
        int offset,
        SourceText sourceText,
        out LocationOrLocationLink[] links)
    {
        links = [];

        SyntaxToken token;
        try
        {
            token = root.FindToken(offset);
        }
        catch
        {
            return false;
        }

        var attribute = token.Parent?.AncestorsAndSelf().OfType<AttributeSyntax>()
            .FirstOrDefault(static candidate => candidate.IsMacroAttribute());
        if (attribute is null || !attribute.Name.Span.Contains(token.Span))
            return false;

        if (!attribute.TryGetMacroName(out var macroName))
            return false;

        var workspace = project.Solution.Workspace;
        if (workspace is null)
            return false;

        var originSpan = attribute.Name.Span;

        foreach (var macroReference in project.MacroReferences)
        {
            IEnumerable<Project> candidateProjects = project.Solution.Projects;
            if (!string.IsNullOrWhiteSpace(macroReference.SourceProjectFilePath))
            {
                candidateProjects = candidateProjects.Where(candidate =>
                    string.Equals(candidate.FilePath, macroReference.SourceProjectFilePath, StringComparison.OrdinalIgnoreCase));
            }

            foreach (var loadedMacro in TryGetAttachedMacros(macroReference, macroName))
            {
                var metadataNames = new[] { loadedMacro.GetType().FullName, loadedMacro.GetType().Name }
                    .Where(static name => !string.IsNullOrWhiteSpace(name))
                    .Distinct(StringComparer.Ordinal);

                foreach (var candidateProject in candidateProjects)
                {
                    var candidateCompilation = workspace.GetCompilation(candidateProject.Id);
                    foreach (var metadataName in metadataNames)
                    {
                        var typeSymbol = candidateCompilation.GetTypeByMetadataName(metadataName!);
                        if (typeSymbol is null || typeSymbol.DeclaringSyntaxReferences.IsDefaultOrEmpty)
                            continue;

                        links = DefinitionLocationMapper.BuildLocationLinks(typeSymbol, sourceText, originSpan)
                            .Select(static link => (LocationOrLocationLink)link)
                            .ToArray();
                        if (links.Length > 0)
                            return true;
                    }
                }
            }
        }

        return false;
    }

    private static IEnumerable<IAttachedDeclarationMacro> TryGetAttachedMacros(MacroReference macroReference, string macroName)
    {
        try
        {
            return macroReference.GetPlugins()
                .SelectMany(static plugin => plugin.GetMacros())
                .OfType<IAttachedDeclarationMacro>()
                .Where(attached => string.Equals(attached.Name, macroName, StringComparison.Ordinal))
                .ToArray();
        }
        catch
        {
            return [];
        }
    }
}

internal static class DefinitionLocationMapper
{
    public static IEnumerable<LocationLink> BuildLocationLinks(ISymbol symbol, SourceText sourceText, TextSpan originSpan)
    {
        var originSelectionRange = PositionHelper.ToRange(sourceText, originSpan);
        var seen = new HashSet<string>(StringComparer.Ordinal);

        foreach (var syntaxReference in symbol.DeclaringSyntaxReferences)
        {
            var selectionSpan = syntaxReference.Span;
            var targetSpan = syntaxReference.GetSyntax().Span;
            if (TryCreateLocationLink(syntaxReference.SyntaxTree, targetSpan, selectionSpan, originSelectionRange, out var locationLink) &&
                seen.Add($"{locationLink.TargetUri}|{locationLink.TargetSelectionRange.Start.Line}:{locationLink.TargetSelectionRange.Start.Character}:{locationLink.TargetSelectionRange.End.Line}:{locationLink.TargetSelectionRange.End.Character}"))
            {
                yield return locationLink;
            }
        }

        foreach (var location in symbol.Locations)
        {
            if (!location.IsInSource || location.SourceTree is null)
                continue;

            if (TryCreateLocationLink(location.SourceTree, location.SourceSpan, location.SourceSpan, originSelectionRange, out var mappedLocationLink) &&
                seen.Add($"{mappedLocationLink.TargetUri}|{mappedLocationLink.TargetSelectionRange.Start.Line}:{mappedLocationLink.TargetSelectionRange.Start.Character}:{mappedLocationLink.TargetSelectionRange.End.Line}:{mappedLocationLink.TargetSelectionRange.End.Character}"))
            {
                yield return mappedLocationLink;
            }
        }
    }

    private static bool TryCreateLocationLink(
        SyntaxTree syntaxTree,
        TextSpan targetSpan,
        TextSpan selectionSpan,
        OmniSharp.Extensions.LanguageServer.Protocol.Models.Range originSelectionRange,
        out LocationLink locationLink)
    {
        var path = syntaxTree.FilePath;

        if (string.IsNullOrWhiteSpace(path) || path == "file")
        {
            locationLink = null!;
            return false;
        }

        var uri = DocumentUri.FromFileSystemPath(path);

        var text = syntaxTree.GetText();
        if (text is null)
        {
            locationLink = null!;
            return false;
        }

        locationLink = new LocationLink
        {
            OriginSelectionRange = originSelectionRange,
            TargetUri = uri,
            TargetRange = PositionHelper.ToRange(text, targetSpan),
            TargetSelectionRange = PositionHelper.ToRange(text, selectionSpan)
        };

        return true;
    }
}
