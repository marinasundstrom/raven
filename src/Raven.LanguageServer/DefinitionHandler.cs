using System.Diagnostics;

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
        var totalStopwatch = Stopwatch.StartNew();
        var gateWaitStopwatch = Stopwatch.StartNew();
        double gateWaitMs = 0;
        double analysisContextMs = 0;
        double semanticModelMs = 0;
        double resolutionMs = 0;
        int resultCount = 0;

        try
        {
            using var _ = await _documents.EnterDocumentSemanticAccessAsync(request.TextDocument.Uri, cancellationToken, "definition").ConfigureAwait(false);
            gateWaitMs = gateWaitStopwatch.Elapsed.TotalMilliseconds;
            var stageStopwatch = Stopwatch.StartNew();
            var context = await _documents.GetAnalysisContextAsync(request.TextDocument.Uri, cancellationToken).ConfigureAwait(false);
            analysisContextMs = stageStopwatch.Elapsed.TotalMilliseconds;
            if (context is null)
                return new LocationOrLocationLinks();
            var document = context.Value.Document;
            var syntaxTree = context.Value.SyntaxTree;
            var sourceText = context.Value.SourceText;
            stageStopwatch.Restart();
            var semanticModel = context.Value.Compilation.GetSemanticModel(syntaxTree);
            semanticModelMs = stageStopwatch.Elapsed.TotalMilliseconds;
            var root = syntaxTree.GetRoot(cancellationToken);
            var offset = Math.Clamp(PositionHelper.ToOffset(sourceText, request.Position), 0, root.FullSpan.End);

            if (TryResolveMacroDefinition(document.Project, root, offset, sourceText, out var macroLinks))
            {
                resultCount = macroLinks.Length;
                return new LocationOrLocationLinks(macroLinks);
            }

            stageStopwatch.Restart();
            var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, offset);
            resolutionMs = stageStopwatch.Elapsed.TotalMilliseconds;
            if (resolution is null)
                return new LocationOrLocationLinks();

            var links = DefinitionLocationMapper.BuildLocationLinks(
                    SymbolResolutionHelpers.GetNavigationTargetSymbol(resolution.Value),
                    sourceText,
                    resolution.Value.Node.Span)
                .Select(location => (LocationOrLocationLink)location)
                .ToArray();
            resultCount = links.Length;

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
        finally
        {
            totalStopwatch.Stop();
            LanguageServerPerformanceInstrumentation.RecordOperation(
                "definition",
                request.TextDocument.Uri,
                null,
                totalStopwatch.Elapsed.TotalMilliseconds,
                resultCount: resultCount,
                stages:
                [
                    new LanguageServerPerformanceInstrumentation.StageTiming("gateWait", gateWaitMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("analysisContext", analysisContextMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("semanticModel", semanticModelMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("resolution", resolutionMs)
                ]);
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

        string? macroName = null;
        TextSpan originSpan = default;

        var attribute = token.Parent?.AncestorsAndSelf().OfType<AttributeSyntax>()
            .FirstOrDefault(static candidate => candidate.IsMacroAttribute());
        if (attribute is not null && attribute.Name.Span.Contains(token.Span) && attribute.TryGetMacroName(out var attributeMacroName))
        {
            macroName = attributeMacroName;
            originSpan = attribute.Name.Span;
        }
        else
        {
            var freestandingMacro = token.Parent?.AncestorsAndSelf().OfType<FreestandingMacroExpressionSyntax>().FirstOrDefault();
            if (freestandingMacro is null || !freestandingMacro.Name.Span.Contains(token.Span) || !freestandingMacro.TryGetMacroName(out var freestandingMacroName))
                return false;

            macroName = freestandingMacroName;
            originSpan = freestandingMacro.Name.Span;
        }

        var workspace = project.Solution.Workspace;
        if (workspace is null)
            return false;

        foreach (var macroReference in project.MacroReferences)
        {
            IEnumerable<Project> candidateProjects = project.Solution.Projects;
            if (!string.IsNullOrWhiteSpace(macroReference.SourceProjectFilePath))
            {
                candidateProjects = candidateProjects.Where(candidate =>
                    string.Equals(candidate.FilePath, macroReference.SourceProjectFilePath, StringComparison.OrdinalIgnoreCase));
            }

            foreach (var loadedMacro in TryGetMacros(macroReference, macroName))
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

    private static IEnumerable<IMacroDefinition> TryGetMacros(MacroReference macroReference, string macroName)
    {
        try
        {
            return macroReference.GetPlugins()
                .SelectMany(static plugin => plugin.GetMacros())
                .Where(macro => string.Equals(macro.Name, macroName, StringComparison.Ordinal))
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
