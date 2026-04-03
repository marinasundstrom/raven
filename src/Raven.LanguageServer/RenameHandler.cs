using Microsoft.Extensions.Logging;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using TextDocumentSelector = OmniSharp.Extensions.LanguageServer.Protocol.Models.TextDocumentSelector;

namespace Raven.LanguageServer;

internal sealed class RenameHandler : IRenameHandler, IPrepareRenameHandler
{
    private readonly DocumentStore _documents;
    private readonly WorkspaceManager _workspaceManager;
    private readonly ILogger<RenameHandler> _logger;

    public RenameHandler(DocumentStore documents, WorkspaceManager workspaceManager, ILogger<RenameHandler> logger)
    {
        _documents = documents;
        _workspaceManager = workspaceManager;
        _logger = logger;
    }

    public RenameRegistrationOptions GetRegistrationOptions(RenameCapability capability, ClientCapabilities clientCapabilities)
        => new()
        {
            DocumentSelector = TextDocumentSelector.ForLanguage("raven"),
            PrepareProvider = true
        };

    public void SetCapability(RenameCapability capability)
    {
    }

    public async Task<RangeOrPlaceholderRange?> Handle(PrepareRenameParams request, CancellationToken cancellationToken)
    {
        try
        {
            using var _ = await _documents.EnterCompilerAccessAsync(cancellationToken, "prepareRename", request.TextDocument.Uri).ConfigureAwait(false);
            var context = await _documents.GetAnalysisContextAsync(request.TextDocument.Uri, cancellationToken).ConfigureAwait(false);
            if (context is null)
                return null;

            var compilation = context.Value.Compilation;
            var syntaxTree = context.Value.SyntaxTree;
            var sourceText = context.Value.SourceText;
            var semanticModel = compilation.GetSemanticModel(syntaxTree);
            var root = syntaxTree.GetRoot(cancellationToken);
            var offset = Math.Clamp(PositionHelper.ToOffset(sourceText, request.Position), 0, root.FullSpan.End);
            var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, offset);
            if (resolution is null)
                return null;

            var span = ReferenceSearchService.GetReferenceSpan(resolution.Value.Node);
            if (span.Length == 0)
                return null;

            var placeholder = sourceText.GetSubText(span);
            return new PlaceholderRange
            {
                Range = PositionHelper.ToRange(sourceText, span),
                Placeholder = placeholder
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
                "Prepare rename failed for {Uri} at {Line}:{Character}.",
                request.TextDocument.Uri,
                request.Position.Line,
                request.Position.Character);
            return null;
        }
    }

    public async Task<WorkspaceEdit?> Handle(RenameParams request, CancellationToken cancellationToken)
    {
        try
        {
            using var _ = await _documents.EnterCompilerAccessAsync(cancellationToken, "rename", request.TextDocument.Uri).ConfigureAwait(false);
            if (!RenameService.IsValidIdentifier(request.NewName))
                return null;

            var context = await _documents.GetAnalysisContextAsync(request.TextDocument.Uri, cancellationToken).ConfigureAwait(false);
            if (context is null)
                return null;

            var compilation = context.Value.Compilation;
            var syntaxTree = context.Value.SyntaxTree;
            var sourceText = context.Value.SourceText;
            var semanticModel = compilation.GetSemanticModel(syntaxTree);
            var root = syntaxTree.GetRoot(cancellationToken);
            var offset = Math.Clamp(PositionHelper.ToOffset(sourceText, request.Position), 0, root.FullSpan.End);
            var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, offset);
            if (resolution is null)
                return null;

            var targetSymbol = ReferenceSearchService.NormalizeSymbol(resolution.Value.Symbol);
            return await RenameService.BuildWorkspaceEditAsync(
                _workspaceManager.GetProjectsSnapshot(),
                targetSymbol,
                request.NewName,
                cancellationToken).ConfigureAwait(false);
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            return null;
        }
        catch (Exception ex)
        {
            _logger.LogError(
                ex,
                "Rename failed for {Uri} at {Line}:{Character} to '{NewName}'.",
                request.TextDocument.Uri,
                request.Position.Line,
                request.Position.Character,
                request.NewName);
            return null;
        }
    }
}

internal static class RenameService
{
    public static async Task<WorkspaceEdit> BuildWorkspaceEditAsync(
        IReadOnlyList<Project> projects,
        ISymbol targetSymbol,
        string newName,
        CancellationToken cancellationToken)
    {
        var changes = new Dictionary<DocumentUri, IEnumerable<TextEdit>>();
        var references = (await ReferenceSearchService.FindReferencesAsync(
                projects,
                targetSymbol,
                includeDeclarations: true,
                cancellationToken)
            .ConfigureAwait(false))
            .GroupBy(reference => reference.FilePath, StringComparer.Ordinal)
            .ToArray();

        foreach (var group in references)
        {
            var uri = DocumentUri.FromFileSystemPath(group.Key);
            var edits = group
                .OrderByDescending(reference => reference.Span.Start)
                .Select(reference => new TextEdit
                {
                    Range = PositionHelper.ToRange(reference.SourceText, reference.Span),
                    NewText = newName
                })
                .ToArray();

            changes[uri] = edits;
        }

        return new WorkspaceEdit
        {
            Changes = changes
        };
    }

    public static bool IsValidIdentifier(string? newName)
    {
        if (string.IsNullOrWhiteSpace(newName))
            return false;

        var trimmed = newName.Trim();
        var unescaped = trimmed.StartsWith('@') ? trimmed[1..] : trimmed;
        if (string.IsNullOrWhiteSpace(unescaped))
            return false;

        if (!SyntaxFacts.IsIdentifierStartCharacter(unescaped[0]))
            return false;

        for (var i = 1; i < unescaped.Length; i++)
        {
            if (!SyntaxFacts.IsIdentifierPartCharacter(unescaped[i]))
                return false;
        }

        if (!trimmed.StartsWith('@') && SyntaxFacts.TryParseKeyword(unescaped, out _))
            return false;

        return true;
    }
}
