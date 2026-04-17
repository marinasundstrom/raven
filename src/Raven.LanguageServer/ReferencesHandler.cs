using Microsoft.Extensions.Logging;

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

internal sealed class ReferencesHandler : IReferencesHandler
{
    private readonly DocumentStore _documents;
    private readonly WorkspaceManager _workspaceManager;
    private readonly ILogger<ReferencesHandler> _logger;

    public ReferencesHandler(DocumentStore documents, WorkspaceManager workspaceManager, ILogger<ReferencesHandler> logger)
    {
        _documents = documents;
        _workspaceManager = workspaceManager;
        _logger = logger;
    }

    public ReferenceRegistrationOptions GetRegistrationOptions(ReferenceCapability capability, ClientCapabilities clientCapabilities)
        => new()
        {
            DocumentSelector = TextDocumentSelector.ForLanguage("raven")
        };

    public void SetCapability(ReferenceCapability capability)
    {
    }

    public async Task<LocationContainer?> Handle(ReferenceParams request, CancellationToken cancellationToken)
    {
        try
        {
            using var _ = await _documents.EnterDocumentSemanticAccessAsync(request.TextDocument.Uri, cancellationToken, "references").ConfigureAwait(false);
            var context = await _documents.GetAnalysisContextAsync(request.TextDocument.Uri, cancellationToken).ConfigureAwait(false);
            if (context is null)
                return new LocationContainer();

            var syntaxTree = context.Value.SyntaxTree;
            var sourceText = context.Value.SourceText;
            var semanticModel = context.Value.Compilation.GetSemanticModel(syntaxTree);
            var root = syntaxTree.GetRoot(cancellationToken);
            var offset = Math.Clamp(PositionHelper.ToOffset(sourceText, request.Position), 0, root.FullSpan.End);

            var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, offset);
            if (resolution is null)
                return new LocationContainer();

            var targetSymbol = SymbolResolutionHelpers.GetReferenceTargetSymbol(resolution.Value);
            var references = await ReferenceSearchService.FindReferencesAsync(
                    _workspaceManager.GetProjectsSnapshot(),
                    targetSymbol,
                    includeDeclarations: request.Context.IncludeDeclaration,
                    cancellationToken)
                .ConfigureAwait(false);

            var locations = references
                .Select(reference => (LspLocation)new LspLocation
                {
                    Uri = DocumentUri.FromFileSystemPath(reference.FilePath),
                    Range = PositionHelper.ToRange(reference.SourceText, reference.Span)
                })
                .ToArray();

            return new LocationContainer(locations);
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            return new LocationContainer();
        }
        catch (Exception ex)
        {
            _logger.LogError(
                ex,
                "References request failed for {Uri} at {Line}:{Character}.",
                request.TextDocument.Uri,
                request.Position.Line,
                request.Position.Character);
            return new LocationContainer();
        }
    }
}

internal static class ReferenceSearchService
{
    public static async Task<IReadOnlyList<ReferenceResult>> FindReferencesAsync(
        IReadOnlyList<Project> projects,
        ISymbol targetSymbol,
        bool includeDeclarations,
        CancellationToken cancellationToken)
    {
        var results = new List<ReferenceResult>();
        var seen = new HashSet<string>(StringComparer.Ordinal);

        if (includeDeclarations)
        {
            foreach (var syntaxReference in targetSymbol.DeclaringSyntaxReferences)
            {
                var syntaxTree = syntaxReference.SyntaxTree;
                var path = syntaxTree.FilePath;
                if (string.IsNullOrWhiteSpace(path) || path == "file")
                    continue;

                var text = syntaxTree.GetText();
                if (text is null)
                    continue;

                var declarationSyntax = syntaxReference.GetSyntax(cancellationToken);
                var declarationSpan = GetReferenceSpan(declarationSyntax);

                var key = BuildKey(path, declarationSpan);
                if (seen.Add(key))
                    results.Add(new ReferenceResult(path, text, declarationSpan));
            }
        }

        foreach (var project in projects)
        {
            cancellationToken.ThrowIfCancellationRequested();
            var compilation = project.Solution.Workspace?.GetCompilation(project.Id);
            if (compilation is null)
                continue;

            foreach (var document in project.Documents)
            {
                cancellationToken.ThrowIfCancellationRequested();
                var syntaxTree = await document.GetSyntaxTreeAsync(cancellationToken);
                if (syntaxTree is null)
                    continue;

                var path = syntaxTree.FilePath;
                if (string.IsNullOrWhiteSpace(path) || path == "file")
                    continue;

                var sourceText = syntaxTree.GetText();
                if (sourceText is null)
                    continue;

                var semanticModel = compilation.GetSemanticModel(syntaxTree);
                var root = syntaxTree.GetRoot(cancellationToken);

                foreach (var node in root.DescendantNodes().Where(IsReferenceCandidate))
                {
                    cancellationToken.ThrowIfCancellationRequested();
                    if (!SymbolResolutionHelpers.TryGetPreferredSymbolInfo(semanticModel, node, out var symbolInfo))
                        continue;
                    if (!TryMatchesSymbol(symbolInfo, targetSymbol, node, out var referenceSpan))
                        continue;

                    var key = BuildKey(path, referenceSpan);
                    if (!seen.Add(key))
                        continue;

                    results.Add(new ReferenceResult(path, sourceText, referenceSpan));
                }
            }
        }

        return results;
    }

    public static ISymbol NormalizeSymbol(ISymbol symbol)
    {
        if (symbol is IMethodSymbol method)
        {
            if (method.MethodKind == MethodKind.Constructor)
                return (ISymbol?)method.ContainingType ?? method;

            if (method.AssociatedSymbol is IPropertySymbol or IEventSymbol)
                return method.AssociatedSymbol;
        }

        if (symbol is IFieldSymbol field &&
            field.AssociatedSymbol is IPropertySymbol or IEventSymbol)
        {
            return field.AssociatedSymbol;
        }

        return symbol;
    }

    private static bool IsReferenceCandidate(SyntaxNode node)
        => node is IdentifierNameSyntax
            or MemberAccessExpressionSyntax
            or MemberBindingExpressionSyntax;

    private static bool TryMatchesSymbol(SymbolInfo symbolInfo, ISymbol targetSymbol, SyntaxNode node, out TextSpan span)
    {
        span = GetReferenceSpan(node);

        if (symbolInfo.Symbol is not null &&
            SymbolEqualityComparer.Default.Equals(NormalizeSymbol(symbolInfo.Symbol), targetSymbol))
        {
            return true;
        }

        if (symbolInfo.CandidateSymbols.IsDefaultOrEmpty)
            return false;

        foreach (var candidate in symbolInfo.CandidateSymbols)
        {
            if (SymbolEqualityComparer.Default.Equals(NormalizeSymbol(candidate), targetSymbol))
                return true;
        }

        return false;
    }

    internal static TextSpan GetReferenceSpan(SyntaxNode node)
        => node switch
        {
            IdentifierNameSyntax identifier => identifier.Identifier.Span,
            MemberAccessExpressionSyntax memberAccess => memberAccess.Name.Span,
            MemberBindingExpressionSyntax memberBinding => memberBinding.Name.Span,
            ParameterSyntax parameter => parameter.Identifier.Span,
            MethodDeclarationSyntax method => method.Identifier.Span,
            FunctionStatementSyntax function => function.Identifier.Span,
            PropertyDeclarationSyntax property => property.Identifier.Span,
            EventDeclarationSyntax @event => @event.Identifier.Span,
            ConstructorDeclarationSyntax constructor => constructor.InitKeyword.Span,
            ParameterlessConstructorDeclarationSyntax init => init.InitKeyword.Span,
            DelegateDeclarationSyntax @delegate => @delegate.Identifier.Span,
            VariableDeclaratorSyntax declarator => declarator.Identifier.Span,
            TypeDeclarationSyntax typeDeclaration => typeDeclaration.Identifier.Span,
            _ => node.Span
        };

    private static string BuildKey(string path, TextSpan span)
        => $"{path}|{span.Start}:{span.End}";
}

internal readonly record struct ReferenceResult(string FilePath, SourceText SourceText, TextSpan Span);
