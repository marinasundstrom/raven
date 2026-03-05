using System.Collections.Immutable;
using System.Threading;

using Microsoft.Extensions.Logging;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Protocol.Workspace;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using LspSymbolKind = OmniSharp.Extensions.LanguageServer.Protocol.Models.SymbolKind;

namespace Raven.LanguageServer;

internal sealed class WorkspaceSymbolsHandler : IWorkspaceSymbolsHandler
{
    private readonly WorkspaceManager _workspaceManager;
    private readonly ILogger<WorkspaceSymbolsHandler> _logger;

    public WorkspaceSymbolsHandler(WorkspaceManager workspaceManager, ILogger<WorkspaceSymbolsHandler> logger)
    {
        _workspaceManager = workspaceManager;
        _logger = logger;
    }

    public WorkspaceSymbolRegistrationOptions GetRegistrationOptions(WorkspaceSymbolCapability capability, ClientCapabilities clientCapabilities)
        => new();

    public void SetCapability(WorkspaceSymbolCapability capability)
    {
    }

    public async Task<Container<WorkspaceSymbol>?> Handle(WorkspaceSymbolParams request, CancellationToken cancellationToken)
    {
        try
        {
            var query = request.Query?.Trim() ?? string.Empty;
            var symbols = await WorkspaceSymbolSearchService
                .FindSymbolsAsync(_workspaceManager.GetProjectsSnapshot(), query, cancellationToken)
                .ConfigureAwait(false);

            return new Container<WorkspaceSymbol>(symbols);
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            return new Container<WorkspaceSymbol>();
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Workspace symbol search failed for query '{Query}'.", request.Query);
            return new Container<WorkspaceSymbol>();
        }
    }
}

internal static class WorkspaceSymbolSearchService
{
    private const int MaxResults = 200;
    private static readonly SemaphoreSlim s_indexBuildGate = new(1, 1);
    private static SymbolIndexCache? s_cachedIndex;

    public static async Task<IReadOnlyList<WorkspaceSymbol>> FindSymbolsAsync(
        IReadOnlyList<Project> projects,
        string query,
        CancellationToken cancellationToken)
    {
        var entries = await GetOrBuildIndexAsync(projects, cancellationToken).ConfigureAwait(false);
        var results = new List<WorkspaceSymbol>();
        var hasQuery = !string.IsNullOrWhiteSpace(query);

        foreach (var entry in entries)
        {
            cancellationToken.ThrowIfCancellationRequested();

            if (hasQuery && !MatchesQuery(entry.Name, entry.ContainerName, query))
                continue;

            results.Add(new WorkspaceSymbol
            {
                Name = entry.Name,
                Kind = entry.Kind,
                ContainerName = entry.ContainerName,
                Location = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Location
                {
                    Uri = entry.Uri,
                    Range = entry.Range
                }
            });

            if (results.Count >= MaxResults)
                return results;
        }

        return results;
    }

    private static bool IsDeclarationCandidate(SyntaxNode node)
        => node is BaseNamespaceDeclarationSyntax
            or TypeDeclarationSyntax
            or EnumDeclarationSyntax
            or UnionDeclarationSyntax
            or MethodDeclarationSyntax
            or ConstructorDeclarationSyntax
            or ParameterlessConstructorDeclarationSyntax
            or FunctionStatementSyntax
            or DelegateDeclarationSyntax
            or PropertyDeclarationSyntax
            or IndexerDeclarationSyntax
            or FieldDeclarationSyntax
            or EventDeclarationSyntax;

    private static bool MatchesQuery(string name, string? containerName, string query)
    {
        if (name.Contains(query, StringComparison.OrdinalIgnoreCase))
            return true;

        return !string.IsNullOrWhiteSpace(containerName) &&
               containerName.Contains(query, StringComparison.OrdinalIgnoreCase);
    }

    private static string GetSymbolName(ISymbol symbol)
    {
        if (symbol is IMethodSymbol { MethodKind: MethodKind.Constructor } constructor)
            return constructor.ContainingType?.Name ?? constructor.Name;

        return symbol.Name;
    }

    private static string? GetContainerName(ISymbol symbol)
    {
        var containing = symbol.ContainingSymbol;
        if (containing is null || containing.Kind == Raven.CodeAnalysis.SymbolKind.Assembly)
            return null;

        return containing.ToDisplayString(SymbolDisplayFormat.RavenSignatureFormat
            .WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly)
            .WithKindOptions(SymbolDisplayKindOptions.None));
    }

    private static LspSymbolKind MapSymbolKind(ISymbol symbol)
    {
        return symbol switch
        {
            INamespaceSymbol => LspSymbolKind.Namespace,
            IMethodSymbol method when method.MethodKind == MethodKind.Constructor => LspSymbolKind.Constructor,
            IMethodSymbol => LspSymbolKind.Method,
            IPropertySymbol => LspSymbolKind.Property,
            IFieldSymbol => LspSymbolKind.Field,
            IEventSymbol => LspSymbolKind.Event,
            ITypeParameterSymbol => LspSymbolKind.TypeParameter,
            INamedTypeSymbol namedType => namedType.TypeKind switch
            {
                TypeKind.Class => LspSymbolKind.Class,
                TypeKind.Interface => LspSymbolKind.Interface,
                TypeKind.Struct => LspSymbolKind.Struct,
                TypeKind.Enum => LspSymbolKind.Enum,
                TypeKind.Delegate => LspSymbolKind.Function,
                _ => LspSymbolKind.Class
            },
            _ => LspSymbolKind.Variable
        };
    }

    private static TextSpan GetSelectionSpan(SyntaxNode declaration)
    {
        return declaration switch
        {
            BaseNamespaceDeclarationSyntax ns => ns.Name.Span,
            EnumDeclarationSyntax @enum => @enum.Identifier.Span,
            UnionDeclarationSyntax union => union.Identifier.Span,
            BaseTypeDeclarationSyntax type => type.Identifier.Span,
            MethodDeclarationSyntax method => method.Identifier.Span,
            ConstructorDeclarationSyntax ctor => ctor.InitKeyword.Span,
            ParameterlessConstructorDeclarationSyntax ctor => ctor.InitKeyword.Span,
            FunctionStatementSyntax function => function.Identifier.Span,
            DelegateDeclarationSyntax @delegate => @delegate.Identifier.Span,
            PropertyDeclarationSyntax property => property.Identifier.Span,
            IndexerDeclarationSyntax indexer => indexer.Identifier.Span,
            FieldDeclarationSyntax field => field.Declaration.Declarators.FirstOrDefault()?.Identifier.Span ?? field.Span,
            EventDeclarationSyntax @event => @event.Identifier.Span,
            _ => declaration.Span
        };
    }

    private static async Task<ImmutableArray<WorkspaceSymbolEntry>> GetOrBuildIndexAsync(
        IReadOnlyList<Project> projects,
        CancellationToken cancellationToken)
    {
        var fingerprint = ComputeFingerprint(projects);
        var cached = s_cachedIndex;
        if (cached is not null && cached.Fingerprint == fingerprint)
            return cached.Entries;

        await s_indexBuildGate.WaitAsync(cancellationToken).ConfigureAwait(false);
        try
        {
            cached = s_cachedIndex;
            if (cached is not null && cached.Fingerprint == fingerprint)
                return cached.Entries;

            var entries = await BuildIndexAsync(projects, cancellationToken).ConfigureAwait(false);
            s_cachedIndex = new SymbolIndexCache(fingerprint, entries);
            return entries;
        }
        finally
        {
            s_indexBuildGate.Release();
        }
    }

    private static async Task<ImmutableArray<WorkspaceSymbolEntry>> BuildIndexAsync(
        IReadOnlyList<Project> projects,
        CancellationToken cancellationToken)
    {
        var entries = ImmutableArray.CreateBuilder<WorkspaceSymbolEntry>();
        var seen = new HashSet<string>(StringComparer.Ordinal);

        foreach (var project in projects.OrderBy(p => p.Id.ToString(), StringComparer.Ordinal))
        {
            cancellationToken.ThrowIfCancellationRequested();
            var compilation = project.Solution.Workspace?.GetCompilation(project.Id);
            if (compilation is null)
                continue;

            foreach (var document in project.Documents.OrderBy(d => d.FilePath, StringComparer.OrdinalIgnoreCase))
            {
                cancellationToken.ThrowIfCancellationRequested();
                var syntaxTree = await document.GetSyntaxTreeAsync(cancellationToken).ConfigureAwait(false);
                if (syntaxTree is null)
                    continue;

                var path = syntaxTree.FilePath;
                if (string.IsNullOrWhiteSpace(path) || path == "file")
                    continue;

                var semanticModel = compilation.GetSemanticModel(syntaxTree);
                var root = syntaxTree.GetRoot(cancellationToken);
                var sourceText = syntaxTree.GetText();
                if (sourceText is null)
                    continue;

                foreach (var declaration in root.DescendantNodes().Where(IsDeclarationCandidate))
                {
                    cancellationToken.ThrowIfCancellationRequested();
                    var symbol = semanticModel.GetDeclaredSymbol(declaration);
                    if (symbol is null)
                        continue;

                    var name = GetSymbolName(symbol);
                    if (string.IsNullOrWhiteSpace(name))
                        continue;

                    var containerName = GetContainerName(symbol);
                    var selectionSpan = GetSelectionSpan(declaration);
                    var key = $"{path}|{selectionSpan.Start}:{selectionSpan.End}|{name}";
                    if (!seen.Add(key))
                        continue;

                    entries.Add(new WorkspaceSymbolEntry(
                        name,
                        containerName,
                        MapSymbolKind(symbol),
                        DocumentUri.FromFileSystemPath(path),
                        PositionHelper.ToRange(sourceText, selectionSpan)));
                }
            }
        }

        return entries.MoveToImmutable();
    }

    private static int ComputeFingerprint(IReadOnlyList<Project> projects)
    {
        var hash = new HashCode();

        foreach (var project in projects.OrderBy(p => p.Id.ToString(), StringComparer.Ordinal))
        {
            hash.Add(project.Id);
            hash.Add(project.Version.GetHashCode());
            hash.Add(project.Documents.Count());
        }

        return hash.ToHashCode();
    }

    private sealed record SymbolIndexCache(int Fingerprint, ImmutableArray<WorkspaceSymbolEntry> Entries);

    private sealed record WorkspaceSymbolEntry(
        string Name,
        string? ContainerName,
        LspSymbolKind Kind,
        DocumentUri Uri,
        OmniSharp.Extensions.LanguageServer.Protocol.Models.Range Range);
}
