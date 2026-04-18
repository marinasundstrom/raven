using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Threading;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

/// <summary>
/// Coordinates a <see cref="Solution"/> instance and raises events when it changes.
/// </summary>
public class Workspace
{
    private Solution _currentSolution;
    private readonly object _compilationGate = new();
    private readonly Dictionary<ProjectId, ProjectCompilationState> _projectCompilations = new();
    private readonly Dictionary<ProjectId, ProjectCompilationState> _analysisProjectCompilations = new();

    protected Workspace(string kind)
        : this(kind, HostServices.Default)
    {
    }

    protected Workspace(string kind, HostServices services)
    {
        Kind = kind;
        Services = services ?? throw new ArgumentNullException(nameof(services));
        _currentSolution = new Solution(services, this);
    }

    public string Kind { get; }

    /// <summary>Services available to this workspace.</summary>
    public HostServices Services { get; }

    public event EventHandler<WorkspaceChangeEventArgs>? WorkspaceChanged;

    public Solution CurrentSolution => _currentSolution;

    /// <summary>Creates a new empty <see cref="Solution"/> using the workspace services.</summary>
    public Solution CreateSolution() => new Solution(Services, this);

    /// <summary>Opens the specified <see cref="Solution"/> as the current solution.</summary>
    public void OpenSolution(Solution solution)
    {
        if (solution is null) throw new ArgumentNullException(nameof(solution));
        if (!ReferenceEquals(solution.Services, Services))
            throw new InvalidOperationException("Solution was created with different host services.");
        if (!ReferenceEquals(solution.Workspace, this))
            throw new InvalidOperationException("Solution was created with different workspace.");

        TryApplyChanges(solution);
    }

    /// <summary>Attempts to apply a new solution and raises the appropriate change event.</summary>
    public bool TryApplyChanges(Solution newSolution)
    {
        if (newSolution is null) throw new ArgumentNullException(nameof(newSolution));
        if (!ReferenceEquals(newSolution.Services, Services))
            throw new InvalidOperationException("Solution was created with different host services.");
        if (!ReferenceEquals(newSolution.Workspace, this))
            throw new InvalidOperationException("Solution was created with different workspace.");
        var oldSolution = _currentSolution;
        if (ReferenceEquals(oldSolution, newSolution)) return true;

        var (kind, projectId, documentId) = ComputeChangeKind(oldSolution, newSolution);
        _currentSolution = newSolution;

        // drop compilation caches for removed projects
        var removed = _projectCompilations.Keys.Where(id => newSolution.GetProject(id) is null).ToList();
        foreach (var id in removed)
            _projectCompilations.Remove(id);

        removed = _analysisProjectCompilations.Keys.Where(id => newSolution.GetProject(id) is null).ToList();
        foreach (var id in removed)
            _analysisProjectCompilations.Remove(id);

        OnWorkspaceChanged(new WorkspaceChangeEventArgs(kind, oldSolution, newSolution, projectId, documentId));
        return true;
    }

    private static (WorkspaceChangeKind kind, ProjectId? projectId, DocumentId? documentId)
        ComputeChangeKind(Solution oldSolution, Solution newSolution)
    {
        foreach (var proj in newSolution.Projects)
        {
            var oldProj = oldSolution.GetProject(proj.Id);
            if (oldProj is null)
                return (WorkspaceChangeKind.ProjectAdded, proj.Id, null);

            if (oldProj.Version != proj.Version)
            {
                foreach (var doc in proj.Documents)
                {
                    var oldDoc = oldProj.GetDocument(doc.Id);
                    if (oldDoc is null)
                        return (WorkspaceChangeKind.DocumentAdded, proj.Id, doc.Id);
                    if (oldDoc.Version != doc.Version)
                        return (WorkspaceChangeKind.DocumentChanged, proj.Id, doc.Id);
                }
                foreach (var oldDoc in oldProj.Documents)
                {
                    if (proj.GetDocument(oldDoc.Id) is null)
                        return (WorkspaceChangeKind.DocumentRemoved, proj.Id, oldDoc.Id);
                }
                return (WorkspaceChangeKind.ProjectChanged, proj.Id, null);
            }
        }
        foreach (var oldProj in oldSolution.Projects)
        {
            if (newSolution.GetProject(oldProj.Id) is null)
                return (WorkspaceChangeKind.ProjectRemoved, oldProj.Id, null);
        }
        return (WorkspaceChangeKind.SolutionChanged, null, null);
    }

    protected virtual void OnWorkspaceChanged(WorkspaceChangeEventArgs e)
        => WorkspaceChanged?.Invoke(this, e);

    /// <summary>
    /// Gets a <see cref="Compilation"/> for the specified project. Results are cached
    /// and incrementally rebuilt when documents change.
    /// </summary>
    public Compilation GetCompilation(ProjectId projectId)
    {
        return GetCompilation(projectId, new HashSet<ProjectId>());
    }

    public Compilation GetCompilation(Project project)
    {
        ArgumentNullException.ThrowIfNull(project);
        return GetCompilation(project, new HashSet<ProjectId>());
    }

    public Compilation CreateAnalysisCompilation(ProjectId projectId)
    {
        var project = CurrentSolution.GetProject(projectId)
            ?? throw new ArgumentException("Project not found", nameof(projectId));

        return CreateAnalysisCompilation(project, new HashSet<ProjectId>());
    }

    private Compilation GetCompilation(ProjectId projectId, HashSet<ProjectId> building)
    {
        var project = CurrentSolution.GetProject(projectId)
            ?? throw new ArgumentException("Project not found", nameof(projectId));

        return GetCompilation(project, building, _projectCompilations);
    }

    private Compilation GetCompilation(Project project, HashSet<ProjectId> building)
    {
        return GetCompilation(project, building, _projectCompilations);
    }

    private Compilation GetCompilation(
        Project project,
        HashSet<ProjectId> building,
        Dictionary<ProjectId, ProjectCompilationState> compilationCache)
    {
        var projectId = project.Id;

        lock (_compilationGate)
        {
            if (!building.Add(projectId))
                throw new InvalidOperationException("Circular project reference detected.");

            try
            {
                if (!compilationCache.TryGetValue(projectId, out var state))
                {
                    return BuildCompilation(project, null, building, compilationCache);
                }

                if (state.Version == project.Version)
                    return state.Compilation;

                return BuildCompilation(project, state, building, compilationCache);
            }
            finally
            {
                building.Remove(projectId);
            }
        }
    }

    private Compilation CreateAnalysisCompilation(Project project, HashSet<ProjectId> building)
    {
        ArgumentNullException.ThrowIfNull(project);

        return GetCompilation(project, building, _analysisProjectCompilations);
    }

    private Compilation BuildCompilation(
        Project project,
        ProjectCompilationState? state,
        HashSet<ProjectId> building,
        Dictionary<ProjectId, ProjectCompilationState> compilationCache)
    {
        state ??= new ProjectCompilationState();
        var previousCompilation = state.Compilation;

        var syntaxTrees = new List<SyntaxTree>();
        var reusedSyntaxTrees = ImmutableArray.CreateBuilder<SyntaxTree>();
        var changedExecutableOwners = new List<(SyntaxTree Tree, ImmutableArray<Compilation.ExecutableOwnerDescriptor> Descriptors)>();
        var matchedExecutableOwners = new List<(SyntaxTree Tree, ImmutableArray<Compilation.MatchedExecutableOwner> Matches, ImmutableDictionary<Compilation.ExecutableOwnerDescriptor, Compilation.OwnerRelativeTextChange> OwnerChanges)>();
        var documentStates = state.DocumentStates;
        var presentDocs = new HashSet<DocumentId>();

        foreach (var doc in project.Documents)
        {
            presentDocs.Add(doc.Id);
            var tree = doc.SyntaxTree;
            if (tree is null)
                continue;
            if (documentStates.TryGetValue(doc.Id, out var docState) && docState.Version == doc.Version)
            {
                syntaxTrees.Add(docState.SyntaxTree);
                reusedSyntaxTrees.Add(docState.SyntaxTree);
            }
            else
            {
                syntaxTrees.Add(tree);
                if (docState is not null)
                {
                    var (changedOwners, matchedOwners, ownerChanges) = AnalyzeExecutableOwnerChanges(docState.SyntaxTree, tree);
                    changedExecutableOwners.Add((tree, changedOwners));
                    matchedExecutableOwners.Add((tree, matchedOwners, ownerChanges));
                }

                documentStates[doc.Id] = new DocumentState(doc.Version, tree);
            }
        }

        // remove cached documents no longer present
        var toRemove = documentStates.Keys.Where(id => !presentDocs.Contains(id)).ToList();
        foreach (var id in toRemove)
            documentStates.Remove(id);

        var references = new List<MetadataReference>();
        references.AddRange(project.MetadataReferences);

        foreach (var projRef in project.ProjectReferences)
        {
            var referencedProject = project.Solution.GetProject(projRef.ProjectId)
                ?? throw new ArgumentException("Project not found", nameof(projRef.ProjectId));
            var referencedCompilation = GetCompilation(referencedProject, building, compilationCache);
            var compRef = referencedCompilation.ToMetadataReference();
            references.Add(compRef);
        }

        var compilation = Compilation.Create(project.AssemblyName ?? project.Name,
            syntaxTrees.ToArray(), references.ToArray(), [.. project.MacroReferences], project.CompilationOptions);

        var canReuseSemanticIncrementalState = previousCompilation is not null &&
                                               changedExecutableOwners.Count == 0;

        if (canReuseSemanticIncrementalState)
        {
            compilation.InitializeIncrementalState(previousCompilation.CreateIncrementalState(
                reusedSyntaxTrees.ToImmutable(),
                matchedExecutableOwners
                    .Where(static entry => !entry.Matches.IsDefaultOrEmpty)
                    .Select(static entry => new Compilation.IncrementalMatchedSyntaxTree(entry.Tree, entry.Matches[0].PreviousSyntaxTree, entry.Matches, entry.OwnerChanges))
                    .ToImmutableArray()));
            compilation.AdoptIncrementalReuseFrom(previousCompilation);
        }

        foreach (var (tree, descriptors) in changedExecutableOwners)
            compilation.RegisterChangedExecutableOwnerDescriptors(tree, descriptors);

        foreach (var (tree, matches, _) in matchedExecutableOwners)
            compilation.RegisterMatchedExecutableOwners(tree, matches);

        state.Version = project.Version;
        state.Compilation = compilation;
        compilationCache[project.Id] = state;

        return compilation;
    }

    private sealed class ProjectCompilationState
    {
        public VersionStamp Version;
        public Compilation? Compilation;
        public Dictionary<DocumentId, DocumentState> DocumentStates { get; } = new();
    }

    private sealed record DocumentState(VersionStamp Version, SyntaxTree SyntaxTree);

    private static (ImmutableArray<Compilation.ExecutableOwnerDescriptor> ChangedOwners, ImmutableArray<Compilation.MatchedExecutableOwner> MatchedOwners, ImmutableDictionary<Compilation.ExecutableOwnerDescriptor, Compilation.OwnerRelativeTextChange> OwnerChanges)
        AnalyzeExecutableOwnerChanges(
        SyntaxTree previousTree,
        SyntaxTree currentTree)
    {
        var previousOwners = GetExecutableOwners(previousTree.GetRoot()).ToArray();
        var currentOwners = GetExecutableOwners(currentTree.GetRoot()).ToArray();
        var previousOwnersByKey = previousOwners
            .GroupBy(CreateReusableOwnerMatchKey)
            .ToDictionary(
                static group => group.Key,
                static group => new List<SyntaxNode>(group));
        var matchedCurrentToPrevious = new Dictionary<Compilation.ExecutableOwnerDescriptor, Compilation.ExecutableOwnerDescriptor>();
        var usedPreviousOwners = new HashSet<Compilation.ExecutableOwnerDescriptor>();
        var partiallyChangedMatchedOwners = new HashSet<Compilation.ExecutableOwnerDescriptor>();

        var changedBuilder = ImmutableArray.CreateBuilder<Compilation.ExecutableOwnerDescriptor>();
        var matchedBuilder = ImmutableArray.CreateBuilder<Compilation.MatchedExecutableOwner>();
        var ownerChanges = ImmutableDictionary.CreateBuilder<Compilation.ExecutableOwnerDescriptor, Compilation.OwnerRelativeTextChange>();

        foreach (var owner in currentOwners)
        {
            var descriptor = new Compilation.ExecutableOwnerDescriptor(owner.Span, owner.Kind);
            var matchKey = CreateReusableOwnerMatchKey(owner);
            if (!previousOwnersByKey.TryGetValue(matchKey, out var candidates))
            {
                changedBuilder.Add(descriptor);
                continue;
            }

            Compilation.ExecutableOwnerDescriptor? currentParentDescriptor =
                TryGetReusableContainingExecutableOwnerDescriptor(owner, out var parentDescriptor)
                    ? parentDescriptor
                    : null;
            SyntaxNode? previousOwner = null;

            foreach (var candidate in candidates)
            {
                var candidateDescriptor = new Compilation.ExecutableOwnerDescriptor(candidate.Span, candidate.Kind);
                if (usedPreviousOwners.Contains(candidateDescriptor))
                    continue;

                if (currentParentDescriptor is { } requiredParentDescriptor)
                {
                    if (!matchedCurrentToPrevious.TryGetValue(requiredParentDescriptor, out var matchedPreviousParentDescriptor))
                        continue;

                    if (partiallyChangedMatchedOwners.Contains(requiredParentDescriptor))
                        continue;

                    if (!TryGetReusableContainingExecutableOwnerDescriptor(candidate, out var candidateParentDescriptor) ||
                        candidateParentDescriptor != matchedPreviousParentDescriptor)
                    {
                        continue;
                    }
                }

                previousOwner = candidate;
                usedPreviousOwners.Add(candidateDescriptor);
                break;
            }

            if (previousOwner is null)
            {
                changedBuilder.Add(descriptor);
                continue;
            }

            var previousDescriptor = new Compilation.ExecutableOwnerDescriptor(previousOwner.Span, previousOwner.Kind);
            matchedCurrentToPrevious[descriptor] = previousDescriptor;
            matchedBuilder.Add(new Compilation.MatchedExecutableOwner(
                previousTree,
                descriptor,
                previousDescriptor));

            if (TryComputeOwnerRelativeTextChange(previousOwner, owner, out var ownerChange))
            {
                ownerChanges[descriptor] = ownerChange;
                partiallyChangedMatchedOwners.Add(descriptor);
                changedBuilder.Add(descriptor);
            }
        }

        return (changedBuilder.ToImmutable(), matchedBuilder.ToImmutable(), ownerChanges.ToImmutable());
    }

    private static (SyntaxKind Kind, string Identity) CreateReusableOwnerMatchKey(SyntaxNode owner)
    {
        return owner switch
        {
            MethodDeclarationSyntax method => (owner.Kind, $"method:{method.Identifier.ValueText}:{method.ParameterList?.Parameters.Count ?? 0}:{method.TypeParameterList?.Parameters.Count ?? 0}"),
            ConstructorDeclarationSyntax ctor => (owner.Kind, $"ctor:{ctor.ParameterList?.Parameters.Count ?? 0}"),
            ParameterlessConstructorDeclarationSyntax => (owner.Kind, "ctor:0"),
            PropertyDeclarationSyntax property => (owner.Kind, $"property:{property.Identifier.ValueText}"),
            EventDeclarationSyntax @event => (owner.Kind, $"event:{@event.Identifier.ValueText}"),
            AccessorDeclarationSyntax accessor => (owner.Kind, $"accessor:{accessor.Keyword.Kind}"),
            _ => (owner.Kind, owner.ToFullString())
        };
    }

    private static bool TryComputeOwnerRelativeTextChange(
        SyntaxNode previousOwner,
        SyntaxNode currentOwner,
        out Compilation.OwnerRelativeTextChange ownerChange)
    {
        var previousText = previousOwner.ToFullString();
        var currentText = currentOwner.ToFullString();

        var start = 0;
        while (start < previousText.Length &&
               start < currentText.Length &&
               previousText[start] == currentText[start])
        {
            start++;
        }

        if (start == previousText.Length && start == currentText.Length)
        {
            ownerChange = default;
            return false;
        }

        var previousEnd = previousText.Length - 1;
        var currentEnd = currentText.Length - 1;

        while (previousEnd >= start &&
               currentEnd >= start &&
               previousText[previousEnd] == currentText[currentEnd])
        {
            previousEnd--;
            currentEnd--;
        }

        ownerChange = new Compilation.OwnerRelativeTextChange(
            new Text.TextSpan(start, previousEnd - start + 1),
            new Text.TextSpan(start, currentEnd - start + 1));
        return true;
    }

    private static IEnumerable<SyntaxNode> GetExecutableOwners(SyntaxNode root)
    {
        return root.DescendantNodesAndSelf().Where(static node =>
            node is FunctionExpressionSyntax
                or BaseMethodDeclarationSyntax
                or BaseConstructorDeclarationSyntax
                or ParameterlessConstructorDeclarationSyntax
                or AccessorDeclarationSyntax
                or PropertyDeclarationSyntax
                or EventDeclarationSyntax
                or GlobalStatementSyntax
                or CompilationUnitSyntax);
    }

    private static bool TryGetReusableContainingExecutableOwnerDescriptor(
        SyntaxNode node,
        out Compilation.ExecutableOwnerDescriptor descriptor)
    {
        var parentOwner = node.Ancestors().FirstOrDefault(static current =>
            current is FunctionExpressionSyntax
                or BaseMethodDeclarationSyntax
                or BaseConstructorDeclarationSyntax
                or ParameterlessConstructorDeclarationSyntax
                or AccessorDeclarationSyntax
                or PropertyDeclarationSyntax
                or EventDeclarationSyntax
                or GlobalStatementSyntax);

        if (parentOwner is null)
        {
            descriptor = default;
            return false;
        }

        descriptor = new Compilation.ExecutableOwnerDescriptor(parentOwner.Span, parentOwner.Kind);
        return true;
    }

    /// <summary>
    /// Gets diagnostics for the specified project, including analyzer diagnostics.
    /// </summary>
    public ImmutableArray<Diagnostic> GetDiagnostics(
        ProjectId projectId,
        CompilationWithAnalyzersOptions? analyzerOptions = null,
        CancellationToken cancellationToken = default)
    {
        var project = CurrentSolution.GetProject(projectId)
            ?? throw new ArgumentException("Project not found", nameof(projectId));

        var compilation = CreateAnalysisCompilation(projectId);
        var diagnostics = compilation.GetDiagnostics(analyzerOptions, cancellationToken).ToHashSet();

        if (project.CompilationOptions?.RunAnalyzers != false)
        {
            foreach (var reference in project.AnalyzerReferences)
            {
                foreach (var analyzer in reference.GetAnalyzers())
                {
                    var isInternalAnalyzer = AnalyzerDiagnosticIdValidator.IsInternalAnalyzer(analyzer);
                    IEnumerable<Diagnostic> analyzerDiagnostics;
                    try
                    {
                        analyzerDiagnostics = analyzer.Analyze(compilation, cancellationToken);
                    }
                    catch (OperationCanceledException)
                    {
                        throw;
                    }
                    catch
                    {
                        // Analyzer failures should not stop normal compilation diagnostics.
                        continue;
                    }

                    foreach (var diagnostic in analyzerDiagnostics)
                    {
                        AnalyzerDiagnosticIdValidator.Validate(analyzer, diagnostic, isInternalAnalyzer);

                        var mapped = compilation.ApplyCompilationOptions(diagnostic, analyzerOptions?.ReportSuppressedDiagnostics ?? false);
                        if (mapped is not null)
                            diagnostics.Add(mapped);
                    }
                }
            }
        }

        return diagnostics.OrderBy(d => d.Location).ToImmutableArray();
    }

    public ImmutableArray<Diagnostic> GetDocumentDiagnostics(
        ProjectId projectId,
        DocumentId documentId,
        CompilationWithAnalyzersOptions? analyzerOptions = null,
        CancellationToken cancellationToken = default)
    {
        var project = CurrentSolution.GetProject(projectId)
            ?? throw new ArgumentException("Project not found", nameof(projectId));

        var document = project.GetDocument(documentId)
            ?? throw new ArgumentException("Document not found", nameof(documentId));

        var syntaxTree = document.SyntaxTree
            ?? throw new InvalidOperationException("Document does not have a syntax tree.");

        var compilation = CreateAnalysisCompilation(projectId);
        return compilation.GetDiagnostics(syntaxTree, analyzerOptions, cancellationToken);
    }

    public ImmutableArray<Diagnostic> GetDocumentSyntaxDiagnostics(
        ProjectId projectId,
        DocumentId documentId,
        CompilationWithAnalyzersOptions? analyzerOptions = null,
        CancellationToken cancellationToken = default)
    {
        var project = CurrentSolution.GetProject(projectId)
            ?? throw new ArgumentException("Project not found", nameof(projectId));

        var document = project.GetDocument(documentId)
            ?? throw new ArgumentException("Document not found", nameof(documentId));

        var syntaxTree = document.SyntaxTree
            ?? throw new InvalidOperationException("Document does not have a syntax tree.");

        var compilation = GetCompilation(projectId);
        return compilation.GetSyntaxDiagnostics(syntaxTree, analyzerOptions, cancellationToken);
    }

    /// <summary>
    /// Gets code fixes for project diagnostics produced by the supplied providers.
    /// </summary>
    public ImmutableArray<CodeFix> GetCodeFixes(
        ProjectId projectId,
        IEnumerable<CodeFixProvider> providers,
        CompilationWithAnalyzersOptions? analyzerOptions = null,
        CancellationToken cancellationToken = default)
    {
        if (providers is null)
            throw new ArgumentNullException(nameof(providers));

        var providerList = providers.ToImmutableArray();
        if (providerList.Length == 0)
            return ImmutableArray<CodeFix>.Empty;

        var providerMap = new Dictionary<string, List<CodeFixProvider>>(StringComparer.OrdinalIgnoreCase);
        var wildcardProviders = new List<CodeFixProvider>();
        foreach (var provider in providerList)
        {
            foreach (var id in provider.FixableDiagnosticIds)
            {
                if (string.Equals(id, "*", StringComparison.Ordinal))
                {
                    wildcardProviders.Add(provider);
                    continue;
                }

                if (!providerMap.TryGetValue(id, out var list))
                {
                    list = [];
                    providerMap.Add(id, list);
                }

                list.Add(provider);
            }
        }

        var project = CurrentSolution.GetProject(projectId)
            ?? throw new ArgumentException("Project not found", nameof(projectId));

        var fixes = ImmutableArray.CreateBuilder<CodeFix>();
        var diagnostics = GetDiagnostics(projectId, analyzerOptions, cancellationToken);

        foreach (var diagnostic in diagnostics)
        {
            var hasSpecificProviders = providerMap.TryGetValue(diagnostic.Id, out var specificProviders);
            if (!hasSpecificProviders && wildcardProviders.Count == 0)
                continue;

            if (!TryGetDiagnosticDocument(project, diagnostic, out var document))
                continue;

            var seenProviders = new HashSet<CodeFixProvider>();

            if (hasSpecificProviders && specificProviders is not null)
            {
                foreach (var provider in specificProviders)
                    seenProviders.Add(provider);
            }

            foreach (var provider in wildcardProviders)
                seenProviders.Add(provider);

            foreach (var provider in seenProviders)
            {
                var actionBucket = new List<CodeAction>();
                var context = new CodeFixContext(
                    document,
                    diagnostic,
                    actionBucket.Add,
                    cancellationToken);

                provider.RegisterCodeFixes(context);

                foreach (var action in actionBucket)
                    fixes.Add(new CodeFix(document.Id, diagnostic, action, provider));
            }
        }

        return fixes
            .OrderBy(x => x.Diagnostic.Location)
            .ThenBy(x => x.Action.Title, StringComparer.Ordinal)
            .ToImmutableArray();
    }

    /// <summary>
    /// Gets context-driven refactorings for the specified document and selection span.
    /// </summary>
    public ImmutableArray<CodeRefactoring> GetRefactorings(
        DocumentId documentId,
        IEnumerable<CodeRefactoringProvider> providers,
        TextSpan span,
        CancellationToken cancellationToken = default)
    {
        if (providers is null)
            throw new ArgumentNullException(nameof(providers));

        var providerList = providers.ToImmutableArray();
        if (providerList.Length == 0)
            return ImmutableArray<CodeRefactoring>.Empty;

        var document = CurrentSolution.GetDocument(documentId)
            ?? throw new ArgumentException("Document not found", nameof(documentId));

        var refactorings = ImmutableArray.CreateBuilder<CodeRefactoring>();

        foreach (var provider in providerList)
        {
            var actionBucket = new List<CodeAction>();
            var context = new CodeRefactoringContext(
                document,
                span,
                actionBucket.Add,
                cancellationToken);

            try
            {
                provider.RegisterRefactorings(context);
            }
            catch (OperationCanceledException)
            {
                throw;
            }
            catch
            {
                continue;
            }

            foreach (var action in actionBucket)
                refactorings.Add(new CodeRefactoring(documentId, span, action, provider));
        }

        return refactorings
            .OrderBy(x => x.Action.Title, StringComparer.Ordinal)
            .ToImmutableArray();
    }

    /// <summary>
    /// Applies code fixes one-at-a-time, reanalyzing between each application.
    /// </summary>
    public ApplyCodeFixesResult ApplyCodeFixes(
        ProjectId projectId,
        IEnumerable<CodeFixProvider> providers,
        Func<CodeFix, bool>? predicate = null,
        int maxIterations = 100,
        CompilationWithAnalyzersOptions? analyzerOptions = null,
        CancellationToken cancellationToken = default)
    {
        if (providers is null)
            throw new ArgumentNullException(nameof(providers));
        if (maxIterations < 1)
            throw new ArgumentOutOfRangeException(nameof(maxIterations));

        var applied = 0;
        var solution = CurrentSolution;
        var appliedFixes = ImmutableArray.CreateBuilder<CodeFix>();

        for (var i = 0; i < maxIterations; i++)
        {
            TryApplyChanges(solution);

            var fixes = GetCodeFixes(projectId, providers, analyzerOptions, cancellationToken);
            if (predicate is not null)
                fixes = fixes.Where(predicate).ToImmutableArray();

            if (fixes.Length == 0)
                break;

            var selectedFix = fixes[0];
            var updated = selectedFix.Action.GetChangedSolution(solution, cancellationToken);
            if (updated.Version == solution.Version)
                break;

            solution = updated;
            applied++;
            appliedFixes.Add(selectedFix);
        }

        return new ApplyCodeFixesResult(solution, applied, appliedFixes.ToImmutable());
    }

    private static bool TryGetDiagnosticDocument(Project project, Diagnostic diagnostic, out Document document)
    {
        var sourceTree = diagnostic.Location.SourceTree;
        if (sourceTree is not null)
        {
            foreach (var candidate in project.Documents)
            {
                var candidateTree = candidate.GetSyntaxTreeAsync().GetAwaiter().GetResult();
                if (ReferenceEquals(candidateTree, sourceTree))
                {
                    document = candidate;
                    return true;
                }
            }
        }

        if (diagnostic.Location.GetLineSpan() is { Path: { Length: > 0 } path })
        {
            var normalizedDiagnosticPath = Path.GetFullPath(path);
            foreach (var candidate in project.Documents)
            {
                if (string.IsNullOrWhiteSpace(candidate.FilePath))
                    continue;

                var normalizedCandidatePath = Path.GetFullPath(candidate.FilePath);
                if (string.Equals(normalizedCandidatePath, normalizedDiagnosticPath, StringComparison.OrdinalIgnoreCase))
                {
                    document = candidate;
                    return true;
                }
            }
        }

        document = null!;
        return false;
    }
}
