using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;

using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    private static readonly CompletionService s_completionService = new();
    private readonly SemanticBindingState _bindingState = new();
    private readonly ConcurrentDictionary<SyntaxNodeMapKey, byte> _asyncLoweringInProgress = new();
    private readonly ConcurrentDictionary<SyntaxNode, ImmutableDictionary<AttributeSyntax, MacroExpansionResult?>> _macroExpansionCache = new();
    private readonly ConcurrentDictionary<FreestandingMacroExpressionSyntax, FreestandingMacroExpansionResult?> _freestandingMacroExpansionCache = new();
    private readonly ConcurrentDictionary<AttributeSyntax, ImmutableArray<SyntaxNode>> _expandedDeclarationCache = new();
    private readonly ConcurrentDictionary<SyntaxNode, SyntaxNode> _macroReplacementSyntaxMap = new();
    private readonly ConcurrentDictionary<TypeDeclarationSyntax, TypeDeclarationSyntax> _macroContainingTypeSyntaxMap = new();

    private readonly DeclaredSymbolLookup _declaredSymbolLookup;
    private readonly object _diagnosticsCollectionGate = new();
    private readonly object _bindingSetupGate = new();
    private readonly object _expandedRootGate = new();
    private bool _isCollectingDiagnostics;
    private int _diagnosticCollectionThreadId;
    private bool _declarationsComplete;
    private bool _rootBinderCreated;
    private bool _isEnsuringDeclarations;
    private int _declarationSetupThreadId;
    private bool _isCreatingRootBinder;
    private int _rootBinderThreadId;
    private CompilationUnitSyntax? _expandedRoot;

    private ConcurrentDictionary<SyntaxNode, Binder> _binderCache => _bindingState.BinderCache;
    private ConcurrentDictionary<SyntaxNodeMapKey, Binder> _binderCacheByKey => _bindingState.BinderCacheByKey;
    private ConcurrentDictionary<Binder, BinderLifecycleSnapshot> _binderLifecycleSnapshots => _bindingState.BinderLifecycleSnapshots;
    private ConcurrentDictionary<SyntaxNode, SymbolInfo> _symbolMappings => _bindingState.SymbolMappings;
    private ConcurrentDictionary<SyntaxNode, TypeInfo> _typeMappings => _bindingState.TypeMappings;
    private ConcurrentDictionary<SyntaxNode, BoundNode> _boundNodeCache => _bindingState.BoundNodeCache;
    private ConcurrentDictionary<SyntaxNode, (Binder, BoundNode)> _boundNodeCache2 => _bindingState.BoundNodeCacheWithBinder;
    private ConcurrentDictionary<ContextualBoundNodeCacheKey, BoundNode> _contextualBoundNodeCache => _bindingState.ContextualBoundNodeCache;
    private ConcurrentDictionary<ContextualBoundNodeCacheKey, (Binder, BoundNode)> _contextualBoundNodeCache2 => _bindingState.ContextualBoundNodeCacheWithBinder;
    private ConcurrentDictionary<SyntaxNode, BoundNode> _loweredBoundNodeCache => _bindingState.LoweredBoundNodeCache;
    private ConcurrentDictionary<SyntaxNode, (Binder, BoundNode)> _loweredBoundNodeCache2 => _bindingState.LoweredBoundNodeCacheWithBinder;
    private ConcurrentDictionary<FunctionExpressionSyntax, IMethodSymbol> _functionExpressionSymbolCache => _bindingState.FunctionExpressionSymbolCache;
    private ConcurrentDictionary<FunctionExpressionSyntax, byte> _functionExpressionSymbolCreationInProgress => _bindingState.FunctionExpressionSymbolCreationInProgress;
    private ConcurrentDictionary<SyntaxNode, byte> _functionExpressionParameterLookupInProgress => _bindingState.FunctionExpressionParameterLookupInProgress;
    private ConcurrentDictionary<SyntaxNode, byte> _functionExpressionRebindInProgress => _bindingState.FunctionExpressionRebindInProgress;
    private ConcurrentDictionary<BoundNode, SyntaxNode> _syntaxCache => _bindingState.SyntaxCache;
    private ConcurrentDictionary<BoundNode, SyntaxNode> _loweredSyntaxCache => _bindingState.LoweredSyntaxCache;
    private ConcurrentDictionary<SyntaxNode, ImmutableArray<Compilation.VisibleValueDeclaration>> _visibleValueScopeCache => _bindingState.VisibleValueScopeCache;
    private IImmutableList<Diagnostic>? _diagnostics
    {
        get => _bindingState.Diagnostics;
        set => _bindingState.Diagnostics = value;
    }

    private IImmutableList<Diagnostic>? _documentDiagnostics
    {
        get => _bindingState.DocumentDiagnostics;
        set => _bindingState.DocumentDiagnostics = value;
    }

    private DiagnosticBag _declarationDiagnostics => _bindingState.DeclarationDiagnostics;

    public bool IsDebuggingEnabled { get; set; } = true;

    public SemanticModel(Compilation compilation, SyntaxTree syntaxTree)
    {
        Compilation = compilation;
        SyntaxTree = syntaxTree;
        _declaredSymbolLookup = new DeclaredSymbolLookup(this);
    }

    public Compilation Compilation { get; }

    public SyntaxTree SyntaxTree { get; }

    private sealed class SemanticBindingState
    {
        public ConcurrentDictionary<SyntaxNode, Binder> BinderCache { get; } = new();
        public ConcurrentDictionary<SyntaxNodeMapKey, Binder> BinderCacheByKey { get; } = new();
        public ConcurrentDictionary<Binder, BinderLifecycleSnapshot> BinderLifecycleSnapshots { get; } = new();
        public ConcurrentDictionary<SyntaxNode, SymbolInfo> SymbolMappings { get; } = new();
        public ConcurrentDictionary<SyntaxNode, TypeInfo> TypeMappings { get; } = new();
        public ConcurrentDictionary<SyntaxNode, BoundNode> BoundNodeCache { get; } = new();
        public ConcurrentDictionary<SyntaxNode, (Binder, BoundNode)> BoundNodeCacheWithBinder { get; } = new();
        public ConcurrentDictionary<ContextualBoundNodeCacheKey, BoundNode> ContextualBoundNodeCache { get; } = new(ContextualBoundNodeCacheKeyComparer.Instance);
        public ConcurrentDictionary<ContextualBoundNodeCacheKey, (Binder, BoundNode)> ContextualBoundNodeCacheWithBinder { get; } = new(ContextualBoundNodeCacheKeyComparer.Instance);
        public ConcurrentDictionary<SyntaxNode, BoundNode> LoweredBoundNodeCache { get; } = new();
        public ConcurrentDictionary<SyntaxNode, (Binder, BoundNode)> LoweredBoundNodeCacheWithBinder { get; } = new();
        public ConcurrentDictionary<FunctionExpressionSyntax, IMethodSymbol> FunctionExpressionSymbolCache { get; } = new();
        public ConcurrentDictionary<FunctionExpressionSyntax, byte> FunctionExpressionSymbolCreationInProgress { get; } = new();
        public ConcurrentDictionary<SyntaxNode, byte> FunctionExpressionParameterLookupInProgress { get; } = new();
        public ConcurrentDictionary<SyntaxNode, byte> FunctionExpressionRebindInProgress { get; } = new();
        public ConcurrentDictionary<BoundNode, SyntaxNode> SyntaxCache { get; } = new(ReferenceEqualityComparer.Instance);
        public ConcurrentDictionary<BoundNode, SyntaxNode> LoweredSyntaxCache { get; } = new(ReferenceEqualityComparer.Instance);
        public ConcurrentDictionary<SyntaxNode, ImmutableArray<Compilation.VisibleValueDeclaration>> VisibleValueScopeCache { get; } = new();
        public DiagnosticBag DeclarationDiagnostics { get; } = new();
        public IImmutableList<Diagnostic>? Diagnostics { get; set; }
        public IImmutableList<Diagnostic>? DocumentDiagnostics { get; set; }
    }

    private readonly record struct ContextualBoundNodeCacheKey(SyntaxNode Node, ITypeSymbol TargetType);

    internal readonly record struct BinderLifecycleSnapshot(
        string BinderType,
        SyntaxKind NodeKind,
        Text.TextSpan NodeSpan,
        string CacheKind,
        bool IsStructuralCacheable,
        string ContainingSymbolKey,
        string? ParentBinderType,
        string? ParentContainingSymbolKey);

    private sealed class ContextualBoundNodeCacheKeyComparer : IEqualityComparer<ContextualBoundNodeCacheKey>
    {
        public static readonly ContextualBoundNodeCacheKeyComparer Instance = new();

        public bool Equals(ContextualBoundNodeCacheKey x, ContextualBoundNodeCacheKey y)
            => ReferenceEquals(x.Node, y.Node) &&
               SymbolEqualityComparer.Default.Equals(x.TargetType, y.TargetType);

        public int GetHashCode(ContextualBoundNodeCacheKey obj)
            => HashCode.Combine(
                RuntimeHelpers.GetHashCode(obj.Node),
                SymbolEqualityComparer.Default.GetHashCode(obj.TargetType));
    }

    /// <summary>
    /// Gets completion items available at a position in this semantic model's syntax tree.
    /// </summary>
    /// <param name="position">The zero-based position in the syntax tree.</param>
    /// <returns>A sequence of completion items.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown when <paramref name="position"/> is outside the tree bounds.</exception>
    public IEnumerable<CompletionItem> GetCompletions(int position)
    {
        var treeLength = SyntaxTree.GetRoot().FullSpan.End;
        if ((uint)position > (uint)treeLength)
            throw new ArgumentOutOfRangeException(nameof(position));

        return s_completionService.GetCompletions(this, position);
    }

    /// <summary>
    /// Gets completion items available at a position in this semantic model's syntax tree asynchronously.
    /// </summary>
    /// <param name="position">The zero-based position in the syntax tree.</param>
    /// <param name="cancellationToken">Token used to cancel the operation.</param>
    /// <returns>A materialized set of completion items.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown when <paramref name="position"/> is outside the tree bounds.</exception>
    public Task<ImmutableArray<CompletionItem>> GetCompletionsAsync(
        int position,
        CancellationToken cancellationToken = default)
    {
        var treeLength = SyntaxTree.GetRoot().FullSpan.End;
        if ((uint)position > (uint)treeLength)
            throw new ArgumentOutOfRangeException(nameof(position));

        return s_completionService.GetCompletionsAsync(this, position, cancellationToken);
    }

    public IImmutableList<Diagnostic> GetDiagnostics(CancellationToken cancellationToken = default)
    {
        if (_diagnostics is null)
            EnsureDiagnosticBindingCompleted(requireCompleteDeclarations: true);

        return _diagnostics;
    }

    internal IImmutableList<Diagnostic> GetDocumentDiagnostics(CancellationToken cancellationToken = default)
    {
        if (_documentDiagnostics is null)
            EnsureDiagnosticBindingCompleted(requireCompleteDeclarations: false);

        return _documentDiagnostics;
    }

    private void EnsureDiagnosticBindingCompleted(bool requireCompleteDeclarations = true)
    {
        lock (_diagnosticsCollectionGate)
        {
            if (requireCompleteDeclarations && _diagnostics is not null)
                return;

            if (!requireCompleteDeclarations && _documentDiagnostics is not null)
                return;

            var currentThreadId = Environment.CurrentManagedThreadId;
            if (_isCollectingDiagnostics)
            {
                if (_diagnosticCollectionThreadId == currentThreadId)
                    return;
            }

            _isCollectingDiagnostics = true;
            _diagnosticCollectionThreadId = currentThreadId;

            try
            {
                var root = SyntaxTree.GetRoot();
                if (!requireCompleteDeclarations &&
                    TryCollectTransferredDocumentDiagnostics(root, out var transferredDocumentDiagnostics))
                {
                    _documentDiagnostics = transferredDocumentDiagnostics;
                    return;
                }

                if (requireCompleteDeclarations)
                {
                    Compilation.EnsureSourceDeclarationsComplete();
                }
                else
                {
                    EnsureDeclarations();
                    EnsureMemberSignaturesDeclared();
                }

                EnsureDeclarations();
                var binder = requireCompleteDeclarations
                    ? GetRootBinderForCompleteDiagnostics(root)
                    : GetBinderForIncrementalSemanticQuery(root);

                var diagnosticsBuilder = ImmutableArray.CreateBuilder<Diagnostic>();
                if (!TryCollectIncrementalDiagnostics(root, binder, diagnosticsBuilder))
                {
                    Traverse(root, binder);

                    DocumentationCommentValidator.Analyze(this, root, binder.Diagnostics);

                    diagnosticsBuilder.AddRange(_binderCache.Values
                        .SelectMany(static binderState => binderState.Diagnostics.AsEnumerable()));
                }

                diagnosticsBuilder.AddRange(_declarationDiagnostics.AsEnumerable());
                var diagnostics = diagnosticsBuilder
                    .Distinct()
                    .ToImmutableArray();

                if (requireCompleteDeclarations)
                    _diagnostics = diagnostics;
                else
                    _documentDiagnostics = diagnostics;

                StoreSemanticDiagnosticDescriptors(root, diagnostics.ToImmutableArray());
            }
            finally
            {
                _isCollectingDiagnostics = false;
                _diagnosticCollectionThreadId = 0;
            }
        }

        bool TryCollectTransferredDocumentDiagnostics(
            SyntaxNode root,
            out IImmutableList<Diagnostic> diagnostics)
        {
            diagnostics = default!;
            if (Compilation.IsSemanticDiagnosticTransferBlocked(SyntaxTree))
                return false;

            var allOwners = GetExecutableOwnersForDiagnostics(root).ToArray();
            if (allOwners.Length == 0 ||
                allOwners.Any(Compilation.IsChangedExecutableOwner))
            {
                return false;
            }

            var diagnosticsBuilder = ImmutableArray.CreateBuilder<Diagnostic>();
            var transferredAny = false;
            foreach (var owner in allOwners)
            {
                if (TryGetTransferredSemanticDiagnostics(owner, out var ownerDiagnostics))
                {
                    transferredAny = true;
                    diagnosticsBuilder.AddRange(ownerDiagnostics);
                }
            }

            if (!transferredAny)
                return false;

            diagnostics = diagnosticsBuilder
                .Distinct()
                .ToImmutableArray();
            return true;
        }

        void Traverse(SyntaxNode node, Binder currentBinder)
        {
            foreach (var child in node.ChildNodes())
            {
                var childBinder = requireCompleteDeclarations
                    ? GetBinder(child, currentBinder)
                    : GetBinderForIncrementalSemanticQuery(child, currentBinder);

                if (child is AttributeSyntax attributeSyntax)
                {
                    if (attributeSyntax.IsMacroAttribute())
                    {
                        _ = GetMacroExpansion(attributeSyntax);
                        continue;
                    }

                    // Attribute names/arguments have attribute-specific binding rules.
                    // Binding descendant expressions directly can produce bogus name lookup
                    // diagnostics (e.g. [Obsolete] resolving as an identifier expression).
                    var attributeBinder = childBinder as AttributeBinder
                        ?? new AttributeBinder(childBinder.ContainingSymbol, childBinder);
                    _ = attributeBinder.BindAttribute(attributeSyntax);
                    continue;
                }

                if (child is GlobalStatementSyntax global)
                {
                    // Bind the contained statement so locals are registered
                    childBinder.GetOrBind(global.Statement);
                    BindStatementAttributeSyntaxes(global, childBinder);
                    continue;
                }

                if (child is AccessorListSyntax)
                {
                    Traverse(child, childBinder);
                    continue;
                }

                if (child is ExpressionSyntax || child is StatementSyntax)
                {
                    childBinder.GetOrBind(child);
                    BindStatementAttributeSyntaxes(child, childBinder);
                    continue;
                }

                Traverse(child, childBinder);
            }
        }

        bool TryCollectIncrementalDiagnostics(
            SyntaxNode root,
            Binder rootBinder,
            ImmutableArray<Diagnostic>.Builder diagnosticsBuilder)
        {
            if (Compilation.IsSemanticDiagnosticTransferBlocked(SyntaxTree))
                return false;

            var changedOwners = GetExecutableOwnersForDiagnostics(root)
                .Where(Compilation.IsChangedExecutableOwner)
                .ToArray();
            var allOwners = GetExecutableOwnersForDiagnostics(root).ToArray();

            if (changedOwners.Length == 0)
                return TryCollectTransferredDiagnosticsForAllOwners(root, rootBinder, allOwners, diagnosticsBuilder);

            var changedOwnerSet = changedOwners
                .Select(static owner => new Compilation.ExecutableOwnerDescriptor(owner.Span, owner.Kind))
                .ToHashSet();

            var ownersToBind = changedOwners
                .Where(owner => !owner.DescendantNodes()
                    .Any(descendant => IsExecutableOwnerForDiagnostics(descendant) &&
                                       changedOwnerSet.Contains(new Compilation.ExecutableOwnerDescriptor(descendant.Span, descendant.Kind))))
                .ToArray();

            if (ownersToBind.Length == 0)
                return false;

            ownersToBind = ExpandTopLevelOrderSensitiveOwners(root, ownersToBind);

            var ownersToBindSet = ownersToBind
                .Select(static owner => new Compilation.ExecutableOwnerDescriptor(owner.Span, owner.Kind))
                .ToHashSet();

            foreach (var owner in allOwners)
            {
                if (ownersToBindSet.Contains(new Compilation.ExecutableOwnerDescriptor(owner.Span, owner.Kind)))
                    continue;

                if (IsCoveredByOwnerToBind(owner))
                    continue;

                if (!TryGetTransferredSemanticDiagnostics(owner, out var diagnostics))
                    return false;

                diagnosticsBuilder.AddRange(diagnostics);
            }

            foreach (var owner in ownersToBind)
            {
                var beforeCount = diagnosticsBuilder.Count;
                if (owner is GlobalStatementSyntax globalOwner)
                    BindPrecedingGlobalStatementsForScope(root, globalOwner);

                var ownerBinder = ReferenceEquals(owner, root)
                    ? rootBinder
                    : requireCompleteDeclarations
                        ? GetBinder(owner)
                        : GetBinderForIncrementalSemanticQuery(owner);
                Traverse(owner, ownerBinder);

                diagnosticsBuilder.AddRange(_binderCache.Values
                    .SelectMany(static binderState => binderState.Diagnostics.AsEnumerable())
                    .Where(diagnostic => BelongsToOwner(diagnostic, owner)));

                StoreSemanticDiagnosticDescriptors(
                    owner,
                    diagnosticsBuilder.Skip(beforeCount).ToImmutableArray());
            }

            DocumentationCommentValidator.Analyze(this, root, rootBinder.Diagnostics);
            diagnosticsBuilder.AddRange(rootBinder.Diagnostics.AsEnumerable()
                .Where(diagnostic => ReferenceEquals(diagnostic.Location.SourceTree, SyntaxTree) &&
                                     !allOwners.Any(owner => owner.Span.IntersectsWith(diagnostic.Location.SourceSpan))));
            return true;

            bool IsCoveredByOwnerToBind(SyntaxNode owner)
                => ownersToBind.Any(ownerToBind =>
                    !ReferenceEquals(ownerToBind, owner) &&
                    ownerToBind.Span.Start <= owner.Span.Start &&
                    ownerToBind.Span.End >= owner.Span.End);
        }

        Binder GetRootBinderForCompleteDiagnostics(SyntaxNode root)
        {
            EnsureRootBinderCreated();
            return GetBinder(root);
        }

        bool TryCollectTransferredDiagnosticsForAllOwners(
            SyntaxNode root,
            Binder rootBinder,
            SyntaxNode[] allOwners,
            ImmutableArray<Diagnostic>.Builder diagnosticsBuilder)
        {
            var transferredAny = false;
            foreach (var owner in allOwners)
            {
                if (TryGetTransferredSemanticDiagnostics(owner, out var diagnostics))
                {
                    transferredAny = true;
                    diagnosticsBuilder.AddRange(diagnostics);
                }
            }

            if (!transferredAny)
                return false;

            DocumentationCommentValidator.Analyze(this, root, rootBinder.Diagnostics);
            diagnosticsBuilder.AddRange(rootBinder.Diagnostics.AsEnumerable()
                .Where(diagnostic => ReferenceEquals(diagnostic.Location.SourceTree, SyntaxTree) &&
                                     !allOwners.Any(owner => owner.Span.IntersectsWith(diagnostic.Location.SourceSpan))));
            return true;
        }

        static SyntaxNode[] ExpandTopLevelOrderSensitiveOwners(SyntaxNode root, SyntaxNode[] ownersToBind)
        {
            var firstChangedGlobalStart = ownersToBind
                .OfType<GlobalStatementSyntax>()
                .Select(static owner => (int?)owner.Span.Start)
                .Min();

            if (firstChangedGlobalStart is not { } start)
                return ownersToBind;

            var expanded = ownersToBind.ToList();
            var seen = expanded
                .Select(static owner => new Compilation.ExecutableOwnerDescriptor(owner.Span, owner.Kind))
                .ToHashSet();

            foreach (var global in root.DescendantNodesAndSelf().OfType<GlobalStatementSyntax>())
            {
                if (global.Span.Start < start)
                    continue;

                if (seen.Add(new Compilation.ExecutableOwnerDescriptor(global.Span, global.Kind)))
                    expanded.Add(global);
            }

            return expanded.ToArray();
        }

        bool TryGetTransferredSemanticDiagnostics(
            SyntaxNode owner,
            out ImmutableArray<Diagnostic> diagnostics)
        {
            diagnostics = default;

            if (!Compilation.TryGetSemanticDiagnosticDescriptors(owner, out var descriptors))
                return false;

            var builder = ImmutableArray.CreateBuilder<Diagnostic>(descriptors.Length);
            foreach (var descriptor in descriptors)
            {
                var location = Location.Create(
                    owner.SyntaxTree,
                    new Text.TextSpan(owner.Span.Start + descriptor.RelativeStart, descriptor.Length));

                builder.Add(new Diagnostic(
                    descriptor.Descriptor,
                    location,
                    descriptor.MessageArgs.ToArray(),
                    descriptor.Severity,
                    descriptor.IsSuppressed,
                    descriptor.Properties));
            }

            diagnostics = builder.ToImmutable();
            return true;
        }

        void StoreSemanticDiagnosticDescriptors(
            SyntaxNode root,
            ImmutableArray<Diagnostic> diagnostics)
        {
            var byOwner = diagnostics
                .Where(diagnostic => ReferenceEquals(diagnostic.Location.SourceTree, SyntaxTree))
                .Select(diagnostic => (diagnostic, owner: TryFindDiagnosticOwner(root, diagnostic.Location.SourceSpan)))
                .Where(static item => item.owner is not null)
                .GroupBy(static item => item.owner!)
                .ToDictionary(static group => group.Key, static group => group.ToImmutableArray());

            foreach (var owner in GetExecutableOwnersForDiagnostics(root))
            {
                var descriptors = byOwner.TryGetValue(owner, out var ownerDiagnostics)
                    ? ownerDiagnostics.Select(item => new Compilation.SemanticDiagnosticDescriptor(
                        item.diagnostic.Descriptor,
                        item.diagnostic.Location.SourceSpan.Start - owner.Span.Start,
                        item.diagnostic.Location.SourceSpan.Length,
                        item.diagnostic.Severity,
                        item.diagnostic.IsSuppressed,
                        item.diagnostic.Properties,
                        item.diagnostic.GetMessageArgs().Cast<object?>().ToImmutableArray()))
                    .ToImmutableArray()
                    : ImmutableArray<Compilation.SemanticDiagnosticDescriptor>.Empty;

                Compilation.StoreSemanticDiagnosticDescriptors(owner, descriptors);
            }
        }

        static SyntaxNode? TryFindDiagnosticOwner(SyntaxNode root, Text.TextSpan span)
        {
            var node = root.FindNode(span, getInnermostNodeForTie: true);
            return node?.AncestorsAndSelf().FirstOrDefault(IsExecutableOwnerForDiagnostics);
        }

        static bool BelongsToOwner(Diagnostic diagnostic, SyntaxNode owner)
            => ReferenceEquals(diagnostic.Location.SourceTree, owner.SyntaxTree) &&
               owner.Span.IntersectsWith(diagnostic.Location.SourceSpan);

        static IEnumerable<SyntaxNode> GetExecutableOwnersForDiagnostics(SyntaxNode root)
            => root.DescendantNodesAndSelf().Where(IsExecutableOwnerForDiagnostics);

        static bool IsExecutableOwnerForDiagnostics(SyntaxNode node)
            => node is FunctionExpressionSyntax
                or BaseMethodDeclarationSyntax
                or BaseConstructorDeclarationSyntax
                or ParameterlessConstructorDeclarationSyntax
                or AccessorDeclarationSyntax
                or PropertyDeclarationSyntax
                or EventDeclarationSyntax
                or GlobalStatementSyntax;

        void BindStatementAttributeSyntaxes(SyntaxNode statementNode, Binder parentBinder)
        {
            foreach (var attributeSyntax in statementNode.DescendantNodes().OfType<AttributeSyntax>())
            {
                if (attributeSyntax.IsMacroAttribute())
                {
                    _ = GetMacroExpansion(attributeSyntax);
                    continue;
                }

                var attributeParent = (SyntaxNode?)attributeSyntax.Parent ?? statementNode;
                var binderForAttribute = GetBinder(attributeParent, parentBinder);
                var attributeBinder = binderForAttribute as AttributeBinder
                    ?? new AttributeBinder(binderForAttribute.ContainingSymbol, binderForAttribute);

                _ = attributeBinder.BindAttribute(attributeSyntax);
            }
        }
    }

    private static SyntaxNode? TryGetMacroTarget(AttributeSyntax attributeSyntax)
        => attributeSyntax.Parent?.Parent switch
        {
            AttributeListSyntax { Parent: SyntaxNode parent } => parent,
            SyntaxNode parent => parent,
            _ => null
        };

    /// <summary>
    /// Gets symbol information about a syntax node
    /// </summary>
    /// <param name="node">The syntax node</param>
    /// <param name="cancellationToken"></param>
    /// <returns>The symbol info</returns>
    public SymbolInfo GetSymbolInfo(SyntaxNode node, CancellationToken cancellationToken = default)
    {
        Compilation.PerformanceInstrumentation.SemanticQuery.RecordSymbolInfoQuery();

        if (TryGetPipeInvocationSymbolInfo(node, out var pipeInvocationInfo))
        {
            pipeInvocationInfo = ProjectBackingFieldSymbolsToAssociatedProperty(node, pipeInvocationInfo);
            _symbolMappings[node] = pipeInvocationInfo;
            return pipeInvocationInfo;
        }

        if (node is AttributeSyntax attributeSyntax &&
            BindAttribute(attributeSyntax)?.AttributeConstructor is { } attributeConstructor)
        {
            var attributeInfo = new SymbolInfo(attributeConstructor);
            _symbolMappings[node] = attributeInfo;
            StoreNodeInterestSymbolDescriptor(node, attributeConstructor);
            return attributeInfo;
        }

        Compilation.EnsureSourceDeclarationsComplete();

        if (node is IdentifierNameSyntax invokedMemberName &&
            invokedMemberName.Parent is MemberAccessExpressionSyntax invokedMemberAccess &&
            IsSameSyntaxNode(invokedMemberAccess.Name, invokedMemberName) &&
            invokedMemberAccess.Parent is InvocationExpressionSyntax invokedMemberInvocation &&
            IsSameSyntaxNode(invokedMemberInvocation.Expression, invokedMemberAccess))
        {
            if (ShouldUseDirectTypeMemberFastPath(invokedMemberAccess.Expression) &&
                TryResolveAvailableTypeExpression(invokedMemberAccess.Expression, out var invokedReceiverType) &&
                TryCreateAvailableInvocationSymbolInfo(
                    invokedReceiverType.GetMembers(invokedMemberName.Identifier.ValueText).OfType<IMethodSymbol>(),
                    invokedMemberInvocation,
                    out var directTypeMemberInfo))
            {
                directTypeMemberInfo = ProjectBackingFieldSymbolsToAssociatedProperty(node, directTypeMemberInfo);
                _symbolMappings[node] = directTypeMemberInfo;
                _symbolMappings[invokedMemberAccess] = directTypeMemberInfo;
                _symbolMappings[invokedMemberInvocation] = directTypeMemberInfo;
                return directTypeMemberInfo;
            }

            if (TryGetAvailableInvocationSymbolInfo(invokedMemberInvocation, out var availableInvocationInfo))
            {
                availableInvocationInfo = ProjectBackingFieldSymbolsToAssociatedProperty(node, availableInvocationInfo);
                _symbolMappings[node] = availableInvocationInfo;
                return availableInvocationInfo;
            }

            var invocationInfo = GetSymbolInfo(invokedMemberInvocation, cancellationToken);
            if (invocationInfo.Symbol is not null || !invocationInfo.CandidateSymbols.IsDefaultOrEmpty)
            {
                if (invocationInfo.Symbol is null &&
                    TryChoosePreferredCandidate(invocationInfo.CandidateSymbols, invokedMemberName.Identifier.ValueText, out var preferredCandidate))
                {
                    invocationInfo = new SymbolInfo(preferredCandidate);
                }

                invocationInfo = ProjectBackingFieldSymbolsToAssociatedProperty(node, invocationInfo);
                _symbolMappings[node] = invocationInfo;
                return invocationInfo;
            }
        }

        if (node is IdentifierNameSyntax pipelineIdentifier &&
            pipelineIdentifier.Parent is InvocationExpressionSyntax pipelineInvocation &&
            IsSameSyntaxNode(pipelineInvocation.Expression, pipelineIdentifier))
        {
            if (TryLookupPipelineInvocationSymbol(pipelineInvocation, pipelineIdentifier, pipelineIdentifier.Identifier.ValueText, out var directPipelineInfo))
            {
                directPipelineInfo = ProjectBackingFieldSymbolsToAssociatedProperty(node, directPipelineInfo);
                _symbolMappings[node] = directPipelineInfo;
                return directPipelineInfo;
            }
        }

        if (node is IdentifierNameSyntax earlyWithAssignmentIdentifier &&
            earlyWithAssignmentIdentifier.Parent is WithAssignmentSyntax earlyWithAssignment &&
            IsSameSyntaxNode(earlyWithAssignment.Name, earlyWithAssignmentIdentifier) &&
            TryGetWithAssignmentMemberSymbolInfo(earlyWithAssignment, out var earlyWithAssignmentInfo))
        {
            _symbolMappings[node] = earlyWithAssignmentInfo;
            return earlyWithAssignmentInfo;
        }

        if (node is IdentifierNameSyntax earlyArgumentName &&
            earlyArgumentName.Parent is NameColonSyntax earlyNameColon &&
            earlyNameColon.Parent is ArgumentSyntax earlyArgument &&
            IsSameSyntaxNode(earlyNameColon.Name, earlyArgumentName) &&
            TryGetArgumentParameterSymbolInfo(earlyArgument, out var earlyArgumentInfo))
        {
            _symbolMappings[node] = earlyArgumentInfo;
            return earlyArgumentInfo;
        }

        if (_symbolMappings.TryGetValue(node, out var symbolInfo))
        {
            if (symbolInfo.Symbol is not null || !symbolInfo.CandidateSymbols.IsDefaultOrEmpty)
            {
                symbolInfo = ProjectBackingFieldSymbolsToAssociatedProperty(node, symbolInfo);
                if (symbolInfo.Symbol is { } cachedSymbol)
                    StoreNodeInterestSymbolDescriptor(node, cachedSymbol);

                _symbolMappings[node] = symbolInfo;
                Compilation.PerformanceInstrumentation.SemanticQuery.RecordSymbolInfoCacheHit();
                return symbolInfo;
            }

            _symbolMappings.TryRemove(node, out _);
        }

        if (TryGetCachedNodeInterestSymbolInfo(node, out var cachedNodeInterestInfo))
        {
            cachedNodeInterestInfo = ProjectBackingFieldSymbolsToAssociatedProperty(node, cachedNodeInterestInfo);
            _symbolMappings[node] = cachedNodeInterestInfo;
            Compilation.PerformanceInstrumentation.SemanticQuery.RecordSymbolInfoCacheHit();
            return cachedNodeInterestInfo;
        }

        if (TryGetPatternConstantValueSymbolInfo(node, out var patternConstantValueInfo))
        {
            _symbolMappings[node] = patternConstantValueInfo;
            return patternConstantValueInfo;
        }

        if (TryGetAvailableInvocationSymbolInfo(node, out var availableInvocationSymbolInfo))
        {
            availableInvocationSymbolInfo = ProjectBackingFieldSymbolsToAssociatedProperty(node, availableInvocationSymbolInfo);
            _symbolMappings[node] = availableInvocationSymbolInfo;
            return availableInvocationSymbolInfo;
        }

        if (TryGetAvailableSymbolInfo(node, out var availableSymbolInfo) &&
            (availableSymbolInfo.Symbol is not null || !availableSymbolInfo.CandidateSymbols.IsDefaultOrEmpty))
        {
            if (availableSymbolInfo.Symbol is { } availableSymbol)
                StoreNodeInterestSymbolDescriptor(node, availableSymbol);

            _symbolMappings[node] = availableSymbolInfo;
            return availableSymbolInfo;
        }

        if (node is IdentifierNameSyntax earlyMemberIdentifier &&
            earlyMemberIdentifier.Parent is MemberAccessExpressionSyntax earlyMemberAccess &&
            IsSameSyntaxNode(earlyMemberAccess.Name, earlyMemberIdentifier) &&
            TryResolveMemberAccessFromVisibleReceiver(earlyMemberAccess, earlyMemberIdentifier, out var earlyMemberInfo))
        {
            earlyMemberInfo = ProjectBackingFieldSymbolsToAssociatedProperty(node, earlyMemberInfo);
            if (earlyMemberInfo.Symbol is { } earlyMemberSymbol)
                StoreNodeInterestSymbolDescriptor(node, earlyMemberSymbol);

            _symbolMappings[node] = earlyMemberInfo;
            return earlyMemberInfo;
        }

        if (TryGetNodeInterestSymbolInfo(node, out var nodeInterestInfo))
        {
            nodeInterestInfo = ProjectBackingFieldSymbolsToAssociatedProperty(node, nodeInterestInfo);
            _symbolMappings[node] = nodeInterestInfo;
            return nodeInterestInfo;
        }

        SymbolInfo info;

        if (node is SimpleNameSyntax invokedName &&
            invokedName.Parent is InvocationExpressionSyntax invokedCall &&
            IsSameSyntaxNode(invokedCall.Expression, invokedName))
        {
            if (TryBindInterestRegion(invokedCall, out var regionBoundInvocation))
            {
                var regionInfo = regionBoundInvocation.GetSymbolInfo();
                if (regionInfo.Symbol is not null || !regionInfo.CandidateSymbols.IsDefaultOrEmpty)
                {
                    info = regionInfo;
                }
                else
                {
                    var boundInvocation = GetBoundNode(invokedCall);
                    info = boundInvocation.GetSymbolInfo();
                }
            }
            else
            {
                var boundInvocation = GetBoundNode(invokedCall);
                var boundInvocationInfo = boundInvocation.GetSymbolInfo();

                if (boundInvocationInfo.Symbol is not null || !boundInvocationInfo.CandidateSymbols.IsDefaultOrEmpty)
                {
                    info = boundInvocationInfo;
                }
                else if (TryRebindInvocationAfterRefreshingFunctionArguments(invokedCall, out var reboundInvocationInfo))
                {
                    info = reboundInvocationInfo;
                }
                else if (TryLookupPipelineInvocationSymbol(invokedCall, invokedName, invokedName.Identifier.ValueText, out var pipelineInvocationInfo))
                {
                    info = pipelineInvocationInfo;
                }
                else if (TryBindExactSymbol(node, out var exactNameInfo))
                {
                    info = exactNameInfo;
                }
                else
                {
                    Compilation.PerformanceInstrumentation.SemanticQuery.RecordSymbolInfoBinderFallback();
                    var binder = GetBinder(node);
                    info = binder.BindSymbol(node);
                }
            }
        }
        else if (node is IdentifierNameSyntax identifier &&
            identifier.Parent is MemberAccessExpressionSyntax memberAccess &&
            IsSameSyntaxNode(memberAccess.Name, identifier))
        {
            if (TryResolveMemberAccessFromVisibleReceiver(memberAccess, identifier, out var fastMemberInfo))
            {
                info = fastMemberInfo;
                goto Complete;
            }

            if (TryBindExactSymbol(node, out var exactInfo))
            {
                info = exactInfo;
            }
            else if (TryBindExactSymbol(memberAccess, out var memberAccessInfo))
            {
                info = memberAccessInfo;
            }
            else if (memberAccess.Parent is InvocationExpressionSyntax invocation &&
                     IsSameSyntaxNode(invocation.Expression, memberAccess) &&
                     TryBindExactSymbol(invocation, out var invocationInfo))
            {
                info = invocationInfo;
            }
            else
            {
                if (memberAccess.Parent is InvocationExpressionSyntax invocationExpression &&
                    GetSymbolInfo(invocationExpression, cancellationToken) is var invocationSymbolInfo &&
                    (invocationSymbolInfo.Symbol is not null || !invocationSymbolInfo.CandidateSymbols.IsDefaultOrEmpty))
                {
                    info = invocationSymbolInfo;
                    goto Complete;
                }

                info = TryGetBoundNodeForSemanticQuery(memberAccess, out var boundMemberAccessNode) &&
                       boundMemberAccessNode is BoundExpression boundMemberAccess
                    ? boundMemberAccess.GetSymbolInfo()
                    : SymbolInfo.None;

                if (info.Symbol is null &&
                    info.CandidateSymbols.IsDefaultOrEmpty &&
                    memberAccess.Parent is InvocationExpressionSyntax operationInvocation &&
                    GetOperation(operationInvocation, cancellationToken) is IInvocationOperation invocationOperation &&
                    invocationOperation.TargetMethod is { } targetMethod)
                {
                    info = new SymbolInfo(targetMethod);
                }

                if (info.Symbol is null &&
                    info.CandidateSymbols.IsDefaultOrEmpty &&
                    memberAccess.Parent is InvocationExpressionSyntax refreshedInvocationSyntax &&
                    IsSameSyntaxNode(refreshedInvocationSyntax.Expression, memberAccess))
                {
                    ClearCachedSemanticState(memberAccess);
                    ClearCachedSemanticState(refreshedInvocationSyntax);

                    if (TryGetBoundNodeForSemanticQuery(refreshedInvocationSyntax, out var refreshedInvocationNode) &&
                        refreshedInvocationNode is BoundInvocationExpression refreshedInvocation)
                        info = new SymbolInfo(refreshedInvocation.Method);
                }

            }
        }
        else if (node is IdentifierNameSyntax receiverIdentifier &&
                 receiverIdentifier.Parent is MemberAccessExpressionSyntax receiverMemberAccess &&
                 IsSameSyntaxNode(receiverMemberAccess.Expression, receiverIdentifier))
        {
            if (TryBindExactSymbol(node, out var exactInfo))
            {
                info = exactInfo;
            }
            else
            {
                var semanticQueryBoundMemberAccess = TryGetBoundNodeForSemanticQuery(receiverMemberAccess, out var receiverMemberAccessNode) &&
                                                     receiverMemberAccessNode is BoundExpression receiverBoundExpression
                    ? receiverBoundExpression
                    : null;
                var receiverInfo = semanticQueryBoundMemberAccess switch
                {
                    BoundMemberAccessExpression memberAccessExpression => memberAccessExpression.Receiver.GetSymbolInfo(),
                    BoundExpression boundExpression => boundExpression.GetSymbolInfo(),
                    _ => SymbolInfo.None
                };

                if (receiverInfo.Symbol is not null || !receiverInfo.CandidateSymbols.IsDefaultOrEmpty)
                {
                    info = receiverInfo;
                }
                else if (semanticQueryBoundMemberAccess is not null &&
                         TryFindBoundNodeBySyntax(semanticQueryBoundMemberAccess, receiverIdentifier, out var boundReceiverNode))
                {
                    var resolvedFromChild = boundReceiverNode switch
                    {
                        BoundExpression boundExpression => boundExpression.GetSymbolInfo(),
                        BoundStatement boundStatement => boundStatement.GetSymbolInfo(),
                        _ => default
                    };
                    if (resolvedFromChild.Symbol is not null || !resolvedFromChild.CandidateSymbols.IsDefaultOrEmpty)
                    {
                        info = resolvedFromChild;
                    }
                    else
                    {
                        Compilation.PerformanceInstrumentation.SemanticQuery.RecordSymbolInfoBinderFallback();
                        var binder = GetBinderForIncrementalSemanticQuery(node);
                        info = binder.BindSymbol(node);
                    }
                }
                else
                {
                    Compilation.PerformanceInstrumentation.SemanticQuery.RecordSymbolInfoBinderFallback();
                    var binder = GetBinderForIncrementalSemanticQuery(node);
                    info = binder.BindSymbol(node);
                }
            }
        }
        else if (node is IdentifierNameSyntax memberBindingIdentifier &&
                 memberBindingIdentifier.Parent is MemberBindingExpressionSyntax memberBinding &&
                 IsSameSyntaxNode(memberBinding.Name, memberBindingIdentifier))
        {
            if (TryGetMemberBindingTargetMemberSymbolInfo(memberBinding, out var targetMemberInfo))
            {
                info = targetMemberInfo;
            }
            else if (TryBindExactSymbol(node, out var exactInfo))
            {
                info = exactInfo;
            }
            else if (TryBindExactSymbol(memberBinding, out var memberBindingInfo))
            {
                info = memberBindingInfo;
            }
            else
            {
                info = TryGetBoundNodeForSemanticQuery(memberBinding, out var boundMemberBindingNode) &&
                       boundMemberBindingNode is BoundExpression boundMemberBinding
                    ? boundMemberBinding.GetSymbolInfo()
                    : SymbolInfo.None;
            }
        }
        else if (node is IdentifierNameSyntax withAssignmentIdentifier &&
                 withAssignmentIdentifier.Parent is WithAssignmentSyntax withAssignment &&
                 IsSameSyntaxNode(withAssignment.Name, withAssignmentIdentifier) &&
                 TryGetWithAssignmentMemberSymbolInfo(withAssignment, out var withAssignmentInfo))
        {
            info = withAssignmentInfo;
        }
        else if (node is IdentifierNameSyntax argumentNameIdentifier &&
                 argumentNameIdentifier.Parent is NameColonSyntax nameColon &&
                 nameColon.Parent is ArgumentSyntax argument &&
                 IsSameSyntaxNode(nameColon.Name, argumentNameIdentifier) &&
                 TryGetArgumentParameterSymbolInfo(argument, out var argumentInfo))
        {
            info = argumentInfo;
        }
        else if (node is ExpressionSyntax expression)
        {
            if (IsExpressionWithoutDirectSymbol(expression))
            {
                info = SymbolInfo.None;
                goto Complete;
            }

            if (TryGetCachedBoundNode(expression) is BoundExpression cachedExpression &&
                !IsLikelyStaleFunctionBodyNode(cachedExpression))
            {
                var cachedInfo = cachedExpression.GetSymbolInfo();
                if (cachedInfo.Symbol is not null || !cachedInfo.CandidateSymbols.IsDefaultOrEmpty)
                {
                    info = cachedInfo;
                    goto Complete;
                }
            }

            if (expression is not InvocationExpressionSyntax &&
                expression is not InfixOperatorExpressionSyntax and not PrefixOperatorExpressionSyntax &&
                TryBindExactSymbol(expression, out var exactInfo))
            {
                info = exactInfo;
                goto Complete;
            }

            if (expression is InvocationExpressionSyntax earlyInvocationExpression &&
                TryResolveInvocationOperatorFromReceiver(earlyInvocationExpression, out var earlyInvocationOperatorInfo))
            {
                info = earlyInvocationOperatorInfo;
                goto Complete;
            }

            if (TryBindInterestRegion(expression, out var regionBoundExpression))
            {
                var regionInfo = regionBoundExpression.GetSymbolInfo();
                if (regionInfo.Symbol is not null || !regionInfo.CandidateSymbols.IsDefaultOrEmpty)
                {
                    info = regionInfo;
                    goto Complete;
                }
            }

            var boundExpression = TryGetBoundNodeForSemanticQuery(expression, out var boundExpressionNode) &&
                                  boundExpressionNode is BoundExpression semanticQueryBoundExpression
                ? semanticQueryBoundExpression
                : null;
            var boundInfo = boundExpression?.GetSymbolInfo() ?? SymbolInfo.None;
            if (boundInfo.Symbol is not null || !boundInfo.CandidateSymbols.IsDefaultOrEmpty)
            {
                info = boundInfo;
                goto Complete;
            }

            if (expression is InvocationExpressionSyntax &&
                GetBoundNode(expression) is BoundExpression fullyBoundInvocation)
            {
                var fullyBoundInfo = fullyBoundInvocation.GetSymbolInfo();
                if (fullyBoundInfo.Symbol is not null || !fullyBoundInfo.CandidateSymbols.IsDefaultOrEmpty)
                {
                    Compilation.PerformanceInstrumentation.SemanticQuery.RecordSymbolInfoBinderFallback();
                    info = fullyBoundInfo;
                    goto Complete;
                }
            }

            if (expression is InvocationExpressionSyntax)
            {
                EnsureDiagnosticBindingCompleted();
                if (TryGetCachedBoundNode(expression) is BoundExpression diagnosticsBoundInvocation)
                {
                    var diagnosticsBoundInfo = diagnosticsBoundInvocation.GetSymbolInfo();
                    if (diagnosticsBoundInfo.Symbol is not null || !diagnosticsBoundInfo.CandidateSymbols.IsDefaultOrEmpty)
                    {
                        info = diagnosticsBoundInfo;
                        goto Complete;
                    }
                }
            }

            if (TryRebindExpressionFromEnclosingFunctionContext(expression, out var functionContextInfo))
            {
                info = functionContextInfo;
                goto Complete;
            }

            if (TryResolveInterestLocalSymbol(expression) is { } interestLocalSymbol)
            {
                info = new SymbolInfo(interestLocalSymbol);
                goto Complete;
            }

            var binder = GetBinderForIncrementalSemanticQuery(expression);
            var binderInfo = binder.BindSymbol(expression);
            if (binderInfo.Symbol is not null || !binderInfo.CandidateSymbols.IsDefaultOrEmpty)
            {
                Compilation.PerformanceInstrumentation.SemanticQuery.RecordSymbolInfoBinderFallback();
                info = binderInfo;
                goto Complete;
            }

            Compilation.PerformanceInstrumentation.SemanticQuery.RecordSymbolInfoOperationFallback();
            var operation = GetOperation(expression, cancellationToken);
            var operationSymbol = operation switch
            {
                IFieldReferenceOperation fieldReference => fieldReference.Field,
                IPropertyReferenceOperation propertyReference => propertyReference.Property,
                IMethodReferenceOperation methodReference => methodReference.Method,
                IMemberReferenceOperation memberReference => memberReference.Symbol,
                IInvocationOperation invocation => invocation.TargetMethod,
                IParameterReferenceOperation parameterReference => parameterReference.Parameter,
                ILocalReferenceOperation localReference => localReference.Local,
                IVariableReferenceOperation variableReference => variableReference.Variable,
                _ => null
            };

            if (operationSymbol is not null)
            {
                info = new SymbolInfo(operationSymbol);
            }
            else
            {
                info = boundExpression?.GetSymbolInfo() ?? SymbolInfo.None;

            }
        }
        else if (node is StatementSyntax statement)
        {
            EnsureDiagnosticBindingCompleted();
            info = TryGetBoundNodeForSemanticQuery(statement, out var boundStatementNode) &&
                   boundStatementNode is BoundStatement boundStatement
                ? boundStatement.GetSymbolInfo()
                : SymbolInfo.None;
        }
        else
        {
            Compilation.PerformanceInstrumentation.SemanticQuery.RecordSymbolInfoBinderFallback();
            var binder = GetBinderForIncrementalSemanticQuery(node);
            info = binder.BindSymbol(node);
        }

        if (info.Symbol is null &&
            info.CandidateSymbols.IsDefaultOrEmpty &&
            node is SimpleNameSyntax pipelineName &&
            pipelineName.Parent is InvocationExpressionSyntax fallbackPipelineInvocation &&
            IsSameSyntaxNode(fallbackPipelineInvocation.Expression, pipelineName))
        {
            if (TryLookupPipelineInvocationSymbol(fallbackPipelineInvocation, pipelineName, pipelineName.Identifier.ValueText, out var pipelineInfo))
            {
                info = pipelineInfo;
            }
        }

    Complete:
        info = ProjectBackingFieldSymbolsToAssociatedProperty(node, info);
        if (info.Symbol is { } resolvedSymbol)
            StoreNodeInterestSymbolDescriptor(node, resolvedSymbol);

        _symbolMappings[node] = info;
        return info;
    }

    private static bool IsExpressionWithoutDirectSymbol(ExpressionSyntax expression)
        => expression is PrefixOperatorExpressionSyntax { Kind: SyntaxKind.AwaitExpression };

    private static bool TryChoosePreferredCandidate(
        ImmutableArray<ISymbol> candidates,
        string name,
        [System.Diagnostics.CodeAnalysis.NotNullWhen(true)] out ISymbol? symbol)
    {
        symbol = null;

        if (candidates.IsDefaultOrEmpty)
            return false;

        symbol = candidates.FirstOrDefault(candidate => string.Equals(candidate.Name, name, StringComparison.Ordinal))
                 ?? candidates[0];
        return symbol is not null;
    }

    private bool TryGetPipeInvocationSymbolInfo(SyntaxNode node, out SymbolInfo info)
    {
        info = SymbolInfo.None;

        var invocation = TryGetPipeInvocationForSymbolNode(node);
        if (invocation is null)
            return false;

        if (invocation.Parent is not InfixOperatorExpressionSyntax
            {
                OperatorToken.Kind: SyntaxKind.PipeToken
            } pipeExpression ||
            !IsSameSyntaxNode(pipeExpression.Right, invocation))
        {
            return false;
        }

        var boundPipe = TryGetCachedBoundNode(pipeExpression) as BoundInvocationExpression
            ?? GetBoundNode(pipeExpression) as BoundInvocationExpression;
        if (boundPipe is null)
            return false;

        info = boundPipe.GetSymbolInfo();
        return info.Symbol is not null || !info.CandidateSymbols.IsDefaultOrEmpty;
    }

    private static InvocationExpressionSyntax? TryGetPipeInvocationForSymbolNode(SyntaxNode node)
        => node switch
        {
            InvocationExpressionSyntax invocation => invocation,
            SimpleNameSyntax simpleName
                when simpleName.Parent is InvocationExpressionSyntax invocation &&
                     IsSameSyntaxNode(invocation.Expression, simpleName) => invocation,
            SimpleNameSyntax simpleName
                when simpleName.Parent is MemberAccessExpressionSyntax memberAccess &&
                     IsSameSyntaxNode(memberAccess.Name, simpleName) &&
                     memberAccess.Parent is InvocationExpressionSyntax invocation &&
                     IsSameSyntaxNode(invocation.Expression, memberAccess) => invocation,
            MemberAccessExpressionSyntax memberAccess
                when memberAccess.Parent is InvocationExpressionSyntax invocation &&
                     IsSameSyntaxNode(invocation.Expression, memberAccess) => invocation,
            _ => null
        };

    private bool TryGetMemberBindingTargetMemberSymbolInfo(MemberBindingExpressionSyntax memberBinding, out SymbolInfo info)
    {
        info = SymbolInfo.None;

        var memberName = memberBinding.Name.Identifier.ValueText;
        if (string.IsNullOrWhiteSpace(memberName))
            return false;

        if (!TryGetTargetTypeForExpression(memberBinding, out var targetType) ||
            targetType is null ||
            targetType.TypeKind == TypeKind.Error)
        {
            return false;
        }

        var members = targetType
            .GetMembers(memberName)
            .Where(static member => member is IPropertySymbol or IFieldSymbol or IMethodSymbol)
            .ToImmutableArray<ISymbol>();
        if (members.IsDefaultOrEmpty)
            return false;

        var preferred = members.OfType<IPropertySymbol>().FirstOrDefault()
            ?? members.OfType<IFieldSymbol>().FirstOrDefault() as ISymbol
            ?? members.OfType<IMethodSymbol>().FirstOrDefault()
            ?? members[0];

        info = members.Length == 1
            ? new SymbolInfo(preferred)
            : new SymbolInfo(preferred, members, CandidateReason.MemberGroup);
        return true;
    }

    private bool TryGetTargetTypeForExpression(ExpressionSyntax expression, out ITypeSymbol? targetType)
    {
        targetType = null;

        if (expression.Parent is EqualsValueClauseSyntax { Parent: VariableDeclaratorSyntax variableDeclarator } &&
            variableDeclarator.TypeAnnotation?.Type is { } annotationType)
        {
            targetType = GetTypeInfo(annotationType).Type;
            return targetType is not null && targetType.TypeKind != TypeKind.Error;
        }

        if (expression.Parent is WithAssignmentSyntax withAssignment &&
            TryGetWithAssignmentMemberSymbolInfo(withAssignment, out var withAssignmentInfo))
        {
            targetType = withAssignmentInfo.Symbol switch
            {
                IPropertySymbol property => property.Type,
                IFieldSymbol field => field.Type,
                _ => null
            };

            return targetType is not null && targetType.TypeKind != TypeKind.Error;
        }

        return false;
    }

    private bool TryGetWithAssignmentMemberSymbolInfo(WithAssignmentSyntax assignment, out SymbolInfo info)
    {
        info = SymbolInfo.None;

        var withExpression = assignment.Parent as WithExpressionSyntax
            ?? assignment.Ancestors().OfType<WithExpressionSyntax>().FirstOrDefault();
        if (withExpression is null)
            return false;

        if (!TryGetWithReceiverType(withExpression.Expression, out var receiverType) ||
            receiverType is null ||
            receiverType.TypeKind == TypeKind.Error)
        {
            return false;
        }

        var memberName = assignment.Name.Identifier.ValueText;
        if (string.IsNullOrWhiteSpace(memberName))
            return false;

        var members = receiverType
            .GetMembers(memberName)
            .Where(static member => member is IPropertySymbol or IFieldSymbol)
            .Where(static member => !member.IsStatic)
            .ToImmutableArray<ISymbol>();
        if (members.IsDefaultOrEmpty)
            return false;

        var preferred = members.OfType<IPropertySymbol>().FirstOrDefault()
            ?? members.OfType<IFieldSymbol>().FirstOrDefault() as ISymbol
            ?? members[0];

        info = members.Length == 1
            ? new SymbolInfo(preferred)
            : new SymbolInfo(preferred, members, CandidateReason.MemberGroup);
        return true;
    }

    private bool TryGetWithReceiverType(ExpressionSyntax receiver, out INamedTypeSymbol? receiverType)
    {
        receiverType = null;

        if (receiver is TypeSyntax typeSyntax)
        {
            receiverType = GetTypeInfo(typeSyntax).Type as INamedTypeSymbol;
            if (receiverType is not null && receiverType.TypeKind != TypeKind.Error)
                return true;
        }

        if (GetTypeInfo(receiver).Type is INamedTypeSymbol expressionType &&
            expressionType.TypeKind != TypeKind.Error)
        {
            receiverType = expressionType;
            return true;
        }

        var symbol = GetSymbolInfo(receiver).Symbol;
        receiverType = symbol switch
        {
            INamedTypeSymbol namedType => namedType,
            ILocalSymbol { Type: INamedTypeSymbol localType } => localType,
            IParameterSymbol { Type: INamedTypeSymbol parameterType } => parameterType,
            IPropertySymbol { Type: INamedTypeSymbol propertyType } => propertyType,
            IFieldSymbol { Type: INamedTypeSymbol fieldType } => fieldType,
            _ => null
        };

        if (receiverType is not null && receiverType.TypeKind != TypeKind.Error)
            return true;

        if (TryGetCachedBoundNode(receiver) is BoundTypeExpression { Type: INamedTypeSymbol boundType })
        {
            receiverType = boundType;
            return receiverType.TypeKind != TypeKind.Error;
        }

        if (TryGetCachedBoundNode(receiver) is BoundObjectCreationExpression { Type: INamedTypeSymbol createdType })
        {
            receiverType = createdType;
            return receiverType.TypeKind != TypeKind.Error;
        }

        return false;
    }

    public bool TryGetSymbolInfo(SyntaxNode node, out SymbolInfo info, CancellationToken cancellationToken = default)
    {
        if (TryGetAvailableSymbolInfo(node, out info))
            return true;

        info = GetSymbolInfo(node, cancellationToken);
        return info.Symbol is not null || !info.CandidateSymbols.IsDefaultOrEmpty;
    }

    internal bool TryGetCachedSymbolInfo(SyntaxNode node, out SymbolInfo info)
    {
        if (_symbolMappings.TryGetValue(node, out info) && HasSymbolInfo(info))
        {
            Compilation.PerformanceInstrumentation.SemanticQuery.RecordSymbolInfoCacheHit();
            return true;
        }

        if (TryGetCachedBoundSymbolInfo(node, out info))
            return true;

        if (node is IdentifierNameSyntax identifier)
        {
            if (identifier.Parent is InvocationExpressionSyntax invocation &&
                IsSameSyntaxNode(invocation.Expression, identifier))
            {
                if (TryGetCachedSymbolInfo(invocation, out info))
                {
                    _symbolMappings[node] = info;
                    return true;
                }
            }

            if (identifier.Parent is MemberAccessExpressionSyntax memberAccess)
            {
                if (IsSameSyntaxNode(memberAccess.Name, identifier))
                {
                    if (memberAccess.Parent is InvocationExpressionSyntax memberInvocation &&
                        IsSameSyntaxNode(memberInvocation.Expression, memberAccess) &&
                        TryGetCachedSymbolInfo(memberInvocation, out info))
                    {
                        _symbolMappings[node] = info;
                        return true;
                    }

                    if (TryGetCachedSymbolInfo(memberAccess, out info))
                    {
                        _symbolMappings[node] = info;
                        return true;
                    }
                }
                else if (IsSameSyntaxNode(memberAccess.Expression, identifier) &&
                         TryGetCachedBoundNode(memberAccess) is BoundMemberAccessExpression boundMemberAccess)
                {
                    info = boundMemberAccess.Receiver.GetSymbolInfo();
                    if (HasSymbolInfo(info))
                    {
                        info = ProjectBackingFieldSymbolsToAssociatedProperty(node, info);
                        _symbolMappings[node] = info;
                        return true;
                    }
                }
            }

            if (identifier.Parent is MemberBindingExpressionSyntax memberBinding &&
                IsSameSyntaxNode(memberBinding.Name, identifier) &&
                TryGetCachedSymbolInfo(memberBinding, out info))
            {
                _symbolMappings[node] = info;
                return true;
            }
        }

        info = default;
        return false;
    }

    /// <summary>
    /// Tries to retrieve symbol information from semantic state that is already available.
    /// This method does not bind cold bodies, create operations, or run diagnostics.
    /// </summary>
    internal bool TryGetAvailableSymbolInfo(SyntaxNode node, out SymbolInfo info)
    {
        if (TryGetCachedSymbolInfo(node, out info))
            return true;

        if (node is IdentifierNameSyntax identifier &&
            TryLookupVisibleValueSymbol(identifier) is { } visibleSymbol)
        {
            info = new SymbolInfo(visibleSymbol);
            _symbolMappings[node] = info;
            StoreNodeInterestSymbolDescriptor(node, visibleSymbol);
            return true;
        }

        if (node is IdentifierNameSyntax syntaxScopedIdentifier &&
            TryResolveAvailableLocalReferenceFromSyntax(syntaxScopedIdentifier, out var syntaxScopedLocal))
        {
            info = new SymbolInfo(syntaxScopedLocal);
            _symbolMappings[node] = info;
            StoreNodeInterestSymbolDescriptor(node, syntaxScopedLocal);
            return true;
        }

        if (node is IdentifierNameSyntax aliasIdentifier &&
            TryLookupAvailableAlias(aliasIdentifier, out var aliasSymbol) &&
            aliasSymbol is not null)
        {
            info = new SymbolInfo(aliasSymbol);
            _symbolMappings[node] = info;
            StoreNodeInterestSymbolDescriptor(node, aliasSymbol);
            return true;
        }

        if (node is IdentifierNameSyntax namedTypeIdentifier &&
            IsAvailableNamedTypeLookupContext(namedTypeIdentifier) &&
            TryLookupAvailableNamedType(namedTypeIdentifier.Identifier.ValueText, 0, out var namedType) &&
            namedType is not null)
        {
            info = new SymbolInfo(namedType);
            _symbolMappings[node] = info;
            StoreNodeInterestSymbolDescriptor(node, namedType);
            return true;
        }

        if (TryGetAvailableMemberAccessSymbolInfo(node, out info))
            return true;

        if (TryGetCachedNodeInterestSymbolInfo(node, out info))
            return true;

        if (TryGetDeclarationSymbolInfo(node, out info))
            return true;

        info = default;
        return false;
    }

    private bool TryLookupAvailableAlias(IdentifierNameSyntax identifier, out IAliasSymbol? alias)
    {
        alias = null;

        if (!IsAvailableNamedTypeLookupContext(identifier))
            return false;

        var name = identifier.Identifier.ValueText;
        if (string.IsNullOrWhiteSpace(name))
            return false;

        for (var binder = GetBinderForIncrementalSemanticQuery(identifier); binder is not null; binder = binder.ParentBinder)
        {
            if (binder is not ImportBinder importBinder)
                continue;

            if (importBinder.GetAliases().TryGetValue(name, out var aliases))
            {
                alias = aliases.FirstOrDefault();
                return alias is not null;
            }
        }

        return false;
    }

    private static bool IsAvailableNamedTypeLookupContext(IdentifierNameSyntax identifier)
    {
        if (!identifier.AncestorsAndSelf().OfType<TypeSyntax>().Any() &&
            !IsLikelyTypeIdentifier(identifier.Identifier.ValueText))
        {
            return false;
        }

        if (identifier.AncestorsAndSelf().OfType<TypeSyntax>().Any())
            return true;

        if (identifier.Parent is InvocationExpressionSyntax invocation &&
            IsSameSyntaxNode(invocation.Expression, identifier))
        {
            return true;
        }

        return identifier.Parent is MemberAccessExpressionSyntax memberAccess &&
               IsSameSyntaxNode(memberAccess.Expression, identifier);
    }

    private static bool IsLikelyTypeIdentifier(string name)
        => !string.IsNullOrEmpty(name) && char.IsUpper(name[0]);

    private static bool IsLikelyTypeExpression(ExpressionSyntax expression)
        => expression switch
        {
            SimpleNameSyntax simpleName => IsLikelyTypeIdentifier(simpleName.Identifier.ValueText),
            MemberAccessExpressionSyntax memberAccess => IsLikelyTypeExpression(memberAccess.Expression),
            _ => false
        };

    private bool ShouldUseDirectTypeMemberFastPath(ExpressionSyntax expression)
    {
        if (!IsLikelyTypeExpression(expression))
            return false;

        if (!TryGetAvailableSymbolInfo(expression, out var receiverInfo))
            return true;

        if (GetNamedTypeFromAvailableSymbol(receiverInfo.Symbol) is not null)
            return true;

        return receiverInfo.CandidateSymbols
            .Select(GetNamedTypeFromAvailableSymbol)
            .Any(static type => type is not null);
    }

    private bool TryResolveAvailableLocalReferenceFromSyntax(
        IdentifierNameSyntax identifier,
        [System.Diagnostics.CodeAnalysis.NotNullWhen(true)] out ILocalSymbol? localSymbol)
    {
        localSymbol = null;

        var name = identifier.Identifier.ValueText;
        if (string.IsNullOrWhiteSpace(name))
            return false;

        foreach (var scope in identifier.Ancestors().OfType<BlockSyntax>())
        {
            foreach (var declarator in DescendantNodesExcludingNestedScopes(scope).OfType<VariableDeclaratorSyntax>())
            {
                if (declarator.Span.Start >= identifier.Span.Start ||
                    !string.Equals(declarator.Identifier.ValueText, name, StringComparison.Ordinal))
                {
                    continue;
                }

                if (TryGetStableLocalDeclarationSymbol(declarator, out localSymbol) ||
                    TryResolveAvailableLocalSymbol(declarator, out localSymbol))
                    return true;
            }
        }

        return false;
    }

    private bool TryGetAvailableInvocationSymbolInfo(SyntaxNode node, out SymbolInfo info)
    {
        info = SymbolInfo.None;

        var invocation = TryGetInvocationForSymbolInfoNode(node);
        if (invocation is null)
            return false;

        if (!TryGetAvailableInvocationCandidates(invocation, out var methods) ||
            methods.IsDefaultOrEmpty)
        {
            return false;
        }

        if (methods.Any(static method => method.TypeParameters.Length > 0 && HasUnresolvedMethodTypeParameters(method)))
        {
            if (TryGetInvocationGenericName(invocation.Expression) is { } genericName)
            {
                var typeArguments = ResolveAvailableTypeArguments(genericName.TypeArgumentList);
                if (typeArguments.IsDefaultOrEmpty ||
                    typeArguments.Length != genericName.TypeArgumentList.Arguments.Count)
                {
                    return false;
                }

                methods = methods
                    .Select(method => method.TypeParameters.Length == typeArguments.Length
                        ? method.Construct(typeArguments.ToArray())
                        : method)
                    .ToImmutableArray();
            }
            else
            {
                return false;
            }
        }

        var candidates = methods.Cast<ISymbol>().ToImmutableArray();
        var preferred = methods.Length == 1
            ? methods[0]
            : TryChooseInvocationMethodCandidate(methods, invocation, InvocationCandidateFallback.None);

        if (preferred is null)
            return false;

        info = new SymbolInfo(preferred, candidates);
        CacheAvailableInvocationSymbolInfo(invocation, info);
        return true;
    }

    private static bool HasUnresolvedMethodTypeParameters(IMethodSymbol method)
    {
        if (method.TypeParameters.IsDefaultOrEmpty)
            return false;

        if (ContainsTypeParameter(method.ReturnType))
            return true;

        foreach (var parameter in method.Parameters)
        {
            if (ContainsTypeParameter(parameter.Type))
                return true;
        }

        return false;
    }

    private static GenericNameSyntax? TryGetInvocationGenericName(ExpressionSyntax expression)
        => expression switch
        {
            GenericNameSyntax genericName => genericName,
            MemberAccessExpressionSyntax { Name: GenericNameSyntax genericName } => genericName,
            _ => null
        };

    private static bool TryCreateAvailableInvocationSymbolInfo(
        IEnumerable<IMethodSymbol> methods,
        InvocationExpressionSyntax invocation,
        out SymbolInfo info)
    {
        var candidateMethods = methods.ToImmutableArray();
        if (candidateMethods.IsDefaultOrEmpty ||
            candidateMethods.Any(static method => method.TypeParameters.Length > 0))
        {
            info = default;
            return false;
        }

        var candidates = candidateMethods.Cast<ISymbol>().ToImmutableArray();
        var preferred = candidateMethods.Length == 1
            ? candidateMethods[0]
            : TryChooseInvocationMethodCandidate(candidateMethods, invocation, InvocationCandidateFallback.FirstCandidate);

        info = new SymbolInfo(preferred, candidates);
        return true;
    }

    internal enum InvocationCandidateFallback
    {
        None,
        FirstCandidate,
        FirstCompatibleOrSecondCandidateWhenArgumentsPresent
    }

    internal static IMethodSymbol? TryChooseInvocationMethodCandidate(
        IEnumerable<IMethodSymbol> methods,
        InvocationExpressionSyntax invocation,
        InvocationCandidateFallback fallback)
    {
        var candidates = methods.ToImmutableArray();
        var argumentCount = invocation.ArgumentList.Arguments.Count +
            (invocation.TrailingBlock is null ? 0 : 1);
        IMethodSymbol? selected = null;

        foreach (var method in candidates)
        {
            if (!TryGetFastParameterCount(method, out var total))
                continue;

            var required = total;
            var hasParams = method is not PEMethodSymbol && method.Parameters.Any(static parameter => parameter.IsVarParams);

            if (argumentCount < required || (!hasParams && argumentCount > total))
                continue;

            if (selected is not null)
            {
                return fallback == InvocationCandidateFallback.None
                    ? null
                    : selected;
            }

            selected = method;
        }

        if (selected is not null)
            return selected;

        return fallback switch
        {
            InvocationCandidateFallback.FirstCandidate => candidates.FirstOrDefault(),
            InvocationCandidateFallback.FirstCompatibleOrSecondCandidateWhenArgumentsPresent
                when argumentCount > 0 && candidates.Length > 1 => candidates[1],
            _ => null
        };
    }

    private static bool TryGetFastParameterCount(IMethodSymbol method, out int count)
    {
        if (method is PEMethodSymbol peMethod)
            return peMethod.TryGetCachedParameterCount(out count);

        count = method.Parameters.Length;
        return true;
    }

    private static InvocationExpressionSyntax? TryGetInvocationForSymbolInfoNode(SyntaxNode node)
        => node switch
        {
            InvocationExpressionSyntax invocation => invocation,
            SimpleNameSyntax simpleName
                when simpleName.Parent is InvocationExpressionSyntax invocation &&
                     IsSameSyntaxNode(invocation.Expression, simpleName) => invocation,
            SimpleNameSyntax simpleName
                when simpleName.Parent is MemberAccessExpressionSyntax memberAccess &&
                     IsSameSyntaxNode(memberAccess.Name, simpleName) &&
                     memberAccess.Parent is InvocationExpressionSyntax invocation &&
                     IsSameSyntaxNode(invocation.Expression, memberAccess) => invocation,
            MemberAccessExpressionSyntax memberAccess
                when memberAccess.Parent is InvocationExpressionSyntax invocation &&
                     IsSameSyntaxNode(invocation.Expression, memberAccess) => invocation,
            _ => null
        };

    private void CacheAvailableInvocationSymbolInfo(InvocationExpressionSyntax invocation, SymbolInfo info)
    {
        _symbolMappings[invocation] = info;
        _symbolMappings[invocation.Expression] = info;

        switch (invocation.Expression)
        {
            case SimpleNameSyntax simpleName:
                _symbolMappings[simpleName] = info;
                break;
            case MemberAccessExpressionSyntax { Name: SimpleNameSyntax memberName }:
                _symbolMappings[memberName] = info;
                break;
            case MemberBindingExpressionSyntax { Name: SimpleNameSyntax memberName }:
                _symbolMappings[memberName] = info;
                break;
        }
    }

    /// <summary>
    /// Tries to retrieve invocation candidates from semantic state and declarations that are already available.
    /// This method does not bind cold bodies, create operations, or run diagnostics.
    /// </summary>
    internal bool TryGetAvailableInvocationCandidates(InvocationExpressionSyntax invocation, out ImmutableArray<IMethodSymbol> methods)
    {
        var builder = ImmutableArray.CreateBuilder<IMethodSymbol>();

        void AddIfNotPresent(IMethodSymbol? method)
        {
            if (method is null)
                return;

            foreach (var existing in builder)
            {
                if (SymbolEqualityComparer.Default.Equals(existing, method))
                    return;
            }

            builder.Add(method);
        }

        void AddSymbolInfoCandidates(SymbolInfo symbolInfo)
        {
            if (symbolInfo.Symbol is IMethodSymbol method)
                AddIfNotPresent(method);

            if (symbolInfo.Symbol is INamedTypeSymbol type)
                AddAvailableConstructors(type, AddIfNotPresent);

            if (!symbolInfo.CandidateSymbols.IsDefaultOrEmpty)
            {
                foreach (var candidateMethod in symbolInfo.CandidateSymbols.OfType<IMethodSymbol>())
                    AddIfNotPresent(candidateMethod);

                foreach (var candidateType in symbolInfo.CandidateSymbols.OfType<INamedTypeSymbol>())
                    AddAvailableConstructors(candidateType, AddIfNotPresent);
            }
        }

        if (TryGetCachedSymbolInfo(invocation, out var cachedInvocationInfo))
        {
            AddSymbolInfoCandidates(cachedInvocationInfo);
            methods = builder.ToImmutable();
            if (methods.Length > 0)
                return true;
        }

        if (invocation.Expression is MemberAccessExpressionSyntax { Name: SimpleNameSyntax memberName } memberAccess &&
            TryResolveAvailableTypeExpression(memberAccess.Expression, out var typeExpressionType) &&
            typeExpressionType is not null)
        {
            foreach (var method in typeExpressionType.GetMembers(memberName.Identifier.ValueText).OfType<IMethodSymbol>())
                AddIfNotPresent(method);
        }

        if (invocation.Expression is MemberAccessExpressionSyntax { Name: SimpleNameSyntax availableMemberName } availableMemberAccess &&
            TryGetAvailableSymbolInfo(availableMemberAccess.Expression, out var availableReceiverInfo))
        {
            var availableReceiverType = GetNamedTypeFromAvailableSymbol(availableReceiverInfo.Symbol)
                ?? availableReceiverInfo.CandidateSymbols.Select(GetNamedTypeFromAvailableSymbol).FirstOrDefault(static type => type is not null);
            if (availableReceiverType is not null)
            {
                foreach (var method in availableReceiverType.GetMembers(availableMemberName.Identifier.ValueText).OfType<IMethodSymbol>())
                    AddIfNotPresent(method);
            }
        }

        if (invocation.Expression is MemberAccessExpressionSyntax { Name: SimpleNameSyntax receiverMemberName } receiverMemberAccess &&
            TryGetAvailableTypeInfo(receiverMemberAccess.Expression, out var receiverTypeInfo) &&
            (receiverTypeInfo.Type ?? receiverTypeInfo.ConvertedType)?.GetPlainType() is INamedTypeSymbol receiverType)
        {
            foreach (var method in receiverType.GetMembers(receiverMemberName.Identifier.ValueText).OfType<IMethodSymbol>())
                AddIfNotPresent(method);
        }

        methods = builder.ToImmutable();
        if (methods.Length > 0)
            return true;

        if (TryGetAvailableExtensionInvocationCandidates(invocation, out var extensionCandidates))
        {
            foreach (var extensionCandidate in extensionCandidates)
                AddIfNotPresent(extensionCandidate);

            methods = builder.ToImmutable();
            return methods.Length > 0;
        }

        if (TryGetAvailablePipeInvocationCandidates(invocation, out var pipeCandidates))
        {
            foreach (var pipeCandidate in pipeCandidates)
                AddIfNotPresent(pipeCandidate);

            methods = builder.ToImmutable();
            return methods.Length > 0;
        }

        if (TryGetAvailableSymbolInfo(invocation, out var invocationInfo))
            AddSymbolInfoCandidates(invocationInfo);

        if (TryGetAvailableSymbolInfo(invocation.Expression, out var expressionInfo))
            AddSymbolInfoCandidates(expressionInfo);

        var invocationName = invocation.Expression switch
        {
            IdentifierNameSyntax identifier => identifier.Identifier.ValueText,
            GenericNameSyntax genericName => genericName.Identifier.ValueText,
            _ => null
        };

        if (invocation.Expression is GenericNameSyntax &&
            !string.IsNullOrWhiteSpace(invocationName))
        {
            var containingType = GetBinder(invocation).ContainingSymbol switch
            {
                INamedTypeSymbol type => type,
                IMethodSymbol method => method.ContainingType,
                _ => null
            };

            if (containingType is not null)
            {
                foreach (var method in containingType.GetMembers(invocationName).OfType<IMethodSymbol>())
                    AddIfNotPresent(method);
            }
        }

        if (invocation.Expression is IdentifierNameSyntax invocationIdentifier &&
            TryLookupAvailableFunctionDeclarations(invocationIdentifier, invocationIdentifier.Identifier.ValueText, out var availableFunctions))
        {
            foreach (var function in availableFunctions)
                AddIfNotPresent(function);
        }

        if (TryResolveAvailableCallableExpression(invocation.Expression, out var callableSymbol))
        {
            switch (callableSymbol)
            {
                case IMethodSymbol method:
                    AddIfNotPresent(method);
                    break;
                case INamedTypeSymbol type:
                    AddAvailableConstructors(type, AddIfNotPresent);
                    break;
            }
        }

        if (builder.Count == 0 &&
            TryGetAvailableTypeInfo(invocation.Expression, out var typeInfo) &&
            (typeInfo.Type ?? typeInfo.ConvertedType)?.GetPlainType() is INamedTypeSymbol expressionType)
        {
            if (expressionType.GetDelegateInvokeMethod() is { } invokeMethod)
                AddIfNotPresent(invokeMethod);

            AddInvokeCandidatesFromType(expressionType, AddIfNotPresent);
            AddAvailableConstructors(expressionType, AddIfNotPresent);
        }

        methods = builder.ToImmutable();
        return methods.Length > 0;
    }

    internal bool TryGetAvailableExtensionInvocationCandidates(InvocationExpressionSyntax invocation, out ImmutableArray<IMethodSymbol> methods)
    {
        methods = ImmutableArray<IMethodSymbol>.Empty;

        if (invocation.Expression is not MemberAccessExpressionSyntax { Name: SimpleNameSyntax memberName } memberAccess)
            return false;

        var binder = GetBinder(invocation);
        var extensionName = memberName.Identifier.ValueText;
        var receiverType = TryGetAvailableReceiverType(memberAccess.Expression) ??
                           TryGetCachedReceiverType(memberAccess.Expression);
        var hasReceiverType = receiverType is not null && receiverType.TypeKind != TypeKind.Error;

        if (!hasReceiverType)
            return false;

        var hasCachedExtensions = ExtensionMemberLookup.TryGetCached(
                binder,
                receiverType!,
                out var cachedExtensions,
                extensionName,
                includePartialMatches: false,
                kinds: ExtensionMemberKinds.InstanceMethods);
        var usedCachedExtensions = hasCachedExtensions && !cachedExtensions.IsEmpty;
        var extensions = usedCachedExtensions
            ? cachedExtensions
            : ExtensionMemberLookup.Lookup(
                binder,
                receiverType!,
                extensionName,
                includePartialMatches: false,
                kinds: ExtensionMemberKinds.InstanceMethods);

        methods = BuildAvailableExtensionMethods(extensions.InstanceMethods);

        if (methods.IsDefaultOrEmpty && usedCachedExtensions)
        {
            extensions = ExtensionMemberLookup.Lookup(
                binder,
                receiverType!,
                extensionName,
                includePartialMatches: false,
                kinds: ExtensionMemberKinds.InstanceMethods);
            methods = BuildAvailableExtensionMethods(extensions.InstanceMethods);
        }

        if (methods.IsDefaultOrEmpty)
            methods = BuildAvailableExtensionMethods(binder.LookupExtensionMethodsByName(extensionName));

        if (methods.IsDefaultOrEmpty)
            return false;

        if (hasReceiverType)
        {
            methods = methods
                .OrderByDescending(method => SymbolEqualityComparer.Default.Equals(method.Parameters[0].Type, receiverType))
                .ThenByDescending(method => method.Parameters[0].Type.TypeKind != TypeKind.Error &&
                                           Compilation.ClassifyConversion(receiverType!, method.Parameters[0].Type, includeUserDefined: true).Exists)
                .ToImmutableArray();
        }

        return methods.Length > 0;

        ImmutableArray<IMethodSymbol> BuildAvailableExtensionMethods(IEnumerable<IMethodSymbol> candidateMethods)
            => candidateMethods
                .Where(method => string.Equals(method.Name, extensionName, StringComparison.Ordinal))
                .Where(method => method.Parameters.Length >= invocation.ArgumentList.Arguments.Count + 1)
                .Select(method => ConstructAvailableExtensionCandidate(method, receiverType!))
                .OrderBy(static method => ContainsTypeParameter(method.ReturnType))
                .ToImmutableArray();
    }

    private static IMethodSymbol ConstructAvailableExtensionCandidate(IMethodSymbol method, ITypeSymbol receiverType)
    {
        if (receiverType.TypeKind == TypeKind.Error ||
            method.Parameters.IsDefaultOrEmpty ||
            method.Parameters.Length == 0 ||
            method.TypeParameters.IsDefaultOrEmpty)
        {
            return method;
        }

        var receiverParameterType = method.Parameters[0].Type;
        if (receiverParameterType is null ||
            receiverParameterType.TypeKind == TypeKind.Error ||
            !TryInferExtensionReceiverSubstitutions(receiverParameterType, receiverType, out var substitutions) ||
            substitutions.Count == 0)
        {
            return method;
        }

        var typeArguments = new ITypeSymbol[method.TypeParameters.Length];
        var changed = false;

        for (var i = 0; i < method.TypeParameters.Length; i++)
        {
            var typeParameter = method.TypeParameters[i];
            var existingArgument = method.TypeArguments.Length == method.TypeParameters.Length
                ? method.TypeArguments[i]
                : typeParameter;

            if (substitutions.TryGetValue(typeParameter, out var inferred) &&
                inferred.TypeKind != TypeKind.Error)
            {
                typeArguments[i] = inferred;
                changed |= !SymbolEqualityComparer.Default.Equals(existingArgument, inferred);
            }
            else
            {
                typeArguments[i] = existingArgument;
            }
        }

        return changed ? method.Construct(typeArguments) : method;
    }

    private ITypeSymbol? TryGetCachedReceiverType(ExpressionSyntax receiver)
    {
        if (_typeMappings.TryGetValue(receiver, out var receiverTypeInfo) &&
            HasTypeInfo(receiverTypeInfo))
        {
            var receiverType = receiverTypeInfo.Type ?? receiverTypeInfo.ConvertedType;
            if (receiverType is not null && receiverType.TypeKind != TypeKind.Error)
                return receiverType;
        }

        if (TryGetCachedBoundNode(receiver) is BoundExpression cachedExpression)
        {
            var receiverType = cachedExpression.Type ?? cachedExpression.GetConvertedType();
            if (receiverType is not null && receiverType.TypeKind != TypeKind.Error)
                return receiverType;
        }

        if (TryGetCachedSymbolInfo(receiver, out var receiverInfo))
            return GetTypeFromSymbol(receiverInfo.Symbol?.UnderlyingSymbol ?? receiverInfo.Symbol);

        return null;
    }

    private bool TryGetPatternConstantValueSymbolInfo(SyntaxNode node, out SymbolInfo info)
    {
        info = SymbolInfo.None;

        if (node is not IdentifierNameSyntax identifier)
            return false;

        var patternSyntax = (PatternSyntax?)identifier
            .AncestorsAndSelf()
            .OfType<DeclarationPatternSyntax>()
            .FirstOrDefault(pattern =>
                IsUndesignatedDeclarationPattern(pattern) &&
                pattern.Type.FullSpan.Contains(identifier.Span) &&
                IsTerminalTypeNameIdentifier(pattern.Type, identifier));

        patternSyntax ??= identifier
            .AncestorsAndSelf()
            .OfType<ConstantPatternSyntax>()
            .FirstOrDefault(pattern =>
                pattern.Expression is TypeSyntax typeSyntax &&
                typeSyntax.FullSpan.Contains(identifier.Span) &&
                IsTerminalTypeNameIdentifier(typeSyntax, identifier));

        if (patternSyntax is null)
            return false;

        BindPatternOwner(patternSyntax);

        if (TryGetCachedBoundNode(patternSyntax) is not BoundConstantPattern { Expression: { } valueExpression })
            return false;

        info = valueExpression.GetSymbolInfo();
        if (info.Symbol is null && info.CandidateSymbols.IsDefaultOrEmpty)
            return false;

        info = ProjectBackingFieldSymbolsToAssociatedProperty(node, info);
        return info.Symbol is not null || !info.CandidateSymbols.IsDefaultOrEmpty;

        static bool IsUndesignatedDeclarationPattern(DeclarationPatternSyntax pattern)
            => pattern.Designation is null
               || pattern.Designation is SingleVariableDesignationSyntax { Identifier.IsMissing: true }
               || pattern.Designation is SingleVariableDesignationSyntax { Identifier.Kind: SyntaxKind.None };

        static bool IsTerminalTypeNameIdentifier(TypeSyntax typeSyntax, IdentifierNameSyntax identifier)
        {
            var terminalName = GetTerminalTypeName(typeSyntax);
            return terminalName is IdentifierNameSyntax terminalIdentifier &&
                   terminalIdentifier.Kind == identifier.Kind &&
                   terminalIdentifier.Span == identifier.Span;
        }

        static SimpleNameSyntax? GetTerminalTypeName(TypeSyntax typeSyntax)
        {
            while (typeSyntax is QualifiedNameSyntax qualified)
                typeSyntax = (TypeSyntax)qualified.Right;

            return typeSyntax as SimpleNameSyntax;
        }
    }

    private void BindPatternOwner(PatternSyntax pattern)
    {
        var owner = pattern.Ancestors().FirstOrDefault(static ancestor =>
            ancestor is IsPatternExpressionSyntax or
                IfPatternStatementSyntax or
                WhilePatternStatementSyntax or
                MatchExpressionSyntax or
                MatchStatementSyntax or
                ForStatementSyntax or
                PatternDeclarationAssignmentStatementSyntax or
                CatchClauseSyntax);

        if (owner is not null)
            _ = GetBoundNode(owner);
    }

    internal bool TryGetAvailablePipeInvocationCandidates(InvocationExpressionSyntax invocation, out ImmutableArray<IMethodSymbol> methods)
    {
        methods = ImmutableArray<IMethodSymbol>.Empty;

        if (invocation.Parent is not InfixOperatorExpressionSyntax
            {
                OperatorToken.Kind: SyntaxKind.PipeToken
            } pipeExpression ||
            !IsSameSyntaxNode(pipeExpression.Right, invocation))
        {
            return false;
        }

        var methodName = invocation.Expression switch
        {
            IdentifierNameSyntax identifier => identifier.Identifier.ValueText,
            MemberAccessExpressionSyntax { Name: IdentifierNameSyntax identifier } => identifier.Identifier.ValueText,
            _ => null
        };
        if (string.IsNullOrWhiteSpace(methodName))
            return false;

        if (!TryGetAvailableTypeInfo(pipeExpression.Left, out var receiverTypeInfo))
            return false;

        var receiverType = receiverTypeInfo.Type ?? receiverTypeInfo.ConvertedType;
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return false;

        var extensions = LookupApplicableExtensionMembers(
            receiverType,
            invocation,
            methodName,
            kinds: ExtensionMemberKinds.InstanceMethods | ExtensionMemberKinds.StaticMethods);
        var candidates = extensions.InstanceMethods
            .Concat(extensions.StaticMethods)
            .Where(method => string.Equals(method.Name, methodName, StringComparison.Ordinal))
            .Where(method => method.Parameters.Length == invocation.ArgumentList.Arguments.Count + 1)
            .ToImmutableArray();

        if (candidates.IsDefaultOrEmpty)
            return false;

        methods = candidates
            .OrderByDescending(method => SymbolEqualityComparer.Default.Equals(method.Parameters[0].Type, receiverType))
            .ThenByDescending(method => method.Parameters[0].Type.TypeKind != TypeKind.Error &&
                                       Compilation.ClassifyConversion(receiverType, method.Parameters[0].Type, includeUserDefined: true).Exists)
            .ThenByDescending(static method => method.Parameters.Length > 1 &&
                                              IsExpressionTreeDelegateParameter(method.Parameters[1].Type))
            .ToImmutableArray();
        return methods.Length > 0;
    }

    private bool TryResolveAvailableCallableExpression(ExpressionSyntax expression, out ISymbol? symbol)
    {
        symbol = null;

        if (expression is IdentifierNameSyntax identifier)
        {
            if (TryLookupVisibleValueSymbol(identifier) is { } visibleSymbol)
            {
                symbol = visibleSymbol;
                return true;
            }

            if (TryLookupAvailableFunctionDeclaration(identifier, identifier.Identifier.ValueText, out var method))
            {
                symbol = method;
                return true;
            }

            if (TryLookupAvailableNamedType(identifier.Identifier.ValueText, 0, out var namedType))
            {
                symbol = namedType;
                return true;
            }
        }
        else if (expression is GenericNameSyntax genericName)
        {
            var typeArguments = ResolveAvailableTypeArguments(genericName.TypeArgumentList);
            if (typeArguments.IsDefault)
                return false;

            if (TryLookupAvailableNamedType(genericName.Identifier.ValueText, typeArguments.Length, out var genericType))
            {
                symbol = genericType.TypeParameters.Length == typeArguments.Length
                    ? genericType.Construct(typeArguments.ToArray()) as INamedTypeSymbol
                    : genericType;
                return symbol is not null;
            }
        }

        return false;
    }

    private bool TryLookupAvailableFunctionDeclaration(
        SyntaxNode contextNode,
        string name,
        out IMethodSymbol? method)
    {
        method = TryLookupAvailableFunctionDeclarations(contextNode, name, out var methods)
            ? methods[0]
            : null;
        return method is not null;
    }

    private bool TryLookupAvailableFunctionDeclarations(
        SyntaxNode contextNode,
        string name,
        out ImmutableArray<IMethodSymbol> methods)
    {
        methods = ImmutableArray<IMethodSymbol>.Empty;
        if (string.IsNullOrWhiteSpace(name))
            return false;

        var root = contextNode.SyntaxTree.GetRoot();
        if (root is CompilationUnitSyntax compilationUnit)
            EnsureTopLevelFunctionDeclarations(compilationUnit);

        methods = root
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Where(function => string.Equals(function.Identifier.ValueText, name, StringComparison.Ordinal))
            .Select(function => GetDeclaredSymbol(function) as IMethodSymbol)
            .Where(static symbol => symbol is not null)
            .Cast<IMethodSymbol>()
            .ToImmutableArray();

        return methods.Length > 0;
    }

    private bool TryLookupAvailableNamedType(string name, int arity, out INamedTypeSymbol? type)
    {
        type = null;
        if (string.IsNullOrWhiteSpace(name))
            return false;

        var root = SyntaxTree.GetRoot();
        type = root
            .DescendantNodes()
            .OfType<TypeDeclarationSyntax>()
            .Where(declaration => string.Equals(declaration.Identifier.ValueText, name, StringComparison.Ordinal))
            .Select(declaration => GetDeclaredSymbol(declaration) as INamedTypeSymbol)
            .FirstOrDefault(candidate => candidate is not null && (candidate.Arity == arity || candidate.TypeParameters.Length == arity));
        if (type is not null)
            return true;

        type = root
            .DescendantNodes()
            .OfType<UnionDeclarationSyntax>()
            .Where(declaration => string.Equals(declaration.Identifier.ValueText, name, StringComparison.Ordinal))
            .Select(declaration => GetDeclaredSymbol(declaration) as INamedTypeSymbol)
            .FirstOrDefault(candidate => candidate is not null && (candidate.Arity == arity || candidate.TypeParameters.Length == arity));
        if (type is not null)
            return true;

        if (TryLookupImportedMetadataTypeFromSyntax(root, name, arity, out type))
            return true;

        type = Compilation.GlobalNamespace
            .GetMembers(name)
            .OfType<INamedTypeSymbol>()
            .FirstOrDefault(candidate => candidate.Arity == arity || candidate.TypeParameters.Length == arity);
        if (type is not null)
            return true;

        type = Compilation.GlobalNamespace
            .GetAllMembersRecursive()
            .OfType<INamedTypeSymbol>()
            .FirstOrDefault(candidate =>
                string.Equals(candidate.Name, name, StringComparison.Ordinal) &&
                (candidate.Arity == arity || candidate.TypeParameters.Length == arity));
        return type is not null;
    }

    private bool TryLookupImportedMetadataTypeFromSyntax(
        SyntaxNode context,
        string name,
        int arity,
        out INamedTypeSymbol? type)
    {
        var metadataTypeName = GetMetadataTypeName(name, arity);
        foreach (var importDirective in EnumerateImportDirectives(context))
        {
            var importName = importDirective.Name.ToString().Replace(" ", string.Empty, StringComparison.Ordinal);
            if (string.IsNullOrWhiteSpace(importName))
                continue;

            if (importName.EndsWith(".*", StringComparison.Ordinal))
            {
                var namespaceName = importName[..^2];
                if (string.IsNullOrWhiteSpace(namespaceName))
                    continue;

                type = Compilation.TryGetMetadataReferenceTypeByMetadataName(namespaceName + "." + metadataTypeName);
                if (type is not null)
                    return true;

                continue;
            }

            var importedTypeName = importName;
            var lastDot = importedTypeName.LastIndexOf('.');
            var simpleImportedName = lastDot >= 0
                ? importedTypeName[(lastDot + 1)..]
                : importedTypeName;
            if (!string.Equals(simpleImportedName, name, StringComparison.Ordinal))
                continue;

            type = Compilation.TryGetMetadataReferenceTypeByMetadataName(arity > 0
                ? importedTypeName + "`" + arity
                : importedTypeName);
            if (type is not null)
                return true;
        }

        type = null;
        return false;
    }

    private ImmutableArray<ITypeSymbol> ResolveAvailableTypeArguments(TypeArgumentListSyntax typeArgumentList)
    {
        var builder = ImmutableArray.CreateBuilder<ITypeSymbol>(typeArgumentList.Arguments.Count);
        foreach (var argument in typeArgumentList.Arguments)
        {
            if (!TryResolveAvailableTypeSyntax(argument.Type, out var type))
                return default;

            builder.Add(type);
        }

        return builder.ToImmutable();
    }

    private bool TryResolveAvailableTypeSyntax(TypeSyntax typeSyntax, out ITypeSymbol type)
    {
        if (TryGetAvailableTypeInfo(typeSyntax, out var typeInfo) &&
            (typeInfo.Type ?? typeInfo.ConvertedType) is { TypeKind: not TypeKind.Error } availableType)
        {
            type = availableType;
            return true;
        }

        if (TryGetSpecialType(typeSyntax.ToString(), out type))
            return true;

        if (typeSyntax is IdentifierNameSyntax identifier &&
            TryLookupAvailableNamedType(identifier.Identifier.ValueText, 0, out var namedType))
        {
            type = namedType;
            return true;
        }

        type = Compilation.ErrorTypeSymbol;
        return false;
    }

    private bool TryGetSpecialType(string typeName, out ITypeSymbol type)
    {
        type = typeName switch
        {
            "bool" => Compilation.GetSpecialType(SpecialType.System_Boolean),
            "double" => Compilation.GetSpecialType(SpecialType.System_Double),
            "float" => Compilation.GetSpecialType(SpecialType.System_Single),
            "int" => Compilation.GetSpecialType(SpecialType.System_Int32),
            "nint" => Compilation.GetSpecialType(SpecialType.System_IntPtr),
            "nuint" => Compilation.GetSpecialType(SpecialType.System_UIntPtr),
            "string" => Compilation.GetSpecialType(SpecialType.System_String),
            "uint" => Compilation.GetSpecialType(SpecialType.System_UInt32),
            "unit" => Compilation.GetSpecialType(SpecialType.System_Unit),
            _ => Compilation.ErrorTypeSymbol
        };

        return type.TypeKind != TypeKind.Error;
    }

    private void AddAvailableConstructors(INamedTypeSymbol type, Action<IMethodSymbol?> addIfNotPresent)
    {
        if (type.TryGetUnion() is { } union)
        {
            foreach (var memberType in union.MemberTypes)
            {
                if (type.TryGetUnionCarrierConstructor(memberType, out var unionConstructor))
                    addIfNotPresent(unionConstructor);
            }

            foreach (var caseType in union.DeclaredCaseTypes)
            {
                if (type.TryGetUnionCarrierConstructor(caseType, out var unionConstructor))
                    addIfNotPresent(unionConstructor);
            }

            if (type.InstanceConstructors.IsDefaultOrEmpty || type.InstanceConstructors.Length == 0)
                AddAvailableStandardUnionConstructors(type, addIfNotPresent);
        }

        foreach (var constructor in type.InstanceConstructors)
            addIfNotPresent(constructor);
    }

    private void AddAvailableStandardUnionConstructors(INamedTypeSymbol unionType, Action<IMethodSymbol?> addIfNotPresent)
    {
        var declaration = unionType.DeclaringSyntaxReferences
            .Select(static reference => reference.GetSyntax())
            .OfType<UnionDeclarationSyntax>()
            .FirstOrDefault();
        if (declaration?.MemberTypes is not { } memberTypes)
            return;

        var unitType = Compilation.GetSpecialType(SpecialType.System_Unit);
        var namespaceSymbol = unionType.ContainingNamespace;

        foreach (var memberTypeSyntax in memberTypes.Types)
        {
            if (!TryResolveAvailableTypeSyntax(memberTypeSyntax, out var memberType))
                continue;

            var constructor = new SourceMethodSymbol(
                ".ctor",
                unitType,
                ImmutableArray<SourceParameterSymbol>.Empty,
                unionType,
                unionType,
                namespaceSymbol,
                [memberTypeSyntax.GetLocation()],
                Array.Empty<SyntaxReference>(),
                isStatic: false,
                methodKind: MethodKind.Constructor,
                declaredAccessibility: Accessibility.Public);

            var parameter = new SourceParameterSymbol(
                "value",
                memberType,
                constructor,
                unionType,
                namespaceSymbol,
                [memberTypeSyntax.GetLocation()],
                Array.Empty<SyntaxReference>());

            constructor.SetParameters([parameter]);
            addIfNotPresent(constructor);
        }
    }

    private static void AddInvokeCandidatesFromType(INamedTypeSymbol type, Action<IMethodSymbol?> addIfNotPresent)
    {
        foreach (var invokeCandidate in type.GetMembers("Invoke").OfType<IMethodSymbol>())
        {
            if (!invokeCandidate.IsStatic)
                addIfNotPresent(invokeCandidate);
        }
    }

    private bool TryGetAvailableMemberAccessSymbolInfo(SyntaxNode node, out SymbolInfo info)
    {
        var memberAccess = node switch
        {
            MemberAccessExpressionSyntax access => access,
            IdentifierNameSyntax identifier when
                identifier.Parent is MemberAccessExpressionSyntax access &&
                IsSameSyntaxNode(access.Name, identifier) => access,
            _ => null
        };

        if (memberAccess?.Name is not IdentifierNameSyntax memberName)
        {
            info = default;
            return false;
        }

        if (TryResolveAvailableTypeExpression(memberAccess.Expression, out var typeExpressionType) &&
            typeExpressionType is not null)
        {
            var staticMembers = LookupAvailableMembers(typeExpressionType, memberName.Identifier.ValueText);
            if (!staticMembers.IsDefaultOrEmpty)
            {
                info = CreateAvailableMemberAccessSymbolInfo(node, staticMembers);
                _symbolMappings[node] = info;
                if (!ReferenceEquals(node, memberAccess))
                    _symbolMappings[memberAccess] = info;

                if (info.Symbol is { } staticMember)
                    StoreNodeInterestSymbolDescriptor(node, staticMember);
                return true;
            }
        }

        var receiverType = TryGetAvailableReceiverType(memberAccess.Expression);
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
        {
            info = default;
            return false;
        }

        var members = LookupAvailableMembers(receiverType, memberName.Identifier.ValueText);
        if (members.IsDefaultOrEmpty)
        {
            info = default;
            return false;
        }

        info = CreateAvailableMemberAccessSymbolInfo(node, members);
        _symbolMappings[node] = info;
        if (!ReferenceEquals(node, memberAccess))
            _symbolMappings[memberAccess] = info;

        if (info.Symbol is { } member)
            StoreNodeInterestSymbolDescriptor(node, member);
        return true;
    }

    private SymbolInfo CreateAvailableMemberAccessSymbolInfo(SyntaxNode node, ImmutableArray<ISymbol> members)
    {
        var selected = members.Length == 1
            ? members[0]
            : members.FirstOrDefault(static member => member is IPropertySymbol or IFieldSymbol or IEventSymbol);
        var info = selected is not null
            ? new SymbolInfo(selected, members)
            : new SymbolInfo(CandidateReason.MemberGroup, members);
        return ProjectBackingFieldSymbolsToAssociatedProperty(node, info);
    }

    private static INamedTypeSymbol? GetNamedTypeFromAvailableSymbol(ISymbol? symbol)
        => symbol switch
        {
            INamedTypeSymbol type => type,
            IAliasSymbol { UnderlyingSymbol: INamedTypeSymbol aliasedType } => aliasedType,
            _ => null
        };

    private bool TryResolveAvailableTypeExpression(
        ExpressionSyntax expression,
        [System.Diagnostics.CodeAnalysis.NotNullWhen(true)] out INamedTypeSymbol? type)
    {
        type = null;

        if (expression is SimpleNameSyntax simpleName)
        {
            return TryLookupAvailableNamedType(simpleName.Identifier.ValueText, simpleName is GenericNameSyntax genericName
                ? genericName.TypeArgumentList.Arguments.Count
                : 0, out type) &&
                type is not null;
        }

        var metadataName = expression.ToString();
        type = Compilation.GetTypeByMetadataName(metadataName);
        if (type is not null)
            return true;

        if (expression is not MemberAccessExpressionSyntax memberAccess ||
            memberAccess.Name is not SimpleNameSyntax memberName)
        {
            return false;
        }

        var arity = memberName is GenericNameSyntax memberGeneric
            ? memberGeneric.TypeArgumentList.Arguments.Count
            : 0;
        metadataName = memberAccess.Expression + "." + GetMetadataTypeName(memberName.Identifier.ValueText, arity);
        type = Compilation.GetTypeByMetadataName(metadataName);
        return type is not null;
    }

    private ITypeSymbol? TryGetAvailableReceiverType(ExpressionSyntax receiver)
    {
        if (TryGetAvailableTypeInfo(receiver, out var receiverTypeInfo))
        {
            var receiverType = receiverTypeInfo.Type ?? receiverTypeInfo.ConvertedType;
            if (receiverType is not null && receiverType.TypeKind != TypeKind.Error)
                return receiverType;
        }

        if (TryGetAvailableSymbolInfo(receiver, out var receiverInfo))
            return GetTypeFromSymbol(receiverInfo.Symbol?.UnderlyingSymbol ?? receiverInfo.Symbol);

        return null;
    }

    private static ISymbol? LookupAvailableMember(ITypeSymbol receiverType, string memberName)
    {
        if (string.IsNullOrWhiteSpace(memberName))
            return null;

        for (var current = receiverType as INamedTypeSymbol; current is not null; current = current.BaseType)
        {
            var member = current
                .GetMembers(memberName)
                .FirstOrDefault(static candidate => candidate is IPropertySymbol or IFieldSymbol or IEventSymbol or IMethodSymbol);
            if (member is not null)
                return member;
        }

        return null;
    }

    private static ImmutableArray<ISymbol> LookupAvailableMembers(ITypeSymbol receiverType, string memberName)
    {
        if (string.IsNullOrWhiteSpace(memberName))
            return ImmutableArray<ISymbol>.Empty;

        for (var current = receiverType as INamedTypeSymbol; current is not null; current = current.BaseType)
        {
            var members = current
                .GetMembers(memberName)
                .Where(static candidate => candidate is IPropertySymbol or IFieldSymbol or IEventSymbol or IMethodSymbol)
                .ToImmutableArray();
            if (!members.IsDefaultOrEmpty)
                return members;
        }

        return ImmutableArray<ISymbol>.Empty;
    }

    /// <summary>
    /// Tries to retrieve type information from semantic state that is already available.
    /// This method does not bind cold bodies, create operations, or run diagnostics.
    /// </summary>
    internal bool TryGetAvailableTypeInfo(ExpressionSyntax expression, out TypeInfo typeInfo)
    {
        if (_typeMappings.TryGetValue(expression, out typeInfo) &&
            HasNonErrorTypeInfo(typeInfo))
        {
            if (TryGetContextualArgumentConvertedType(expression, typeInfo.Type, out var contextualConvertedType) &&
                !SymbolEqualityComparer.Default.Equals(typeInfo.ConvertedType, contextualConvertedType))
            {
                typeInfo = new TypeInfo(
                    typeInfo.Type,
                    contextualConvertedType,
                    ComputeConversion(typeInfo.Type, contextualConvertedType));
                _typeMappings[expression] = typeInfo;
            }

            return true;
        }

        if (TryGetAvailableLiteralType(expression, out var literalType) &&
            literalType is not null &&
            literalType.TypeKind != TypeKind.Error)
        {
            var convertedType = literalType;
            if (TryGetTargetTypeForExpression(expression, out var targetType) &&
                targetType is not null &&
                targetType.TypeKind != TypeKind.Error)
            {
                convertedType = targetType;
            }

            typeInfo = new TypeInfo(literalType, convertedType, ComputeConversion(literalType, convertedType));
            _typeMappings[expression] = typeInfo;
            return true;
        }

        if (expression is PrefixOperatorExpressionSyntax { Kind: SyntaxKind.AwaitExpression } awaitExpression &&
            TryGetAvailableAwaitExpressionType(awaitExpression, out var awaitType) &&
            awaitType is not null &&
            awaitType.TypeKind != TypeKind.Error)
        {
            typeInfo = new TypeInfo(awaitType, awaitType, ComputeConversion(awaitType, awaitType));
            _typeMappings[expression] = typeInfo;
            return true;
        }

        if (expression is InfixOperatorExpressionSyntax infixExpression &&
            TryGetAvailableInfixExpressionType(infixExpression, out typeInfo))
        {
            _typeMappings[expression] = typeInfo;
            return true;
        }

        if (expression is FunctionExpressionSyntax functionExpression &&
            TryGetAvailableOrCachedFunctionExpressionDelegateType(functionExpression, out var functionDelegateType) &&
            functionDelegateType is not null &&
            functionDelegateType.TypeKind != TypeKind.Error)
        {
            typeInfo = new TypeInfo(
                functionDelegateType,
                functionDelegateType,
                ComputeConversion(functionDelegateType, functionDelegateType));
            _typeMappings[expression] = typeInfo;
            return true;
        }

        if (TryGetCachedBoundNode(expression) is BoundExpression cachedExpression &&
            !IsLikelyStaleFunctionBodyNode(cachedExpression))
        {
            var type = cachedExpression.Type;
            var convertedType = cachedExpression.GetConvertedType() ?? type;
            if (TryGetContextualArgumentConvertedType(expression, type, out var contextualConvertedType))
                convertedType = contextualConvertedType;

            if ((type is not null && type.TypeKind != TypeKind.Error) ||
                (convertedType is not null && convertedType.TypeKind != TypeKind.Error))
            {
                var conversion = cachedExpression switch
                {
                    BoundConversionExpression cast => cast.Conversion,
                    BoundAsExpression asExpression => asExpression.Conversion,
                    _ => ComputeConversion(type, convertedType)
                };

                typeInfo = new TypeInfo(type, convertedType, conversion);
                _typeMappings[expression] = typeInfo;
                return true;
            }
        }

        if (expression is InvocationExpressionSyntax invocation &&
            TryGetAvailableInvocationCandidates(invocation, out var invocationCandidates))
        {
            var inferredType = invocationCandidates
                .Select(static method => method.MethodKind == MethodKind.Constructor ? method.ContainingType : method.ReturnType)
                .FirstOrDefault(IsUsefulAvailableExpressionType);
            if (inferredType is not null)
            {
                var convertedType = inferredType;
                if (TryGetContextualArgumentConvertedType(expression, inferredType, out var contextualConvertedType))
                    convertedType = contextualConvertedType;

                typeInfo = new TypeInfo(inferredType, convertedType, ComputeConversion(inferredType, convertedType));
                _typeMappings[expression] = typeInfo;
                return true;
            }
        }

        if (expression is InfixOperatorExpressionSyntax
            {
                OperatorToken.Kind: SyntaxKind.PipeToken,
                Right: InvocationExpressionSyntax pipeInvocation
            } &&
            TryGetAvailablePipeInvocationCandidates(pipeInvocation, out var pipeCandidates))
        {
            var inferredType = pipeCandidates
                .Select(static method => method.ReturnType)
                .FirstOrDefault(static type => type is not null && type.TypeKind != TypeKind.Error);
            if (inferredType is not null)
            {
                typeInfo = new TypeInfo(inferredType, inferredType, ComputeConversion(inferredType, inferredType));
                _typeMappings[expression] = typeInfo;
                return true;
            }
        }

        if (expression is IdentifierNameSyntax identifier &&
            TryGetEnclosingParameterTypeFromSyntax(identifier, out var functionParameterType))
        {
            typeInfo = new TypeInfo(functionParameterType, functionParameterType, ComputeConversion(functionParameterType, functionParameterType));
            _typeMappings[expression] = typeInfo;
            return true;
        }

        if (TryGetAvailableSymbolInfo(expression, out var symbolInfo) &&
            GetTypeFromSymbol(symbolInfo.Symbol?.UnderlyingSymbol ?? symbolInfo.Symbol) is { } symbolType &&
            symbolType.TypeKind != TypeKind.Error)
        {
            typeInfo = new TypeInfo(symbolType, symbolType, ComputeConversion(symbolType, symbolType));
            _typeMappings[expression] = typeInfo;
            return true;
        }

        typeInfo = new TypeInfo(null, null);
        return false;
    }

    private static bool IsUsefulAvailableExpressionType(ITypeSymbol? type)
        => type is not null &&
           type.TypeKind != TypeKind.Error &&
           type is not ITypeParameterSymbol;

    private bool TryGetAvailableInfixExpressionType(
        InfixOperatorExpressionSyntax expression,
        out TypeInfo typeInfo)
    {
        typeInfo = default;

        if (expression.OperatorToken.Kind == SyntaxKind.PipeToken)
            return false;

        if (!TryGetAvailableTypeInfo(expression.Left, out var leftInfo) ||
            !TryGetAvailableTypeInfo(expression.Right, out var rightInfo))
        {
            return false;
        }

        var leftType = leftInfo.ConvertedType ?? leftInfo.Type;
        var rightType = rightInfo.ConvertedType ?? rightInfo.Type;
        if (leftType is null ||
            rightType is null ||
            leftType.TypeKind == TypeKind.Error ||
            rightType.TypeKind == TypeKind.Error)
        {
            return false;
        }

        if (!BoundBinaryOperator.TryLookup(Compilation, expression.OperatorToken.Kind, leftType, rightType, out var op) ||
            op.ResultType.TypeKind == TypeKind.Error)
        {
            return false;
        }

        var resultType = op.ResultType;
        var convertedType = resultType;
        if (TryGetTargetTypeForExpression(expression, out var targetType) &&
            targetType is not null &&
            targetType.TypeKind != TypeKind.Error)
        {
            convertedType = targetType;
        }

        typeInfo = new TypeInfo(resultType, convertedType, ComputeConversion(resultType, convertedType));
        return true;
    }

    private bool TryGetAvailableAwaitExpressionType(
        PrefixOperatorExpressionSyntax awaitExpression,
        [System.Diagnostics.CodeAnalysis.NotNullWhen(true)] out ITypeSymbol? type)
    {
        type = null;

        if (awaitExpression.Expression is InvocationExpressionSyntax invocation &&
            TryGetAvailableInvocationCandidates(invocation, out var invocationCandidates) &&
            invocationCandidates.Any(static method => method.TypeParameters.Length > 0 && HasUnresolvedMethodTypeParameters(method)))
        {
            return false;
        }

        if (!TryGetAvailableTypeInfo(awaitExpression.Expression, out var operandTypeInfo))
            return false;

        var operandType = operandTypeInfo.Type ?? operandTypeInfo.ConvertedType;
        if (operandType is null || operandType.TypeKind == TypeKind.Error)
            return false;

        type = AsyncReturnTypeUtilities.ExtractAsyncResultType(Compilation, operandType);
        if (type is not null && type.TypeKind != TypeKind.Error)
            return true;

        if (AwaitablePattern.TryFind(operandType, isAccessible: null, out var awaitable, out _, out _))
        {
            type = awaitable.GetResultMethod.ReturnType;
            if (type.SpecialType == SpecialType.System_Void)
                type = Compilation.GetSpecialType(SpecialType.System_Unit);

            return type is not null && type.TypeKind != TypeKind.Error;
        }

        type = null;
        return false;
    }

    private bool TryGetEnclosingParameterTypeFromSyntax(
        IdentifierNameSyntax identifier,
        [System.Diagnostics.CodeAnalysis.NotNullWhen(true)] out ITypeSymbol? type)
    {
        type = null;
        var name = identifier.Identifier.ValueText;

        foreach (var ancestor in identifier.Ancestors())
        {
            IEnumerable<ParameterSyntax> parameters = ancestor switch
            {
                BaseMethodDeclarationSyntax { ParameterList: { } parameterList } => parameterList.Parameters,
                FunctionStatementSyntax { ParameterList: { } parameterList } => parameterList.Parameters,
                SimpleFunctionExpressionSyntax { Parameter: { } parameter } => [parameter],
                ParenthesizedFunctionExpressionSyntax { ParameterList: { } parameterList } => parameterList.Parameters,
                TrailingBlockExpressionSyntax { Parameter: { } parameter } => [parameter],
                TrailingBlockExpressionSyntax { ParameterList: { } parameterList } => parameterList.Parameters,
                _ => Enumerable.Empty<ParameterSyntax>()
            };

            foreach (var parameter in parameters)
            {
                if (!string.Equals(parameter.Identifier.ValueText, name, StringComparison.Ordinal))
                {
                    continue;
                }

                if (TryGetAvailableParameterType(parameter, out type) && type is not null)
                    return true;
            }
        }

        return false;
    }

    private bool TryGetAvailableParameterType(
        ParameterSyntax parameter,
        [System.Diagnostics.CodeAnalysis.NotNullWhen(true)] out ITypeSymbol? type)
    {
        type = null;

        if (!parameter.Ancestors().OfType<FunctionExpressionSyntax>().Any() &&
            TryResolveParameterSymbolFast(parameter, out var parameterSymbol) &&
            parameterSymbol?.Type is { TypeKind: not TypeKind.Error } symbolType)
        {
            type = symbolType;
            return true;
        }

        if (TryResolveFunctionExpressionParameterSymbolFast(parameter, out var functionParameterSymbol) &&
            functionParameterSymbol?.Type is { TypeKind: not TypeKind.Error } functionParameterType)
        {
            type = functionParameterType;
            return true;
        }

        if (parameter.TypeAnnotation?.Type is { } typeSyntax &&
            TryGetAvailableFunctionParameterType(typeSyntax, out var syntaxType) &&
            syntaxType.TypeKind != TypeKind.Error)
        {
            type = syntaxType;
            return true;
        }

        return false;
    }

    private bool TryGetAvailableLiteralType(ExpressionSyntax expression, out ITypeSymbol? type)
    {
        type = null;

        if (expression is not LiteralExpressionSyntax literal)
            return false;

        if (literal.Kind == SyntaxKind.NullLiteralExpression)
        {
            type = Compilation.NullTypeSymbol;
            return true;
        }

        type = literal.Token.Value switch
        {
            byte => Compilation.GetSpecialType(SpecialType.System_Byte),
            int => Compilation.GetSpecialType(SpecialType.System_Int32),
            long => Compilation.GetSpecialType(SpecialType.System_Int64),
            float => Compilation.GetSpecialType(SpecialType.System_Single),
            double => Compilation.GetSpecialType(SpecialType.System_Double),
            decimal => Compilation.GetSpecialType(SpecialType.System_Decimal),
            bool => Compilation.GetSpecialType(SpecialType.System_Boolean),
            char => Compilation.GetSpecialType(SpecialType.System_Char),
            string => Compilation.GetSpecialType(SpecialType.System_String),
            _ => null
        };

        return type is not null;
    }

    /// <summary>
    /// Tries to retrieve type syntax information from semantic state that is already available.
    /// This method does not bind cold bodies, create operations, or run diagnostics.
    /// </summary>
    internal bool TryGetAvailableTypeInfo(TypeSyntax typeSyntax, out TypeInfo typeInfo)
    {
        if (_typeMappings.TryGetValue(typeSyntax, out typeInfo) &&
            HasNonErrorTypeInfo(typeInfo))
        {
            return true;
        }

        if (TryGetAvailablePredefinedTypeInfo(typeSyntax, out typeInfo))
        {
            _typeMappings[typeSyntax] = typeInfo;
            return true;
        }

        if (typeSyntax is ExpressionSyntax expressionSyntax &&
            !IsExplicitTypeSyntaxContext(typeSyntax))
        {
            return TryGetAvailableTypeInfo(expressionSyntax, out typeInfo);
        }

        if (typeSyntax is SimpleNameSyntax typeName &&
            TryLookupAvailableTypeFromBinder(typeName, out var binderType) &&
            binderType is not null &&
            binderType.TypeKind != TypeKind.Error)
        {
            if (typeName is GenericNameSyntax genericName &&
                binderType is INamedTypeSymbol namedType)
            {
                var typeArguments = ResolveAvailableTypeArguments(genericName.TypeArgumentList);
                if (!typeArguments.IsDefaultOrEmpty &&
                    typeArguments.Length == genericName.TypeArgumentList.Arguments.Count)
                {
                    binderType = namedType.Construct(typeArguments.ToArray());
                }
                else
                {
                    binderType = null;
                }
            }

            if (binderType is not null)
            {
                typeInfo = new TypeInfo(binderType, binderType, ComputeConversion(binderType, binderType));
                _typeMappings[typeSyntax] = typeInfo;
                return true;
            }
        }

        if (TryGetAvailableSymbolInfo(typeSyntax, out var symbolInfo))
        {
            var type = symbolInfo.Symbol switch
            {
                ITypeSymbol typeSymbol => typeSymbol,
                IAliasSymbol { UnderlyingSymbol: ITypeSymbol aliasedType } => aliasedType,
                _ => null
            };

            if (type is not null && type.TypeKind != TypeKind.Error)
            {
                typeInfo = new TypeInfo(type, type, ComputeConversion(type, type));
                _typeMappings[typeSyntax] = typeInfo;
                return true;
            }
        }

        if (TryBindAvailableTypeSyntax(typeSyntax, out var boundType) &&
            boundType is not null &&
            boundType.TypeKind != TypeKind.Error)
        {
            typeInfo = new TypeInfo(boundType, boundType, ComputeConversion(boundType, boundType));
            _typeMappings[typeSyntax] = typeInfo;
            return true;
        }

        typeInfo = new TypeInfo(null, null);
        return false;
    }

    private bool TryBindAvailableTypeSyntax(TypeSyntax typeSyntax, [System.Diagnostics.CodeAnalysis.NotNullWhen(true)] out ITypeSymbol? type)
    {
        type = null;

        try
        {
            type = GetBinder(typeSyntax).BindTypeSyntax(typeSyntax).ResolvedType;
            return type is not null;
        }
        catch
        {
            type = null;
            return false;
        }
    }

    private bool TryGetAvailablePredefinedTypeInfo(TypeSyntax typeSyntax, out TypeInfo typeInfo)
    {
        var specialType = typeSyntax switch
        {
            PredefinedTypeSyntax predefined => predefined.Keyword.Kind switch
            {
                SyntaxKind.BoolKeyword => SpecialType.System_Boolean,
                SyntaxKind.DoubleKeyword => SpecialType.System_Double,
                SyntaxKind.FloatKeyword => SpecialType.System_Single,
                SyntaxKind.IntKeyword => SpecialType.System_Int32,
                SyntaxKind.NIntKeyword => SpecialType.System_IntPtr,
                SyntaxKind.NUIntKeyword => SpecialType.System_UIntPtr,
                SyntaxKind.StringKeyword => SpecialType.System_String,
                SyntaxKind.UIntKeyword => SpecialType.System_UInt32,
                SyntaxKind.UnitKeyword => SpecialType.System_Unit,
                _ => SpecialType.None
            },
            UnitTypeSyntax => SpecialType.System_Unit,
            _ => SpecialType.None
        };

        if (specialType == SpecialType.None)
        {
            typeInfo = default;
            return false;
        }

        var type = Compilation.GetSpecialType(specialType);
        if (type.TypeKind == TypeKind.Error)
        {
            typeInfo = default;
            return false;
        }

        typeInfo = new TypeInfo(type, type, ComputeConversion(type, type));
        return true;
    }

    private bool TryLookupAvailableTypeFromBinder(SimpleNameSyntax typeName, out ITypeSymbol? type)
    {
        var arity = typeName is GenericNameSyntax genericName
            ? genericName.TypeArgumentList.Arguments.Count
            : 0;

        type = GetBinderForIncrementalSemanticQuery(typeName).LookupType(typeName.Identifier.ValueText);
        if (IsMatchingAvailableType(type, arity))
            return true;

        type = LookupImportedType(typeName, arity);
        if (type is not null)
            return true;

        var metadataName = GetMetadataTypeName(typeName.Identifier.ValueText, arity);
        type = Compilation.GetTypeByMetadataName(metadataName);
        return type is not null;
    }

    private ITypeSymbol? LookupImportedType(SimpleNameSyntax typeName, int arity)
    {
        for (var binder = GetBinderForIncrementalSemanticQuery(typeName); binder is not null; binder = binder.ParentBinder)
        {
            if (binder is ImportBinder importBinder)
            {
                foreach (var scope in importBinder.GetImportedNamespacesOrTypeScopes())
                {
                    var type = scope.LookupType(typeName.Identifier.ValueText);
                    if (IsMatchingAvailableType(type, arity))
                        return type;

                    if (scope is INamespaceSymbol namespaceSymbol &&
                        TryGetNamespaceMetadataName(namespaceSymbol, out var namespaceMetadataName) &&
                        Compilation.GetTypeByMetadataName(namespaceMetadataName + "." + GetMetadataTypeName(typeName.Identifier.ValueText, arity)) is { } metadataType)
                    {
                        return metadataType;
                    }
                }

                foreach (var importedType in importBinder.GetImportedTypes())
                {
                    if (string.Equals(importedType.Name, typeName.Identifier.ValueText, StringComparison.Ordinal) &&
                        IsMatchingAvailableType(importedType, arity))
                    {
                        return importedType;
                    }
                }
            }

            if (binder is NamespaceBinder namespaceBinder)
            {
                var type = namespaceBinder.NamespaceSymbol.LookupType(typeName.Identifier.ValueText);
                if (IsMatchingAvailableType(type, arity))
                    return type;
            }
        }

        return null;
    }

    private static string GetMetadataTypeName(string name, int arity)
        => arity > 0 ? name + "`" + arity : name;

    private static bool TryGetNamespaceMetadataName(INamespaceSymbol namespaceSymbol, out string metadataName)
    {
        metadataName = namespaceSymbol.ToMetadataName() ?? string.Empty;
        return !string.IsNullOrWhiteSpace(metadataName);
    }

    private static bool IsMatchingAvailableType(ITypeSymbol? type, int arity)
        => type is not null &&
           type.TypeKind != TypeKind.Error &&
           (type is not INamedTypeSymbol namedType ||
            namedType.Arity == arity ||
            namedType.TypeParameters.Length == arity);

    private static bool HasTypeInfo(TypeInfo typeInfo)
        => typeInfo.Type is not null || typeInfo.ConvertedType is not null;

    private static bool HasNonErrorTypeInfo(TypeInfo typeInfo)
        => (typeInfo.Type is not null && typeInfo.Type.TypeKind != TypeKind.Error) ||
           (typeInfo.ConvertedType is not null && typeInfo.ConvertedType.TypeKind != TypeKind.Error);

    private bool TryGetCachedBoundSymbolInfo(SyntaxNode node, out SymbolInfo info)
    {
        if (TryGetCachedBoundNode(node) is not { } boundNode)
        {
            info = default;
            return false;
        }

        info = boundNode switch
        {
            BoundExpression expression => expression.GetSymbolInfo(),
            BoundStatement statement => statement.GetSymbolInfo(),
            _ => default
        };
        if (!HasSymbolInfo(info))
            return false;

        Compilation.PerformanceInstrumentation.SemanticQuery.RecordSymbolInfoBoundCacheHit();
        info = ProjectBackingFieldSymbolsToAssociatedProperty(node, info);
        _symbolMappings[node] = info;
        return true;
    }

    private static bool HasSymbolInfo(SymbolInfo info)
        => info.Symbol is not null || !info.CandidateSymbols.IsDefaultOrEmpty;

    private bool TryGetCachedNodeInterestSymbolInfo(SyntaxNode node, out SymbolInfo info)
    {
        if (Compilation.TryGetNodeInterestSymbolDescriptor(node, out var cachedDescriptor) &&
            TryResolveNodeInterestSymbolDescriptor(cachedDescriptor, out var cachedSymbol))
        {
            info = new SymbolInfo(cachedSymbol);
            return true;
        }

        info = default;
        return false;
    }

    private bool TryGetDeclarationSymbolInfo(SyntaxNode node, out SymbolInfo info)
    {
        var declarationNode = node switch
        {
            ParameterSyntax => node,
            VariableDeclaratorSyntax => node,
            SingleVariableDesignationSyntax => node,
            FunctionExpressionSyntax => node,
            IdentifierNameSyntax identifier => TryGetIdentifierDeclarationParent(identifier),
            _ => null
        };

        var symbol = declarationNode switch
        {
            null => null,
            FunctionExpressionSyntax functionExpression when TryGetFunctionExpressionSymbol(functionExpression, out var functionSymbol) => functionSymbol,
            ParameterSyntax parameter when TryResolveFunctionExpressionParameterSymbolFast(parameter, out var fastFunctionParameter) => fastFunctionParameter,
            ParameterSyntax parameter when parameter.Ancestors().OfType<FunctionExpressionSyntax>().Any() => null,
            ParameterSyntax parameter => TryResolveParameterSymbolFast(parameter, out var parameterSymbol) ? parameterSymbol : GetDeclaredSymbol(parameter),
            VariableDeclaratorSyntax variableDeclarator when TryGetStableLocalDeclarationSymbol(variableDeclarator, out var stableLocalSymbol) => stableLocalSymbol,
            VariableDeclaratorSyntax variableDeclarator when TryResolveAvailableLocalSymbol(variableDeclarator, out var localSymbol) => localSymbol,
            SingleVariableDesignationSyntax designation when TryResolveAvailablePatternDesignationSymbol(designation, out var designationSymbol, allowErrorType: true) => designationSymbol,
            _ => GetDeclaredSymbol(declarationNode)
        };

        if (symbol is null)
        {
            info = default;
            return false;
        }

        info = new SymbolInfo(symbol);
        StoreSymbolInfo(node, symbol);
        return true;
    }

    private static SyntaxNode? TryGetIdentifierDeclarationParent(IdentifierNameSyntax identifier)
        => identifier.Parent switch
        {
            TypeDeclarationSyntax declaration when declaration.Identifier.Span == identifier.Identifier.Span => declaration,
            UnionDeclarationSyntax declaration when declaration.Identifier.Span == identifier.Identifier.Span => declaration,
            CaseDeclarationSyntax declaration when declaration.Identifier.Span == identifier.Identifier.Span => declaration,
            DelegateDeclarationSyntax declaration when declaration.Identifier.Span == identifier.Identifier.Span => declaration,
            MethodDeclarationSyntax declaration when declaration.Identifier.Span == identifier.Identifier.Span => declaration,
            ConstructorDeclarationSyntax declaration when declaration.InitKeyword.Span == identifier.Identifier.Span => declaration,
            ParameterlessConstructorDeclarationSyntax declaration when declaration.InitKeyword.Span == identifier.Identifier.Span => declaration,
            FunctionStatementSyntax declaration when declaration.Identifier.Span == identifier.Identifier.Span => declaration,
            PropertyDeclarationSyntax declaration when declaration.Identifier.Span == identifier.Identifier.Span => declaration,
            EventDeclarationSyntax declaration when declaration.Identifier.Span == identifier.Identifier.Span => declaration,
            AccessorDeclarationSyntax declaration when declaration.Keyword.Span == identifier.Identifier.Span => declaration,
            ParameterSyntax declaration when declaration.Identifier.Span == identifier.Identifier.Span => declaration,
            VariableDeclaratorSyntax declaration when declaration.Identifier.Span == identifier.Identifier.Span => declaration,
            _ => null
        };

    private bool TryBindExactSymbol(SyntaxNode node, out SymbolInfo info)
    {
        var binder = GetBinderForIncrementalSemanticQuery(node);
        info = binder.BindReferencedSymbol(node);
        return info.Symbol is not null || !info.CandidateSymbols.IsDefaultOrEmpty;
    }

    private static bool IsSameSyntaxNode(SyntaxNode? left, SyntaxNode? right)
    {
        return ReferenceEquals(left, right) ||
               left is not null &&
               right is not null &&
               left.Kind == right.Kind &&
               left.Span == right.Span &&
               ReferenceEquals(left.SyntaxTree, right.SyntaxTree);
    }

    internal bool TryGetNodeInterestSymbolInfo(SyntaxNode node, out SymbolInfo info)
    {
        info = default;

        if (TryGetCachedNodeInterestSymbolInfo(node, out info))
            return true;

        switch (node)
        {
            case IdentifierNameSyntax identifier when TryGetCasePatternHeadSymbol(identifier, out var casePatternSymbol):
                {
                    info = new SymbolInfo(casePatternSymbol);
                    StoreNodeInterestSymbolDescriptor(node, casePatternSymbol);
                    return true;
                }

            case GenericNameSyntax genericName when TryGetCasePatternHeadSymbol(genericName, out var genericCasePatternSymbol):
                {
                    info = new SymbolInfo(genericCasePatternSymbol);
                    StoreNodeInterestSymbolDescriptor(node, genericCasePatternSymbol);
                    return true;
                }

            case IdentifierNameSyntax identifier:
                {
                    if (identifier.Parent is MemberAccessExpressionSyntax memberAccess)
                    {
                        if (ReferenceEquals(memberAccess.Name, identifier))
                            return false;

                        if (ReferenceEquals(memberAccess.Expression, identifier) &&
                            TryLookupVisibleValueSymbol(identifier) is { } receiverSymbol)
                        {
                            info = new SymbolInfo(receiverSymbol);
                            StoreNodeInterestSymbolDescriptor(node, receiverSymbol);
                            return true;
                        }
                    }

                    if (TryLookupVisibleValueSymbol(identifier) is { } visibleSymbol)
                    {
                        info = new SymbolInfo(visibleSymbol);
                        StoreNodeInterestSymbolDescriptor(node, visibleSymbol);
                        return true;
                    }

                    break;
                }

            case ParameterSyntax parameter:
                {
                    var parameterSymbol = TryResolveFunctionExpressionParameterSymbolFast(parameter, out var fastFunctionParameter)
                        ? fastFunctionParameter
                        : parameter.Ancestors().OfType<FunctionExpressionSyntax>().Any()
                        ? null
                        : GetDeclaredSymbol(parameter);
                    if (parameterSymbol is not null)
                    {
                        info = new SymbolInfo(parameterSymbol);
                        StoreNodeInterestSymbolDescriptor(node, parameterSymbol);
                        return true;
                    }

                    break;
                }

            case VariableDeclaratorSyntax declarator:
                {
                    var localSymbol = GetDeclaredSymbol(declarator);
                    if (localSymbol is not null)
                    {
                        info = new SymbolInfo(localSymbol);
                        StoreNodeInterestSymbolDescriptor(node, localSymbol);
                        return true;
                    }

                    break;
                }

            case SingleVariableDesignationSyntax designation:
                {
                    var designatedSymbol = TryResolveAvailablePatternDesignationSymbol(designation, out var availableDesignatedSymbol, allowErrorType: true)
                        ? availableDesignatedSymbol
                        : GetDeclaredSymbol(designation);
                    if (designatedSymbol is not null)
                    {
                        info = new SymbolInfo(designatedSymbol);
                        StoreNodeInterestSymbolDescriptor(node, designatedSymbol);
                        return true;
                    }

                    break;
                }

            case FunctionExpressionSyntax functionExpression:
                {
                    if (TryGetFunctionExpressionSymbol(functionExpression, out var functionSymbol))
                    {
                        info = new SymbolInfo(functionSymbol);
                        StoreNodeInterestSymbolDescriptor(node, functionSymbol);
                        return true;
                    }

                    break;
                }
        }

        return false;
    }

    private bool TryResolveMemberAccessFromVisibleReceiver(
        MemberAccessExpressionSyntax memberAccess,
        IdentifierNameSyntax memberName,
        out SymbolInfo info)
    {
        info = default;

        var receiverSymbol = TryLookupVisibleValueSymbol(memberAccess.Expression);
        var receiverType = GetTypeFromSymbol(receiverSymbol);
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return false;

        var members = receiverType.GetMembers(memberName.Identifier.ValueText)
            .Where(static member => member is IFieldSymbol or IPropertySymbol or IEventSymbol or IMethodSymbol)
            .ToImmutableArray();

        if (members.IsDefaultOrEmpty)
            return false;

        var selected = members.Length == 1
            ? members[0]
            : members.FirstOrDefault(static member => member is IPropertySymbol or IFieldSymbol or IEventSymbol);

        info = selected is not null
            ? new SymbolInfo(selected, members)
            : new SymbolInfo(CandidateReason.MemberGroup, members);
        return true;
    }

    private bool TryGetCasePatternHeadSymbol(SimpleNameSyntax nameSyntax, out ISymbol symbol)
    {
        symbol = null!;

        SyntaxNode? patternNode = nameSyntax.Parent switch
        {
            NominalDeconstructionPatternSyntax nominal when ReferenceEquals(nominal.Type, nameSyntax) => nominal,
            DeclarationPatternSyntax declaration when ReferenceEquals(declaration.Type, nameSyntax) => declaration,
            ConstantPatternSyntax constant when ReferenceEquals(constant.Expression, nameSyntax) => constant,
            MemberPatternSyntax memberPattern when nameSyntax.Parent is QualifiedNameSyntax qualified &&
                                                 ReferenceEquals(qualified.Right, nameSyntax) &&
                                                 ReferenceEquals(memberPattern.Path, qualified) => memberPattern,
            MemberPatternSyntax memberPattern when ReferenceEquals(memberPattern.Path, nameSyntax) => memberPattern,
            _ => null
        };

        if (patternNode is null)
            return false;

        if (GetOperation(patternNode) is not ICasePatternOperation casePattern)
            return false;

        symbol = casePattern.CaseSymbol;
        return true;
    }

    private bool TryRebindInvocationAfterRefreshingFunctionArguments(
        InvocationExpressionSyntax invocation,
        out SymbolInfo info)
    {
        info = default;

        if (!InvocationContainsFunctionArguments(invocation))
            return false;

        var refreshedAnyFunction = false;
        foreach (var functionExpression in invocation.ArgumentList.Arguments
                     .SelectMany(static argument => argument.Expression.DescendantNodesAndSelf().OfType<FunctionExpressionSyntax>()))
        {
            refreshedAnyFunction |= TryGetFunctionExpressionSymbol(functionExpression, out _);
        }

        if (!refreshedAnyFunction)
            return false;

        if (TryGetContextualBindingRoot(invocation, out var contextualRoot) &&
            !ReferenceEquals(contextualRoot, invocation))
        {
            ClearCachedSemanticState(contextualRoot);
            var reboundRoot = GetBoundNode(contextualRoot, BoundTreeView.Original);
            if (TryFindBoundNodeBySyntax(reboundRoot, invocation, out var reboundNode) &&
                reboundNode is BoundExpression reboundExpression)
            {
                var reboundInfo = reboundExpression.GetSymbolInfo();
                if (reboundInfo.Symbol is not null || !reboundInfo.CandidateSymbols.IsDefaultOrEmpty)
                {
                    info = reboundInfo;
                    return true;
                }
            }
        }

        ClearCachedSemanticState(invocation);
        var refreshedInvocation = GetBoundNode(invocation);
        var refreshedInfo = refreshedInvocation.GetSymbolInfo();
        if (refreshedInfo.Symbol is not null || !refreshedInfo.CandidateSymbols.IsDefaultOrEmpty)
        {
            info = refreshedInfo;
            return true;
        }

        return false;
    }

    private bool TryRebindExpressionFromEnclosingFunctionContext(
        ExpressionSyntax expression,
        out SymbolInfo info)
    {
        info = default;

        if (!TryGetEnclosingFunctionExpression(expression, out var enclosingFunctionExpression))
            return false;

        var rebindRoot = GetFunctionExpressionRebindRoot(enclosingFunctionExpression);
        ClearCachedSemanticState(rebindRoot);

        var reboundRoot = GetBoundNode(rebindRoot, BoundTreeView.Original);
        if (!TryFindBoundNodeBySyntax(reboundRoot, expression, out var reboundNode) ||
            reboundNode is not BoundExpression reboundExpression)
        {
            return false;
        }

        var reboundInfo = reboundExpression.GetSymbolInfo();
        if (reboundInfo.Symbol is null && reboundInfo.CandidateSymbols.IsDefaultOrEmpty)
            return false;

        info = reboundInfo;
        return true;
    }

    private static bool InvocationContainsFunctionArguments(InvocationExpressionSyntax invocation)
    {
        foreach (var argument in invocation.ArgumentList.Arguments)
        {
            if (argument.Expression.DescendantNodesAndSelf().OfType<FunctionExpressionSyntax>().Any())
                return true;
        }

        return false;
    }

    private bool TryLookupPipelineInvocationSymbol(
        InvocationExpressionSyntax invocation,
        SyntaxNode contextNode,
        string methodName,
        out SymbolInfo info)
    {
        info = default;

        if (invocation.Parent is not InfixOperatorExpressionSyntax
            {
                OperatorToken.Kind: SyntaxKind.PipeToken
            } pipeExpression ||
            !IsSameSyntaxNode(pipeExpression.Right, invocation))
        {
            return false;
        }

        foreach (var functionExpression in invocation.ArgumentList.Arguments
                     .SelectMany(static argument => argument.Expression.DescendantNodesAndSelf().OfType<FunctionExpressionSyntax>()))
        {
            _ = TryGetFunctionExpressionSymbol(functionExpression, out _);
        }

        var receiverType = GetTypeInfo(pipeExpression.Left).Type
            ?? (GetBoundNode(pipeExpression.Left) as BoundExpression)?.Type;
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
        {
            return false;
        }

        var extensions = LookupApplicableExtensionMembers(receiverType, contextNode, methodName);
        var candidates = extensions.InstanceMethods
            .Concat(extensions.StaticMethods)
            .Where(method => string.Equals(method.Name, methodName, StringComparison.Ordinal))
            .Where(method => method.Parameters.Length == invocation.ArgumentList.Arguments.Count + 1)
            .ToImmutableArray();

        if (candidates.IsDefaultOrEmpty)
        {
            return false;
        }

        var selected = candidates
            .OrderByDescending(method => SymbolEqualityComparer.Default.Equals(method.Parameters[0].Type, receiverType))
            .ThenByDescending(method => method.Parameters[0].Type.TypeKind != TypeKind.Error &&
                                       Compilation.ClassifyConversion(receiverType, method.Parameters[0].Type, includeUserDefined: true).Exists)
            .ThenByDescending(static method => method.Parameters.Length > 1 &&
                                              IsExpressionTreeDelegateParameter(method.Parameters[1].Type))
            .FirstOrDefault();

        if (selected is null)
        {
            return false;
        }
        info = new SymbolInfo(selected);
        return true;
    }

    private static bool IsExpressionTreeDelegateParameter(ITypeSymbol type)
    {
        if (type is not INamedTypeSymbol named)
            return false;

        var definition = (named.OriginalDefinition as INamedTypeSymbol) ?? named;
        return string.Equals(definition.Name, "Expression", StringComparison.Ordinal) ||
               string.Equals(definition.MetadataName, "Expression`1", StringComparison.Ordinal);
    }

    private bool TryResolveNodeInterestSymbolDescriptor(
        Compilation.NodeInterestSymbolDescriptor descriptor,
        out ISymbol symbol)
    {
        var candidate = SyntaxTree.GetRoot().FindNode(descriptor.ReferencedSpan, getInnermostNodeForTie: true);
        for (var current = candidate; current is not null; current = current.Parent)
        {
            if (current.Kind != descriptor.ReferencedKind || current.Span != descriptor.ReferencedSpan)
                continue;

            symbol = current switch
            {
                ParameterSyntax parameter when parameter.Ancestors().OfType<FunctionExpressionSyntax>().Any()
                    => null!,
                ParameterSyntax parameter when TryResolveParameterSymbolFast(parameter, out var parameterSymbol)
                    => parameterSymbol!,
                VariableDeclaratorSyntax variableDeclarator when variableDeclarator.Initializer?.Value.DescendantNodesAndSelf().OfType<FunctionExpressionSyntax>().Any() == true
                    => null!,
                VariableDeclaratorSyntax variableDeclarator when TryResolveAvailableLocalSymbol(variableDeclarator, out var localSymbol, allowErrorType: true)
                    => localSymbol!,
                SingleVariableDesignationSyntax designation when TryResolveAvailablePatternDesignationSymbol(designation, out var designationSymbol, allowErrorType: true)
                    => designationSymbol!,
                FunctionExpressionSyntax functionExpression when TryGetFunctionExpressionSymbol(functionExpression, out var functionSymbol)
                    => functionSymbol!,
                SyntaxNode declaredNode when TryResolveAvailableDeclaredSymbol(declaredNode, out var declaredSymbol)
                    => declaredSymbol!,
                _ => GetDeclaredSymbol(current)!
            };

            if (symbol is not null)
                return true;
        }

        symbol = null!;
        return false;
    }

    private void StoreNodeInterestSymbolDescriptor(SyntaxNode queryNode, ISymbol symbol)
    {
        if (TryCreateNodeInterestSymbolDescriptor(symbol, out var descriptor))
        {
            Compilation.StoreNodeInterestSymbolDescriptor(queryNode, descriptor);
        }
    }

    private bool TryCreateNodeInterestSymbolDescriptor(
        ISymbol symbol,
        out Compilation.NodeInterestSymbolDescriptor descriptor)
    {
        var declarationReference = symbol.DeclaringSyntaxReferences.FirstOrDefault(reference => reference.SyntaxTree == SyntaxTree);
        if (declarationReference?.GetSyntax() is not SyntaxNode declarationNode)
        {
            descriptor = default;
            return false;
        }

        if (declarationNode is ParameterSyntax parameter &&
            parameter.Ancestors().OfType<FunctionExpressionSyntax>().Any())
        {
            descriptor = default;
            return false;
        }

        if (declarationNode is VariableDeclaratorSyntax variableDeclarator &&
            variableDeclarator.Initializer?.Value.DescendantNodesAndSelf().OfType<FunctionExpressionSyntax>().Any() == true)
        {
            descriptor = default;
            return false;
        }

        descriptor = new Compilation.NodeInterestSymbolDescriptor(declarationNode.Span, declarationNode.Kind);
        return true;
    }

    private static SymbolInfo ProjectBackingFieldSymbolsToAssociatedProperty(SyntaxNode node, SymbolInfo info)
    {
        if (!TryProjectSymbol(node, info.Symbol, out var projectedSymbol))
            return info;

        var candidatesChanged = false;
        ImmutableArray<ISymbol> projectedCandidates;

        if (info.CandidateSymbols.IsDefaultOrEmpty)
        {
            projectedCandidates = projectedSymbol is null
                ? ImmutableArray<ISymbol>.Empty
                : ImmutableArray.Create(projectedSymbol);
            candidatesChanged = true;
        }
        else
        {
            var builder = ImmutableArray.CreateBuilder<ISymbol>(info.CandidateSymbols.Length);

            foreach (var candidate in info.CandidateSymbols)
            {
                if (TryProjectSymbol(node, candidate, out var projectedCandidate))
                {
                    if (projectedCandidate is not null && !builder.Contains(projectedCandidate, SymbolEqualityComparer.Default))
                        builder.Add(projectedCandidate);
                    candidatesChanged = true;
                }
                else if (!builder.Contains(candidate, SymbolEqualityComparer.Default))
                {
                    builder.Add(candidate);
                }
            }

            projectedCandidates = builder.ToImmutable();
        }

        if (!candidatesChanged && SymbolEqualityComparer.Default.Equals(info.Symbol, projectedSymbol))
            return info;

        return new SymbolInfo(projectedSymbol, projectedCandidates, info.CandidateReason);
    }

    private static bool TryProjectSymbol(SyntaxNode node, ISymbol? symbol, out ISymbol? projected)
    {
        projected = symbol;

        if (node is IdentifierNameSyntax identifierName &&
            !IsExplicitMemberName(identifierName) &&
            TryGetPrimaryConstructorCaptureParameter(symbol, out var primaryParameter))
        {
            projected = primaryParameter;
            return true;
        }

        if (symbol is IMethodSymbol methodSymbol &&
            methodSymbol.AssociatedSymbol is { } associatedMemberSymbol &&
            associatedMemberSymbol is IPropertySymbol or IEventSymbol)
        {
            projected = associatedMemberSymbol;
            return true;
        }

        if (symbol is not IFieldSymbol fieldSymbol)
            return false;

        if (node is IdentifierNameSyntax identifier &&
            string.Equals(identifier.Identifier.ValueText, "field", StringComparison.Ordinal))
        {
            return false;
        }

        if (fieldSymbol.AssociatedSymbol is { } associatedSymbol &&
            associatedSymbol is IPropertySymbol or IEventSymbol)
        {
            projected = associatedSymbol;
            return true;
        }

        return false;
    }

    private static bool IsExplicitMemberName(IdentifierNameSyntax identifier)
    {
        return identifier.Parent is MemberAccessExpressionSyntax memberAccess &&
               ReferenceEquals(memberAccess.Name, identifier);
    }

    private static bool TryGetPrimaryConstructorCaptureParameter(ISymbol? symbol, out IParameterSymbol? parameter)
    {
        parameter = null;

        while (symbol is not null)
        {
            switch (symbol)
            {
                case SourceFieldSymbol sourceField when sourceField.Initializer is BoundParameterAccess parameterAccess:
                    parameter = parameterAccess.Parameter;
                    return true;
                case SourcePropertySymbol sourceProperty when sourceProperty.BackingField?.Initializer is BoundParameterAccess parameterAccess:
                    parameter = parameterAccess.Parameter;
                    return true;
            }

            var underlying = symbol.UnderlyingSymbol;
            if (ReferenceEquals(underlying, symbol))
                break;

            symbol = underlying;
        }

        return false;
    }

    /// <summary>
    /// Given a syntax node that declares a method, property, or member accessor, get the corresponding symbol.
    /// </summary>
    /// <param name="node"></param>
    /// <returns></returns>
    public ISymbol? GetDeclaredSymbol(SyntaxNode node)
    {
        if (RequiresCompleteSourceDeclarationSymbol(node))
            Compilation.EnsureSourceDeclarationsComplete();

        switch (node)
        {
            case SyntaxNode declaredNode when TryResolveAvailableDeclaredSymbol(declaredNode, out var declaredSymbol):
                StoreSymbolInfo(node, declaredSymbol);
                return declaredSymbol;

            case ParameterSyntax parameter
                when TryResolveFunctionExpressionParameterSymbolFast(parameter, out var functionParameterSymbol):
                StoreSymbolInfo(node, functionParameterSymbol);
                return functionParameterSymbol;

            case ParameterSyntax parameter
                when TryResolveParameterSymbolFast(parameter, out var parameterSymbol):
                StoreSymbolInfo(node, parameterSymbol);
                return parameterSymbol;

            case ParameterSyntax parameter
                when TryResolveAvailableParameterSymbol(parameter, out var availableParameterSymbol):
                StoreSymbolInfo(node, availableParameterSymbol);
                return availableParameterSymbol;

            case VariableDeclaratorSyntax variableDeclarator
                when TryGetStableLocalDeclarationSymbol(variableDeclarator, out var stableLocalSymbol):
                StoreSymbolInfo(node, stableLocalSymbol);
                return stableLocalSymbol;

            case VariableDeclaratorSyntax variableDeclarator
                when IsLocalVariableDeclarator(variableDeclarator) &&
                     TryResolveAvailableLocalSymbol(variableDeclarator, out var localSymbol, allowErrorType: true):
                StoreSymbolInfo(node, localSymbol);
                return localSymbol;

            case SingleVariableDesignationSyntax designation
                when TryResolveAvailablePatternDesignationSymbol(designation, out var designationSymbol, allowErrorType: true):
                StoreSymbolInfo(node, designationSymbol);
                return designationSymbol;
        }

        var symbol = _declaredSymbolLookup.Lookup(node);
        if (symbol is not null)
            StoreSymbolInfo(node, symbol);

        return symbol;
    }

    private static bool RequiresCompleteSourceDeclarationSymbol(SyntaxNode node)
        => node is TypeDeclarationSyntax
            or UnionDeclarationSyntax
            or CaseDeclarationSyntax
            or MethodDeclarationSyntax
            or ConstructorDeclarationSyntax
            or ParameterlessConstructorDeclarationSyntax
            or FunctionStatementSyntax
            or PropertyDeclarationSyntax
            or EventDeclarationSyntax
            or AccessorDeclarationSyntax
            or OperatorDeclarationSyntax
            or ConversionOperatorDeclarationSyntax
            or ParameterSyntax { Parent.Parent: not LocalDeclarationStatementSyntax };

    private bool TryResolveAvailableDeclaredSymbol(SyntaxNode node, out ISymbol? symbol)
    {
        switch (node)
        {
            case TypeDeclarationSyntax typeDeclaration when TryGetClassSymbol(typeDeclaration, out var typeSymbol):
                symbol = typeSymbol;
                return true;

            case UnionDeclarationSyntax unionDeclaration when TryGetUnionSymbol(unionDeclaration, out var unionSymbol):
                symbol = unionSymbol;
                return true;

            case CaseDeclarationSyntax caseDeclaration when TryGetUnionCaseSymbol(caseDeclaration, out var caseSymbol):
                symbol = caseSymbol;
                return true;

            case MethodDeclarationSyntax methodDeclaration when TryGetMethodSymbol(methodDeclaration, out var methodSymbol):
                symbol = methodSymbol;
                return true;

            case PropertyDeclarationSyntax propertyDeclaration when TryGetPropertySymbol(propertyDeclaration, out var propertySymbol):
                symbol = propertySymbol;
                return true;

            case EventDeclarationSyntax eventDeclaration when TryGetEventSymbol(eventDeclaration, out var eventSymbol):
                symbol = eventSymbol;
                return true;

            case AccessorDeclarationSyntax accessorDeclaration when TryResolveAvailableAccessorSymbol(accessorDeclaration, out var accessorSymbol):
                symbol = accessorSymbol;
                return true;

            default:
                symbol = null;
                return false;
        }
    }

    private bool TryResolveAvailableAccessorSymbol(AccessorDeclarationSyntax accessorDeclaration, out IMethodSymbol? accessorSymbol)
    {
        accessorSymbol = null;

        if (accessorDeclaration.Ancestors().OfType<PropertyDeclarationSyntax>().FirstOrDefault() is { } propertyDeclaration &&
            TryGetPropertySymbol(propertyDeclaration, out var propertySymbol))
        {
            accessorSymbol = accessorDeclaration.Kind == SyntaxKind.GetAccessorDeclaration ||
                             accessorDeclaration.Keyword.Kind == SyntaxKind.GetKeyword
                ? propertySymbol.GetMethod
                : propertySymbol.SetMethod;
            return accessorSymbol is not null;
        }

        if (accessorDeclaration.Ancestors().OfType<EventDeclarationSyntax>().FirstOrDefault() is { } eventDeclaration &&
            TryGetEventSymbol(eventDeclaration, out var eventSymbol))
        {
            accessorSymbol = accessorDeclaration.Kind == SyntaxKind.AddAccessorDeclaration ||
                             accessorDeclaration.Keyword.Kind == SyntaxKind.AddKeyword
                ? eventSymbol.AddMethod
                : eventSymbol.RemoveMethod;
            return accessorSymbol is not null;
        }

        return false;
    }

    private void StoreSymbolInfo(SyntaxNode node, ISymbol symbol)
    {
        _symbolMappings[node] = new SymbolInfo(symbol);
        StoreNodeInterestSymbolDescriptor(node, symbol);
    }

    internal bool TryGetStableLocalDeclarationSymbol(
        VariableDeclaratorSyntax variableDeclarator,
        out ILocalSymbol? localSymbol)
    {
        if (!IsLocalVariableDeclarator(variableDeclarator))
        {
            localSymbol = null;
            return false;
        }

        if (TryGetCachedBoundNode(variableDeclarator) is BoundVariableDeclarator cachedDeclarator &&
            !cachedDeclarator.Local.Type.ContainsErrorType())
        {
            localSymbol = cachedDeclarator.Local;
            return true;
        }

        if (TryBindLocalDeclarationForStableLocalSymbol(variableDeclarator, out localSymbol))
            return true;

        if (variableDeclarator.Initializer is null)
        {
            localSymbol = null;
            return false;
        }

        var interestRoot = GetInterestBindingRoot(variableDeclarator, includeExtendedExecutableRoots: true);
        if (interestRoot is null)
        {
            localSymbol = null;
            return false;
        }

        PrimeContextualFunctionExpressions(interestRoot);
        ClearCachedSemanticState(interestRoot);
        _ = GetBoundNode(interestRoot, BoundTreeView.Original);

        if (TryGetCachedBoundNode(variableDeclarator) is BoundVariableDeclarator reboundDeclarator &&
            !reboundDeclarator.Local.Type.ContainsErrorType())
        {
            localSymbol = reboundDeclarator.Local;
            return true;
        }

        localSymbol = null;
        return false;
    }

    internal bool TryGetAvailableLocalDeclarationSymbol(
        VariableDeclaratorSyntax variableDeclarator,
        out ILocalSymbol? localSymbol)
    {
        if (!IsLocalVariableDeclarator(variableDeclarator))
        {
            localSymbol = null;
            return false;
        }

        if (TryGetCachedBoundNode(variableDeclarator) is BoundVariableDeclarator cachedDeclarator &&
            !cachedDeclarator.Local.Type.ContainsErrorType())
        {
            localSymbol = cachedDeclarator.Local;
            return true;
        }

        if (TryResolveAvailableLocalSymbol(variableDeclarator, out localSymbol))
            return true;

        if (variableDeclarator.Initializer?.Value.DescendantNodesAndSelf().Any(static node => node.Kind == SyntaxKind.AwaitExpression) == true)
            return TryResolveAvailableLocalSymbol(variableDeclarator, out localSymbol, allowErrorType: true);

        localSymbol = null;
        return false;
    }

    private static bool IsLocalVariableDeclarator(VariableDeclaratorSyntax variableDeclarator)
        => variableDeclarator.Ancestors().OfType<LocalDeclarationStatementSyntax>().Any();

    private bool TryBindLocalDeclarationForStableLocalSymbol(
        VariableDeclaratorSyntax variableDeclarator,
        out ILocalSymbol? localSymbol)
    {
        if (variableDeclarator.Ancestors().OfType<GlobalStatementSyntax>().FirstOrDefault() is { } globalStatement)
            BindPrecedingGlobalStatementsForScope(SyntaxTree.GetRoot(), globalStatement);

        var bindingRoot =
            variableDeclarator.Ancestors().OfType<LocalDeclarationStatementSyntax>().FirstOrDefault()
            ?? (SyntaxNode?)variableDeclarator;

        if (bindingRoot is null)
        {
            localSymbol = null;
            return false;
        }

        _ = GetBinder(bindingRoot).GetOrBind(bindingRoot);

        if (TryGetCachedBoundNode(variableDeclarator) is BoundVariableDeclarator reboundDeclarator &&
            !reboundDeclarator.Local.Type.ContainsErrorType())
        {
            localSymbol = reboundDeclarator.Local;
            return true;
        }

        localSymbol = null;
        return false;
    }

    private void BindPrecedingGlobalStatementsForScope(SyntaxNode root, GlobalStatementSyntax owner)
    {
        foreach (var global in root.DescendantNodesAndSelf().OfType<GlobalStatementSyntax>())
        {
            if (!ReferenceEquals(global.Parent, owner.Parent))
                continue;

            if (global.Span.Start >= owner.Span.Start)
                break;

            GetBinder(global.Statement).GetOrBind(global.Statement);
        }
    }

    private void PrimeContextualFunctionExpressions(SyntaxNode root)
    {
        foreach (var parameter in root.DescendantNodes()
                     .OfType<ParameterSyntax>()
                     .Where(static parameter =>
                         parameter.Ancestors().OfType<FunctionExpressionSyntax>().Any() ||
                         parameter.Ancestors().OfType<TrailingBlockExpressionSyntax>().Any()))
        {
            _ = GetFunctionExpressionParameterSymbol(parameter);
        }
    }

    public MacroExpansionResult? GetMacroExpansion(
        AttributeSyntax attribute,
        CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(attribute);

        if (!attribute.IsMacroAttribute())
            return null;

        if (TryGetMacroTarget(attribute) is not { } targetDeclaration)
            return null;

        var expansionMap = _macroExpansionCache.GetOrAdd(
            targetDeclaration,
            static (syntax, state) => MacroExpansionService.ExpandAttachedMacros(
                state.Model.Compilation,
                state.Model,
                syntax,
                state.Model._declarationDiagnostics,
                state.CancellationToken),
            (Model: this, CancellationToken: cancellationToken));

        return expansionMap.TryGetValue(attribute, out var expansion)
            ? expansion
            : null;
    }

    public FreestandingMacroExpansionResult? GetMacroExpansion(
        FreestandingMacroExpressionSyntax expression,
        CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(expression);

        return _freestandingMacroExpansionCache.GetOrAdd(
            expression,
            static (syntax, state) => MacroExpansionService.ExpandFreestandingMacro(
                state.Model.Compilation,
                state.Model,
                syntax,
                state.Model._declarationDiagnostics,
                state.CancellationToken),
            (Model: this, CancellationToken: cancellationToken));
    }

    internal bool TryGetMacroReplacementSyntax(SyntaxNode node, out SyntaxNode replacement)
        => _macroReplacementSyntaxMap.TryGetValue(node, out replacement!);

    internal void RegisterMacroReplacementSyntax(SyntaxNode original, SyntaxNode replacement)
        => _macroReplacementSyntaxMap[original] = replacement;

    internal void RegisterMacroReplacementSyntaxTree(SyntaxNode originalRoot, SyntaxNode replacementRoot)
    {
        RegisterMacroReplacementSyntaxTrees(originalRoot, [replacementRoot]);
    }

    internal void RegisterMacroReplacementSyntaxTrees(SyntaxNode originalRoot, IEnumerable<SyntaxNode> replacementRoots)
    {
        ArgumentNullException.ThrowIfNull(originalRoot);
        ArgumentNullException.ThrowIfNull(replacementRoots);

        var replacementRootArray = replacementRoots
            .Where(static root => root is not null)
            .ToArray();

        if (replacementRootArray.Length == 0)
            return;

        RegisterMacroReplacementSyntax(originalRoot, replacementRootArray[0]);

        var replacementLookup = new Dictionary<object, Queue<SyntaxNode>>(ReferenceEqualityComparer.Instance);
        foreach (var replacementRoot in replacementRootArray)
        {
            foreach (var replacementNode in replacementRoot.DescendantNodesAndSelf())
            {
                if (!replacementLookup.TryGetValue(replacementNode.Green, out var matches))
                {
                    matches = new Queue<SyntaxNode>();
                    replacementLookup.Add(replacementNode.Green, matches);
                }

                matches.Enqueue(replacementNode);
            }
        }

        foreach (var originalNode in originalRoot.DescendantNodesAndSelf())
        {
            if (ReferenceEquals(originalNode, originalRoot))
                continue;

            if (!replacementLookup.TryGetValue(originalNode.Green, out var matches) || matches.Count == 0)
                continue;

            RegisterMacroReplacementSyntax(originalNode, matches.Dequeue());
        }
    }

    internal bool TryGetMacroContainingTypeSyntax(TypeDeclarationSyntax generatedType, out TypeDeclarationSyntax containingType)
        => _macroContainingTypeSyntaxMap.TryGetValue(generatedType, out containingType!);

    internal void RegisterMacroContainingTypeSyntax(TypeDeclarationSyntax generatedType, TypeDeclarationSyntax containingType)
        => _macroContainingTypeSyntaxMap[generatedType] = containingType;

    /// <summary>
    /// Gets type information about an expression.
    /// </summary>
    /// <param name="expr">The expression syntax node</param>
    /// <returns>The type info</returns>
    public TypeInfo GetTypeInfo(ExpressionSyntax expr)
    {
        Compilation.PerformanceInstrumentation.SemanticQuery.RecordTypeInfoQuery();

        TypeInfo Cache(TypeInfo info)
        {
            if (HasTypeInfo(info))
                _typeMappings[expr] = info;

            return info;
        }

        if (TryGetAvailableTypeInfo(expr, out var availableTypeInfo))
        {
            Compilation.PerformanceInstrumentation.SemanticQuery.RecordTypeInfoSymbolHit();
            return Cache(availableTypeInfo);
        }

        if (expr is FunctionExpressionSyntax functionExpression &&
            TryGetFunctionExpressionDelegateType(functionExpression, out var functionDelegateType) &&
            functionDelegateType is not null &&
            functionDelegateType.TypeKind != TypeKind.Error)
        {
            Compilation.PerformanceInstrumentation.SemanticQuery.RecordTypeInfoSymbolHit();
            return Cache(new TypeInfo(
                functionDelegateType,
                functionDelegateType,
                ComputeConversion(functionDelegateType, functionDelegateType)));
        }

        if (TryGetNodeInterestSymbolType(expr, out var nodeInterestType) &&
            nodeInterestType is not null &&
            nodeInterestType.TypeKind != TypeKind.Error)
        {
            Compilation.PerformanceInstrumentation.SemanticQuery.RecordTypeInfoSymbolHit();
            return Cache(new TypeInfo(nodeInterestType, nodeInterestType, ComputeConversion(nodeInterestType, nodeInterestType)));
        }

        if (expr is DefaultExpressionSyntax { Type: null } &&
            TryGetTargetTypeForExpression(expr, out var defaultTargetType) &&
            defaultTargetType is not null &&
            defaultTargetType.TypeKind != TypeKind.Error)
        {
            Compilation.PerformanceInstrumentation.SemanticQuery.RecordTypeInfoSymbolHit();
            return Cache(new TypeInfo(
                defaultTargetType,
                defaultTargetType,
                ComputeConversion(defaultTargetType, defaultTargetType)));
        }

        var symbolInfo = GetSymbolInfo(expr);
        var symbolType = GetTypeFromSymbol(symbolInfo.Symbol);
        if (symbolType is null && symbolInfo.Symbol is not null)
        {
            EnsureDeclarationInterestBound(symbolInfo.Symbol);
            symbolType = GetTypeFromSymbol(symbolInfo.Symbol);
        }
        if (symbolType is not null && symbolType.TypeKind != TypeKind.Error)
        {
            Compilation.PerformanceInstrumentation.SemanticQuery.RecordTypeInfoSymbolHit();
            return Cache(new TypeInfo(symbolType, symbolType, ComputeConversion(symbolType, symbolType)));
        }

        Compilation.PerformanceInstrumentation.SemanticQuery.RecordTypeInfoBoundFallback();
        var boundExpr = GetBoundNode(expr) as BoundExpression;

        if (boundExpr is null || (boundExpr.Type is null && boundExpr.GetConvertedType() is null))
        {
            if (TryBindInterestRegion(expr, out var regionBoundExpression))
                boundExpr = regionBoundExpression;

            // Prefer the node-local binding path first, but fall back to the
            // broader diagnostics walk when the immediate region does not yield
            // enough semantic information yet.
            if (boundExpr is null || (boundExpr.Type is null && boundExpr.GetConvertedType() is null))
            {
                Compilation.PerformanceInstrumentation.SemanticQuery.RecordTypeInfoDiagnosticFallback();
                EnsureDiagnosticBindingCompleted();
                boundExpr = GetBoundNode(expr) as BoundExpression;
            }
        }

        if (boundExpr is null)
            return new TypeInfo(null, null);

        ITypeSymbol? naturalType = boundExpr.Type;

        ITypeSymbol? convertedType = boundExpr.GetConvertedType() ?? boundExpr.Type;
        if (TryGetContextualArgumentConvertedType(expr, naturalType, out var contextualConvertedType))
            convertedType = contextualConvertedType;

        var conversion = boundExpr switch
        {
            BoundConversionExpression cast => cast.Conversion,
            BoundAsExpression asExpression => asExpression.Conversion,
            _ => ComputeConversion(naturalType, convertedType)
        };

        return Cache(new TypeInfo(naturalType, convertedType, conversion));
    }

    private bool TryGetContextualArgumentConvertedType(
        ExpressionSyntax expression,
        ITypeSymbol? naturalType,
        [System.Diagnostics.CodeAnalysis.NotNullWhen(true)] out ITypeSymbol? convertedType)
    {
        convertedType = null;

        if (naturalType is null ||
            naturalType.TypeKind == TypeKind.Error ||
            expression.Parent is not ArgumentSyntax argument ||
            !IsSameSyntaxNode(argument.Expression, expression) ||
            argument.Parent is not ArgumentListSyntax argumentList ||
            argumentList.Parent is not InvocationExpressionSyntax invocation ||
            IsSameSyntaxNode(invocation, expression))
        {
            return false;
        }

        if (!TryGetInvocationMethodSymbol(invocation, out var method) || method is null)
            return false;

        var parameter = GetParameterForArgument(method, argumentList.Arguments, argument);
        if (parameter?.Type is not { TypeKind: not TypeKind.Error } parameterType)
            return false;

        convertedType = ProjectContextualConvertedType(parameterType, naturalType);
        return true;
    }

    private bool TryGetArgumentParameterSymbolInfo(ArgumentSyntax argument, out SymbolInfo info)
    {
        info = SymbolInfo.None;

        if (argument.Parent is not ArgumentListSyntax argumentList ||
            argumentList.Parent is not InvocationExpressionSyntax invocation ||
            !TryGetInvocationMethodSymbol(invocation, out var method) ||
            method is null)
        {
            return false;
        }

        var parameter = GetParameterForArgument(method, argumentList.Arguments, argument);
        if (parameter is null)
            return false;

        info = new SymbolInfo(parameter);
        return true;
    }

    private bool TryGetInvocationMethodSymbol(
        InvocationExpressionSyntax invocation,
        [System.Diagnostics.CodeAnalysis.NotNullWhen(true)] out IMethodSymbol? method)
    {
        var invocationInfo = GetSymbolInfo(invocation);
        method = invocationInfo.Symbol as IMethodSymbol
            ?? invocationInfo.CandidateSymbols.OfType<IMethodSymbol>().FirstOrDefault();
        if (method is not null)
            return true;

        var boundInvocation = GetBoundNode(invocation);
        method = boundInvocation switch
        {
            BoundInvocationExpression invocationExpression => invocationExpression.Method,
            BoundObjectCreationExpression objectCreation => objectCreation.Constructor,
            _ => null
        };
        if (method is not null)
            return true;

        if (invocation.Expression is TypeSyntax typeSyntax &&
            GetTypeInfo(typeSyntax).Type is INamedTypeSymbol namedType)
        {
            method = namedType.Constructors.FirstOrDefault();
        }
        if (method is not null)
            return true;

        if (GetSymbolInfo(invocation.Expression).Symbol is INamedTypeSymbol expressionType)
            method = expressionType.Constructors.FirstOrDefault();

        return method is not null;
    }

    private static ITypeSymbol ProjectContextualConvertedType(ITypeSymbol parameterType, ITypeSymbol naturalType)
    {
        if (parameterType is not INamedTypeSymbol parameterNamed ||
            naturalType is not INamedTypeSymbol naturalNamed ||
            naturalNamed.TryGetUnionCase() is not { } naturalCase ||
            !TryProjectUnionFromCaseArguments(naturalNamed, naturalCase, out var projectedUnion) ||
            projectedUnion is null)
        {
            return parameterType;
        }

        var parameterDefinition = parameterNamed.OriginalDefinition as INamedTypeSymbol ?? parameterNamed;
        var projectedDefinition = projectedUnion.OriginalDefinition as INamedTypeSymbol ?? projectedUnion;
        if (SymbolEqualityComparer.Default.Equals(parameterDefinition, projectedDefinition))
            return projectedUnion;

        var parameterUnionDefinition = parameterNamed.TryGetUnion()?.OriginalDefinition as INamedTypeSymbol;
        if (parameterUnionDefinition is not null &&
            SymbolEqualityComparer.Default.Equals(parameterUnionDefinition, projectedDefinition))
        {
            return projectedUnion;
        }

        return parameterType;
    }

    private static bool TryProjectUnionFromCaseArguments(
        INamedTypeSymbol caseType,
        IUnionCaseTypeSymbol caseSymbol,
        [System.Diagnostics.CodeAnalysis.NotNullWhen(true)] out INamedTypeSymbol? projectedUnion)
    {
        projectedUnion = null;

        if (caseType.TypeArguments.IsDefaultOrEmpty)
            return false;

        var caseDefinition = caseSymbol.OriginalDefinition as IUnionCaseTypeSymbol ?? caseSymbol;
        if (caseDefinition is not INamedTypeSymbol caseDefinitionNamed ||
            caseDefinitionNamed.TypeParameters.IsDefaultOrEmpty ||
            caseDefinitionNamed.TypeParameters.Length != caseType.TypeArguments.Length)
        {
            return false;
        }

        var unionDefinition = caseDefinition.Union.OriginalDefinition as INamedTypeSymbol ?? caseDefinition.Union as INamedTypeSymbol;
        if (unionDefinition is null || unionDefinition.TypeParameters.IsDefaultOrEmpty)
            return false;

        var unionTypeArguments = unionDefinition.TypeParameters
            .Select(typeParameter => (ITypeSymbol)typeParameter)
            .ToArray();

        var changed = false;

        for (var i = 0; i < caseDefinitionNamed.TypeParameters.Length; i++)
        {
            var caseTypeParameter = caseDefinitionNamed.TypeParameters[i];
            ITypeParameterSymbol? unionTypeParameter = null;

            if (caseDefinition is SourceUnionCaseTypeSymbol sourceCaseDefinition &&
                sourceCaseDefinition.TryGetProjectedUnionTypeParameter(caseTypeParameter, out var mapped))
            {
                unionTypeParameter = mapped;
            }
            else
            {
                unionTypeParameter = unionDefinition.TypeParameters
                    .FirstOrDefault(tp => string.Equals(tp.Name, caseTypeParameter.Name, StringComparison.Ordinal));
            }

            if (unionTypeParameter is null)
                continue;

            var unionIndex = -1;
            for (var unionParameterIndex = 0; unionParameterIndex < unionDefinition.TypeParameters.Length; unionParameterIndex++)
            {
                if (SymbolEqualityComparer.Default.Equals(unionDefinition.TypeParameters[unionParameterIndex], unionTypeParameter))
                {
                    unionIndex = unionParameterIndex;
                    break;
                }
            }

            if (unionIndex < 0 || unionIndex >= unionTypeArguments.Length)
                continue;

            unionTypeArguments[unionIndex] = caseType.TypeArguments[i];
            changed = true;
        }

        if (!changed)
            return false;

        projectedUnion = (INamedTypeSymbol)unionDefinition.Construct(unionTypeArguments);
        return true;
    }

    private static IParameterSymbol? GetParameterForArgument(
        IMethodSymbol method,
        SeparatedSyntaxList<ArgumentSyntax> arguments,
        ArgumentSyntax argument)
    {
        if (argument.NameColon?.Name.Identifier.ValueText is { Length: > 0 } argumentName)
        {
            return method.Parameters.FirstOrDefault(parameter =>
                string.Equals(parameter.Name, argumentName, StringComparison.OrdinalIgnoreCase));
        }

        var argumentIndex = -1;
        for (var i = 0; i < arguments.Count; i++)
        {
            if (arguments[i] == argument)
            {
                argumentIndex = i;
                break;
            }
        }

        if (argumentIndex < 0)
            return null;

        if (argumentIndex < method.Parameters.Length)
            return method.Parameters[argumentIndex];

        var lastParameter = method.Parameters.LastOrDefault();
        return lastParameter?.IsVarParams == true ? lastParameter : null;
    }

    public bool TryGetTypeInfo(ExpressionSyntax expression, out TypeInfo typeInfo)
    {
        typeInfo = GetTypeInfo(expression);
        return HasTypeInfo(typeInfo);
    }

    private bool TryGetNodeInterestSymbolType(ExpressionSyntax expression, out ITypeSymbol? type)
    {
        if (TryLookupVisibleValueSymbol(expression) is { } visibleSymbol)
        {
            type = GetTypeFromSymbol(visibleSymbol);
            if (type is not null)
                return true;
        }

        if (TryGetNodeInterestSymbolInfo(expression, out var symbolInfo))
        {
            type = GetTypeFromSymbol(symbolInfo.Symbol);
            return type is not null;
        }

        type = null;
        return false;
    }

    internal bool TryGetFunctionExpressionDelegateType(
        FunctionExpressionSyntax functionExpression,
        out ITypeSymbol? delegateType)
    {
        if (TryGetCachedBoundNode(functionExpression) is BoundFunctionExpression cachedFunction &&
            !IsLikelyStaleFunctionBodyNode(cachedFunction) &&
            cachedFunction.DelegateType is not null)
        {
            delegateType = cachedFunction.DelegateType;
            return true;
        }

        if (TryGetAvailableFunctionExpressionDelegateType(functionExpression, out delegateType))
            return true;

        if (TryGetContextualBoundFunctionExpression(functionExpression, out var contextualFunction) &&
            contextualFunction.DelegateType is not null &&
            contextualFunction.DelegateType.TypeKind != TypeKind.Error)
        {
            delegateType = contextualFunction.DelegateType;
            return true;
        }

        delegateType = null;
        return false;
    }

    private bool TryGetAvailableFunctionExpressionDelegateType(
        FunctionExpressionSyntax functionExpression,
        out ITypeSymbol? delegateType)
    {
        delegateType = null;

        if (functionExpression.Parent is not ArgumentSyntax argument ||
            argument.Parent is not ArgumentListSyntax argumentList ||
            argumentList.Parent is not InvocationExpressionSyntax invocation)
        {
            return false;
        }

        var argumentIndex = 0;
        foreach (var current in argumentList.Arguments)
        {
            if (current.Span == argument.Span && current.Kind == argument.Kind)
                break;

            argumentIndex++;
        }

        if (invocation.Parent is InfixOperatorExpressionSyntax
            {
                OperatorToken.Kind: SyntaxKind.PipeToken
            } pipeExpression &&
            IsSameSyntaxNode(pipeExpression.Right, invocation) &&
            TryGetAvailableTypeInfo(pipeExpression.Left, out var pipeReceiverTypeInfo) &&
            (pipeReceiverTypeInfo.Type ?? pipeReceiverTypeInfo.ConvertedType) is { TypeKind: not TypeKind.Error } pipeReceiverType &&
            TryGetAvailablePipeInvocationCandidates(invocation, out var pipeMethods) &&
            TryGetDelegateTypeFromCandidateParameter(pipeMethods, argumentIndex + 1, pipeReceiverType, out delegateType))
        {
            return true;
        }

        if (invocation.Expression is MemberAccessExpressionSyntax memberAccess &&
            TryGetAvailableTypeInfo(memberAccess.Expression, out var extensionReceiverTypeInfo) &&
            (extensionReceiverTypeInfo.Type ?? extensionReceiverTypeInfo.ConvertedType) is { TypeKind: not TypeKind.Error } extensionReceiverType &&
            TryGetAvailableExtensionInvocationCandidates(invocation, out var extensionMethods) &&
            TryGetDelegateTypeFromCandidateParameter(extensionMethods, argumentIndex + 1, extensionReceiverType, out delegateType))
        {
            return true;
        }

        return TryGetAvailableInvocationCandidates(invocation, out var methods) &&
               TryGetDelegateTypeFromCandidateParameter(methods, argumentIndex, receiverType: null, out delegateType);
    }

    private static bool TryGetDelegateTypeFromCandidateParameter(
        ImmutableArray<IMethodSymbol> methods,
        int parameterIndex,
        ITypeSymbol? receiverType,
        out ITypeSymbol? delegateType)
    {
        delegateType = null;

        if (methods.IsDefaultOrEmpty)
            return false;

        foreach (var method in methods)
        {
            if (parameterIndex < 0 || parameterIndex >= method.Parameters.Length)
                continue;

            var parameterType = method.Parameters[parameterIndex].Type;
            if (parameterType is null || parameterType.ContainsErrorType())
                continue;

            if (receiverType is not null &&
                method.Parameters.Length > 0 &&
                TryInferExtensionReceiverSubstitutions(method.Parameters[0].Type, receiverType, out var substitutions))
            {
                parameterType = SubstituteTypeParameters(parameterType, substitutions);
            }

            if (TryUnwrapCallableDelegateType(parameterType, out var candidateDelegate))
            {
                delegateType = candidateDelegate;
                return true;
            }
        }

        return false;
    }

    private static bool TryInferExtensionReceiverSubstitutions(
        ITypeSymbol parameterType,
        ITypeSymbol receiverType,
        out Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions)
    {
        substitutions = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);
        return TryUnifyExtensionReceiver(parameterType, receiverType, substitutions);
    }

    private static bool TryUnifyExtensionReceiver(
        ITypeSymbol parameterType,
        ITypeSymbol receiverType,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions)
    {
        if (parameterType is ITypeParameterSymbol parameter)
        {
            substitutions[parameter] = receiverType;
            return true;
        }

        if (parameterType is IArrayTypeSymbol parameterArray &&
            receiverType is IArrayTypeSymbol receiverArray)
        {
            return TryUnifyExtensionReceiver(parameterArray.ElementType, receiverArray.ElementType, substitutions);
        }

        if (parameterType is not INamedTypeSymbol parameterNamed ||
            receiverType is not INamedTypeSymbol receiverNamed)
        {
            return SymbolEqualityComparer.Default.Equals(parameterType, receiverType);
        }

        if (TryUnifyNamedTypes(parameterNamed, receiverNamed, substitutions))
            return true;

        foreach (var receiverInterface in receiverNamed.AllInterfaces)
        {
            if (TryUnifyNamedTypes(parameterNamed, receiverInterface, substitutions))
                return true;
        }

        for (var baseType = receiverNamed.BaseType; baseType is not null; baseType = baseType.BaseType)
        {
            if (TryUnifyNamedTypes(parameterNamed, baseType, substitutions))
                return true;
        }

        return false;
    }

    private static bool TryUnifyNamedTypes(
        INamedTypeSymbol parameterType,
        INamedTypeSymbol receiverType,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions)
    {
        var parameterDefinition = TypeSubstitution.GetDefinitionForSubstitution(parameterType);
        var receiverDefinition = TypeSubstitution.GetDefinitionForSubstitution(receiverType);

        if (!SymbolEqualityComparer.Default.Equals(parameterDefinition, receiverDefinition))
            return false;

        var parameterArguments = TypeSubstitution.GetShallowTypeArguments(parameterType);
        var receiverArguments = TypeSubstitution.GetShallowTypeArguments(receiverType);

        if (parameterArguments.IsDefault)
            parameterArguments = ImmutableArray<ITypeSymbol>.Empty;

        if (receiverArguments.IsDefault)
            receiverArguments = ImmutableArray<ITypeSymbol>.Empty;

        if (parameterArguments.Length != receiverArguments.Length)
            return false;

        for (var i = 0; i < parameterArguments.Length; i++)
        {
            if (!TryUnifyExtensionReceiver(parameterArguments[i], receiverArguments[i], substitutions))
                return false;
        }

        return true;
    }

    private static ITypeSymbol SubstituteTypeParameters(
        ITypeSymbol type,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions)
    {
        if (type is ITypeParameterSymbol parameter &&
            substitutions.TryGetValue(parameter, out var replacement))
        {
            return replacement;
        }

        if (type is NullableTypeSymbol nullableType)
        {
            var substituted = SubstituteTypeParameters(nullableType.UnderlyingType, substitutions);
            return SymbolEqualityComparer.Default.Equals(substituted, nullableType.UnderlyingType)
                ? type
                : substituted.MakeNullable();
        }

        if (type is IArrayTypeSymbol arrayType)
        {
            var substituted = SubstituteTypeParameters(arrayType.ElementType, substitutions);
            return SymbolEqualityComparer.Default.Equals(substituted, arrayType.ElementType)
                ? type
                : new ArrayTypeSymbol(arrayType.BaseType, substituted, arrayType.ContainingSymbol, arrayType.ContainingType, arrayType.ContainingNamespace, [], arrayType.Rank, arrayType.FixedLength);
        }

        if (type is INamedTypeSymbol namedType)
        {
            var typeArguments = TypeSubstitution.GetShallowTypeArguments(namedType);
            if (typeArguments.IsDefaultOrEmpty)
                return type;

            var substituted = new ITypeSymbol[typeArguments.Length];
            var changed = false;

            for (var i = 0; i < typeArguments.Length; i++)
            {
                substituted[i] = SubstituteTypeParameters(typeArguments[i], substitutions);
                changed |= !SymbolEqualityComparer.Default.Equals(substituted[i], typeArguments[i]);
            }

            if (changed)
            {
                var definition = TypeSubstitution.GetDefinitionForSubstitution(namedType);
                return definition.Construct(substituted);
            }
        }

        return type;
    }

    private static bool ContainsTypeParameter(ITypeSymbol? type)
    {
        if (type is null)
            return false;

        if (type is ITypeParameterSymbol)
            return true;

        if (type is NullableTypeSymbol nullableType)
            return ContainsTypeParameter(nullableType.UnderlyingType);

        if (type is IArrayTypeSymbol arrayType)
            return ContainsTypeParameter(arrayType.ElementType);

        if (type is INamedTypeSymbol namedType)
        {
            var typeArguments = TypeSubstitution.GetShallowTypeArguments(namedType);
            return !typeArguments.IsDefaultOrEmpty && typeArguments.Any(ContainsTypeParameter);
        }

        return false;
    }

    private static bool TryUnwrapCallableDelegateType(ITypeSymbol type, out INamedTypeSymbol delegateType)
    {
        delegateType = null!;

        static ITypeSymbol Unalias(ITypeSymbol symbol)
        {
            while (symbol.IsAlias && symbol.UnderlyingSymbol is ITypeSymbol underlying)
                symbol = underlying;

            return symbol;
        }

        type = Unalias(type);

        if (type is NullableTypeSymbol nullable)
            type = nullable.UnderlyingType;

        if (type is INamedTypeSymbol { TypeKind: TypeKind.Delegate } directDelegate)
        {
            delegateType = directDelegate;
            return true;
        }

        if (type is not INamedTypeSymbol named)
            return false;

        var definition = (named.OriginalDefinition as INamedTypeSymbol) ?? named;
        if (definition.Arity != 1)
            return false;

        var isExpressionType =
            string.Equals(definition.Name, "Expression", StringComparison.Ordinal) ||
            string.Equals(definition.MetadataName, "Expression`1", StringComparison.Ordinal);
        if (!isExpressionType || named.TypeArguments.Length != 1)
            return false;

        var candidate = Unalias(named.TypeArguments[0]);
        if (candidate is not INamedTypeSymbol expressionDelegate)
            return false;

        if (expressionDelegate.TypeKind != TypeKind.Delegate &&
            expressionDelegate.GetDelegateInvokeMethod() is null)
        {
            return false;
        }

        delegateType = expressionDelegate;
        return true;
    }

    internal bool TryGetFunctionExpressionSymbol(
        FunctionExpressionSyntax functionExpression,
        out IMethodSymbol? functionSymbol)
    {
        if (_functionExpressionSymbolCache.TryGetValue(functionExpression, out var cachedSymbol))
        {
            if (!FunctionExpressionSymbolContainsError(cachedSymbol))
            {
                functionSymbol = cachedSymbol;
                return true;
            }

            if (TryGetUpgradedFunctionExpressionSymbol(functionExpression, out var upgradedSymbol))
            {
                functionSymbol = _functionExpressionSymbolCache.AddOrUpdate(
                    functionExpression,
                    upgradedSymbol,
                    (_, _) => upgradedSymbol);
                return true;
            }

            functionSymbol = cachedSymbol;
            return true;
        }

        if (_functionExpressionSymbolCreationInProgress.TryAdd(functionExpression, 0))
        {
            try
            {
                if (TryCreateShallowFunctionExpressionSymbol(functionExpression, out var shallowSymbol))
                {
                    functionSymbol = _functionExpressionSymbolCache.GetOrAdd(functionExpression, shallowSymbol);
                    return true;
                }
            }
            finally
            {
                _functionExpressionSymbolCreationInProgress.TryRemove(functionExpression, out _);
            }
        }

        if (TryGetUpgradedFunctionExpressionSymbol(functionExpression, out var functionExpressionMethod))
        {
            functionSymbol = functionExpressionMethod;
            return true;
        }

        functionSymbol = null;
        return false;
    }

    private bool TryGetUpgradedFunctionExpressionSymbol(
        FunctionExpressionSyntax functionExpression,
        out IMethodSymbol? functionSymbol)
    {
        if (TryGetCachedBoundNode(functionExpression) is BoundFunctionExpression cachedFunction &&
            !IsLikelyStaleFunctionBodyNode(cachedFunction) &&
            TryUpgradeFunctionExpressionSymbolFromBoundFunction(functionExpression, cachedFunction, out var cachedMethod))
        {
            functionSymbol = cachedMethod;
            return true;
        }

        if (TryGetContextualBoundFunctionExpression(functionExpression, out var contextualFunction) &&
            TryUpgradeFunctionExpressionSymbolFromBoundFunction(functionExpression, contextualFunction, out var contextualMethod))
        {
            functionSymbol = contextualMethod;
            return true;
        }

        functionSymbol = null;
        return false;
    }

    private static bool FunctionExpressionSymbolContainsError(IMethodSymbol method)
    {
        if (method.ReturnType.ContainsErrorType())
            return true;

        foreach (var parameter in method.Parameters)
        {
            if (parameter.Type.ContainsErrorType())
                return true;
        }

        return false;
    }

    private bool TryCreateShallowFunctionExpressionSymbol(
        FunctionExpressionSyntax functionExpression,
        out IMethodSymbol? functionSymbol)
    {
        var containingSymbol = TryGetAvailableContainingSymbol(functionExpression) ?? Compilation.GlobalNamespace;

        var containingType = containingSymbol.ContainingType as INamedTypeSymbol;
        var containingNamespace = containingSymbol.ContainingNamespace;
        var delegateInvokeMethod =
            TryGetAvailableOrCachedFunctionExpressionDelegateType(functionExpression, out var delegateType) &&
            delegateType is INamedTypeSymbol namedDelegateType
                ? namedDelegateType.GetDelegateInvokeMethod()
                : null;
        var isAsync = functionExpression switch
        {
            SimpleFunctionExpressionSyntax simple => simple.AsyncKeyword.Kind == SyntaxKind.AsyncKeyword,
            ParenthesizedFunctionExpressionSyntax parenthesized => parenthesized.AsyncKeyword.Kind == SyntaxKind.AsyncKeyword,
            _ => false
        };

        var defaultReturnType = isAsync
            ? Compilation.GetSpecialType(SpecialType.System_Threading_Tasks_Task)
            : Compilation.GetSpecialType(SpecialType.System_Unit);

        var annotatedReturnTypeSyntax = functionExpression switch
        {
            SimpleFunctionExpressionSyntax simple => simple.ReturnType?.Type,
            ParenthesizedFunctionExpressionSyntax parenthesized => parenthesized.ReturnType?.Type,
            _ => null
        };

        ITypeSymbol returnType;
        if (TryResolveShallowFunctionExpressionType(annotatedReturnTypeSyntax, out var resolvedReturnType))
        {
            returnType = resolvedReturnType;
        }
        else if (delegateInvokeMethod?.ReturnType is { TypeKind: not TypeKind.Error } delegateReturnType)
        {
            returnType = delegateReturnType;
        }
        else
        {
            returnType = defaultReturnType;
        }

        var lambdaSymbol = new SourceLambdaSymbol(
            parameters: [],
            returnType,
            containingSymbol,
            containingType,
            containingNamespace,
            [functionExpression.GetLocation()],
            [functionExpression.GetReference()],
            isAsync: isAsync);

        var parameters = functionExpression switch
        {
            SimpleFunctionExpressionSyntax simple when simple.Parameter is not null
                => [CreateShallowFunctionExpressionParameter(lambdaSymbol, simple.Parameter, 0, delegateInvokeMethod)],
            ParenthesizedFunctionExpressionSyntax parenthesized when parenthesized.ParameterList is not null
                => parenthesized.ParameterList.Parameters
                    .Select((parameter, index) => CreateShallowFunctionExpressionParameter(lambdaSymbol, parameter, index, delegateInvokeMethod))
                    .ToArray(),
            _ => []
        };

        lambdaSymbol.SetParameters(parameters);
        functionSymbol = lambdaSymbol;
        return true;
    }

    private bool TryGetContainingExecutableOwnerSymbol(
        SyntaxNode node,
        out ISymbol? symbol)
    {
        for (var current = node.Parent; current is not null; current = current.Parent)
        {
            switch (current)
            {
                case FunctionExpressionSyntax functionExpression:
                    if (TryGetFunctionExpressionSymbol(functionExpression, out var functionSymbol))
                    {
                        symbol = functionSymbol;
                        return true;
                    }

                    symbol = null;
                    return false;

                case BaseConstructorDeclarationSyntax:
                case BaseMethodDeclarationSyntax:
                case ParameterlessConstructorDeclarationSyntax:
                case PropertyDeclarationSyntax:
                case EventDeclarationSyntax:
                case AccessorDeclarationSyntax:
                case GlobalStatementSyntax:
                    return TryGetShallowDeclaredExecutableOwnerSymbol(current, out symbol);

                case CompilationUnitSyntax:
                    symbol = Compilation.GlobalNamespace;
                    return true;
            }
        }

        symbol = null;
        return false;
    }

    private bool TryGetExecutableOwnerSymbol(
        SyntaxNode node,
        out ISymbol? symbol)
    {
        if (!TryGetExecutableOwner(node, out var owner))
        {
            symbol = null;
            return false;
        }

        switch (owner)
        {
            case FunctionExpressionSyntax functionExpression:
                if (TryGetFunctionExpressionSymbol(functionExpression, out var functionSymbol))
                {
                    symbol = functionSymbol;
                    return true;
                }

                symbol = null;
                return false;

            case BaseConstructorDeclarationSyntax:
            case BaseMethodDeclarationSyntax:
            case ParameterlessConstructorDeclarationSyntax:
            case PropertyDeclarationSyntax:
            case EventDeclarationSyntax:
            case AccessorDeclarationSyntax:
            case GlobalStatementSyntax:
                return TryGetShallowDeclaredExecutableOwnerSymbol(owner, out symbol);

            case CompilationUnitSyntax:
                symbol = Compilation.GlobalNamespace;
                return true;
        }

        symbol = null;
        return false;
    }

    private bool TryGetShallowDeclaredExecutableOwnerSymbol(
        SyntaxNode owner,
        out ISymbol? symbol)
    {
        switch (owner)
        {
            case MethodDeclarationSyntax methodDeclaration when
                TryResolveMethodSymbolForDeclaration(methodDeclaration, out var methodSymbol):
                symbol = methodSymbol;
                return true;

            case BaseConstructorDeclarationSyntax constructorDeclaration when
                TryResolveShallowConstructorSymbol(constructorDeclaration, out var constructorSymbol):
                symbol = constructorSymbol;
                return true;

            case ParameterlessConstructorDeclarationSyntax parameterlessConstructor when
                TryResolveShallowParameterlessConstructorSymbol(parameterlessConstructor, out var parameterlessConstructorSymbol):
                symbol = parameterlessConstructorSymbol;
                return true;

            case PropertyDeclarationSyntax propertyDeclaration when
                TryResolveShallowPropertySymbol(propertyDeclaration, out var propertySymbol):
                symbol = propertySymbol;
                return true;

            case EventDeclarationSyntax eventDeclaration when
                TryResolveShallowEventSymbol(eventDeclaration, out var eventSymbol):
                symbol = eventSymbol;
                return true;

            case AccessorDeclarationSyntax accessorDeclaration when
                TryResolveShallowAccessorSymbol(accessorDeclaration, out var accessorSymbol):
                symbol = accessorSymbol;
                return true;

            case GlobalStatementSyntax:
                symbol = Compilation.GlobalNamespace;
                return true;
        }

        symbol = null;
        return false;
    }

    private bool TryResolveShallowConstructorSymbol(
        BaseConstructorDeclarationSyntax constructorDeclaration,
        out IMethodSymbol? constructorSymbol)
    {
        constructorSymbol = null;

        if (constructorDeclaration.Parent is not TypeDeclarationSyntax containingTypeSyntax ||
            !TryGetClassSymbol(containingTypeSyntax, out var containingType))
        {
            return false;
        }

        var parameterCount = constructorDeclaration.ParameterList?.Parameters.Count ?? 0;
        var targetTree = constructorDeclaration.SyntaxTree;
        var targetSpan = constructorDeclaration.Span;

        constructorSymbol = containingType
            .GetMembers(".ctor")
            .OfType<IMethodSymbol>()
            .FirstOrDefault(method =>
                method.Parameters.Length == parameterCount &&
                method.DeclaringSyntaxReferences.Any(reference =>
                    reference.SyntaxTree == targetTree &&
                    reference.Span == targetSpan))
            ?? containingType
                .GetMembers(".ctor")
                .OfType<IMethodSymbol>()
                .FirstOrDefault(method => method.Parameters.Length == parameterCount);

        return constructorSymbol is not null;
    }

    private bool TryResolveShallowParameterlessConstructorSymbol(
        ParameterlessConstructorDeclarationSyntax constructorDeclaration,
        out IMethodSymbol? constructorSymbol)
    {
        constructorSymbol = null;

        if (constructorDeclaration.Parent is not TypeDeclarationSyntax containingTypeSyntax ||
            !TryGetClassSymbol(containingTypeSyntax, out var containingType))
        {
            return false;
        }

        var targetTree = constructorDeclaration.SyntaxTree;
        var targetSpan = constructorDeclaration.Span;

        constructorSymbol = containingType
            .GetMembers(".ctor")
            .OfType<IMethodSymbol>()
            .FirstOrDefault(method =>
                method.Parameters.Length == 0 &&
                method.DeclaringSyntaxReferences.Any(reference =>
                    reference.SyntaxTree == targetTree &&
                    reference.Span == targetSpan))
            ?? containingType
                .GetMembers(".ctor")
                .OfType<IMethodSymbol>()
                .FirstOrDefault(method => method.Parameters.Length == 0);

        return constructorSymbol is not null;
    }

    private bool TryResolveShallowPropertySymbol(
        PropertyDeclarationSyntax propertyDeclaration,
        out IPropertySymbol? propertySymbol)
    {
        propertySymbol = null;

        if (propertyDeclaration.Parent is not TypeDeclarationSyntax containingTypeSyntax ||
            !TryGetClassSymbol(containingTypeSyntax, out var containingType))
        {
            return false;
        }

        var identifierToken = propertyDeclaration.ExplicitInterfaceSpecifier is null
            ? propertyDeclaration.Identifier
            : propertyDeclaration.ExplicitInterfaceSpecifier.Identifier;

        var targetTree = propertyDeclaration.SyntaxTree;
        var targetSpan = propertyDeclaration.Span;

        propertySymbol = containingType
            .GetMembers(identifierToken.ValueText)
            .OfType<IPropertySymbol>()
            .FirstOrDefault(property =>
                property.DeclaringSyntaxReferences.Any(reference =>
                    reference.SyntaxTree == targetTree &&
                    reference.Span == targetSpan))
            ?? containingType
                .GetMembers(identifierToken.ValueText)
                .OfType<IPropertySymbol>()
                .FirstOrDefault();

        return propertySymbol is not null;
    }

    private bool TryResolveShallowEventSymbol(
        EventDeclarationSyntax eventDeclaration,
        out IEventSymbol? eventSymbol)
    {
        eventSymbol = null;

        if (eventDeclaration.Parent is not TypeDeclarationSyntax containingTypeSyntax ||
            !TryGetClassSymbol(containingTypeSyntax, out var containingType))
        {
            return false;
        }

        var identifierToken = eventDeclaration.ExplicitInterfaceSpecifier is null
            ? eventDeclaration.Identifier
            : eventDeclaration.ExplicitInterfaceSpecifier.Identifier;

        var targetTree = eventDeclaration.SyntaxTree;
        var targetSpan = eventDeclaration.Span;

        eventSymbol = containingType
            .GetMembers(identifierToken.ValueText)
            .OfType<IEventSymbol>()
            .FirstOrDefault(@event =>
                @event.DeclaringSyntaxReferences.Any(reference =>
                    reference.SyntaxTree == targetTree &&
                    reference.Span == targetSpan))
            ?? containingType
                .GetMembers(identifierToken.ValueText)
                .OfType<IEventSymbol>()
                .FirstOrDefault();

        return eventSymbol is not null;
    }

    private bool TryResolveShallowAccessorSymbol(
        AccessorDeclarationSyntax accessorDeclaration,
        out IMethodSymbol? accessorSymbol)
    {
        accessorSymbol = null;

        if (accessorDeclaration.Parent is PropertyDeclarationSyntax propertyDeclaration &&
            TryResolveShallowPropertySymbol(propertyDeclaration, out var propertySymbol))
        {
            accessorSymbol = accessorDeclaration.Keyword.Kind switch
            {
                SyntaxKind.GetKeyword => propertySymbol.GetMethod,
                SyntaxKind.SetKeyword => propertySymbol.SetMethod,
                _ => null
            };

            return accessorSymbol is not null;
        }

        if (accessorDeclaration.Parent is EventDeclarationSyntax eventDeclaration &&
            TryResolveShallowEventSymbol(eventDeclaration, out var eventSymbol))
        {
            accessorSymbol = accessorDeclaration.Keyword.Kind switch
            {
                SyntaxKind.AddKeyword => eventSymbol.AddMethod,
                SyntaxKind.RemoveKeyword => eventSymbol.RemoveMethod,
                _ => null
            };

            return accessorSymbol is not null;
        }

        return false;
    }

    private SourceParameterSymbol CreateShallowFunctionExpressionParameter(
        SourceLambdaSymbol lambdaSymbol,
        ParameterSyntax parameterSyntax,
        int parameterIndex = -1,
        IMethodSymbol? delegateInvokeMethod = null)
    {
        ITypeSymbol parameterType;
        RefKind refKind;
        bool hasExplicitDefaultValue;
        object? explicitDefaultValue;
        bool isVarParams;

        if (TryResolveShallowFunctionExpressionType(parameterSyntax.TypeAnnotation?.Type, out var resolvedType))
        {
            parameterType = resolvedType;
            refKind = parameterSyntax.RefKindKeyword.Kind switch
            {
                SyntaxKind.RefKeyword => RefKind.Ref,
                SyntaxKind.OutKeyword => RefKind.Out,
                SyntaxKind.InKeyword => RefKind.In,
                _ => RefKind.None
            };
            hasExplicitDefaultValue = false;
            explicitDefaultValue = null;
            isVarParams = false;
        }
        else if (delegateInvokeMethod is not null &&
                 parameterIndex >= 0 &&
                 parameterIndex < delegateInvokeMethod.Parameters.Length &&
                 delegateInvokeMethod.Parameters[parameterIndex] is { } delegateParameter &&
                 delegateParameter.Type is { TypeKind: not TypeKind.Error } delegateParameterType)
        {
            parameterType = delegateParameterType;
            refKind = delegateParameter.RefKind;
            hasExplicitDefaultValue = delegateParameter.HasExplicitDefaultValue;
            explicitDefaultValue = delegateParameter.ExplicitDefaultValue;
            isVarParams = delegateParameter.IsVarParams;
        }
        else
        {
            parameterType = Compilation.ErrorTypeSymbol;
            refKind = parameterSyntax.RefKindKeyword.Kind switch
            {
                SyntaxKind.RefKeyword => RefKind.Ref,
                SyntaxKind.OutKeyword => RefKind.Out,
                SyntaxKind.InKeyword => RefKind.In,
                _ => RefKind.None
            };
            hasExplicitDefaultValue = false;
            explicitDefaultValue = null;
            isVarParams = false;
        }

        return new SourceParameterSymbol(
            parameterSyntax.Identifier.ValueText,
            parameterType,
            lambdaSymbol,
            lambdaSymbol.ContainingType as INamedTypeSymbol,
            lambdaSymbol.ContainingNamespace,
            [parameterSyntax.GetLocation()],
            [parameterSyntax.GetReference()],
            refKind,
            hasExplicitDefaultValue,
            explicitDefaultValue,
            parameterSyntax.BindingKeyword.Kind == SyntaxKind.VarKeyword,
            isVarParams);
    }

    private bool TryResolveShallowFunctionExpressionType(
        TypeSyntax? typeSyntax,
        out ITypeSymbol resolvedType)
    {
        resolvedType = Compilation.ErrorTypeSymbol;
        if (typeSyntax is null)
            return false;

        var typeInfo = GetTypeInfo(typeSyntax);
        var type = typeInfo.Type ?? typeInfo.ConvertedType;
        if (type is not null && type.TypeKind != TypeKind.Error)
        {
            resolvedType = type;
            return true;
        }

        var symbol = GetBinder(typeSyntax).BindReferencedSymbol(typeSyntax).Symbol;
        type = symbol switch
        {
            ITypeSymbol typeSymbol => typeSymbol,
            IAliasSymbol { UnderlyingSymbol: ITypeSymbol aliasedType } => aliasedType,
            _ => null
        };

        if (type is null || type.TypeKind == TypeKind.Error)
            return false;

        resolvedType = type;
        return true;
    }

    internal ISymbol? TryLookupVisibleValueSymbol(ExpressionSyntax expression)
        => expression is IdentifierNameSyntax identifier
            ? TryLookupVisibleValueSymbol(expression, identifier.Identifier.ValueText)
            : null;

    internal ImmutableArray<ISymbol> GetVisibleValueSymbols(SyntaxNode contextNode)
    {
        var position = contextNode.Span.Start;
        var builder = ImmutableArray.CreateBuilder<ISymbol>();
        var seenDeclarations = new HashSet<SyntaxNode>();

        foreach (var scopeNode in EnumerateVisibleValueScopes(contextNode))
        {
            if (!_visibleValueScopeCache.TryGetValue(scopeNode, out var symbols))
            {
                symbols = GetOrCollectVisibleValueDeclarations(scopeNode);
                _visibleValueScopeCache[scopeNode] = symbols;
            }

            for (var i = 0; i < symbols.Length; i++)
            {
                var candidate = symbols[i];
                if (candidate.Start > position || !seenDeclarations.Add(candidate.DeclarationNode))
                    continue;

                if (TryResolveVisibleValueSymbol(candidate) is { } symbol)
                    builder.Add(symbol);
            }
        }

        return builder.ToImmutable();
    }

    internal ISymbol? TryLookupVisibleValueSymbol(SyntaxNode contextNode, string name)
    {
        if (string.IsNullOrWhiteSpace(name))
            return null;

        if (contextNode is ExpressionSyntax expression &&
            TryResolveInterestLocalSymbol(expression) is { } interestSymbol &&
            string.Equals(interestSymbol.Name, name, StringComparison.Ordinal))
        {
            return interestSymbol;
        }

        if (TryResolveEnclosingFunctionParameterSymbol(contextNode, name, out var functionParameterSymbol))
            return functionParameterSymbol;

        var position = contextNode.Span.Start;

        foreach (var scopeNode in EnumerateVisibleValueScopes(contextNode))
        {
            if (!_visibleValueScopeCache.TryGetValue(scopeNode, out var symbols))
            {
                symbols = GetOrCollectVisibleValueDeclarations(scopeNode);
                _visibleValueScopeCache[scopeNode] = symbols;
            }

            for (var i = 0; i < symbols.Length; i++)
            {
                var candidate = symbols[i];
                if (!string.Equals(candidate.Name, name, StringComparison.Ordinal) ||
                    candidate.Start > position)
                {
                    continue;
                }

                if (TryResolveVisibleValueSymbol(candidate) is { } symbol)
                    return symbol;
            }
        }

        return null;
    }

    private bool TryResolveEnclosingFunctionParameterSymbol(
        SyntaxNode contextNode,
        string name,
        [System.Diagnostics.CodeAnalysis.NotNullWhen(true)] out IParameterSymbol? parameterSymbol)
    {
        parameterSymbol = null;

        foreach (var ancestor in contextNode.Ancestors())
        {
            IEnumerable<ParameterSyntax> parameters = ancestor switch
            {
                SimpleFunctionExpressionSyntax { Parameter: { } parameter } => [parameter],
                ParenthesizedFunctionExpressionSyntax { ParameterList: { } parameterList } => parameterList.Parameters,
                TrailingBlockExpressionSyntax { Parameter: { } parameter } => [parameter],
                TrailingBlockExpressionSyntax { ParameterList: { } parameterList } => parameterList.Parameters,
                _ => Enumerable.Empty<ParameterSyntax>()
            };

            foreach (var parameter in parameters)
            {
                if (!string.Equals(parameter.Identifier.ValueText, name, StringComparison.Ordinal))
                    continue;

                if (ancestor is TrailingBlockExpressionSyntax trailingBlock)
                    parameterSymbol = GetTrailingBlockParameterSymbol(trailingBlock, parameter);
                else if (!TryResolveFunctionExpressionParameterSymbolFast(parameter, out parameterSymbol))
                    return false;

                return parameterSymbol is not null;
            }
        }

        return false;
    }

    private ISymbol? TryResolveInterestLocalSymbol(ExpressionSyntax expression)
    {
        if (expression is not IdentifierNameSyntax identifier ||
            string.IsNullOrWhiteSpace(identifier.Identifier.ValueText))
        {
            return null;
        }

        var name = identifier.Identifier.ValueText;

        foreach (var ancestor in expression.Ancestors())
        {
            switch (ancestor)
            {
                case ForStatementSyntax forStatement
                    when forStatement.Body.Span.Contains(expression.Span):
                    {
                        if (forStatement.Target is IdentifierNameSyntax target &&
                            target.Identifier.ValueText == name)
                        {
                            var iterationType = TryGetForIterationElementType(forStatement.Expression);
                            if (iterationType is not null && iterationType.TypeKind != TypeKind.Error)
                                return CreateSyntheticInterestLocalSymbol(name, iterationType, expression);
                        }

                        if (forStatement.Target is PatternSyntax pattern)
                        {
                            if (TryResolveContextualPatternSymbol(pattern, expression, name, out var patternSymbol))
                                return patternSymbol;

                            var iterationType = TryGetForIterationElementType(forStatement.Expression);
                            if (iterationType is not null &&
                                TryInferPatternDesignationType(pattern, name, iterationType) is { } patternType)
                            {
                                return CreateSyntheticInterestLocalSymbol(name, patternType, expression);
                            }
                        }

                        break;
                    }

                case IfStatementSyntax ifStatement
                    when ifStatement.ThenStatement.Span.Contains(expression.Span):
                    {
                        if (TryResolvePatternDesignationSymbol(ifStatement.Condition, expression, name) is { } patternSymbol)
                            return patternSymbol;

                        break;
                    }

                case IfPatternStatementSyntax ifPatternStatement
                    when ifPatternStatement.ThenStatement.Span.Contains(expression.Span):
                    {
                        if (TryResolvePatternDesignationSymbol(ifPatternStatement.Pattern, expression, name) is { } patternSymbol)
                            return patternSymbol;

                        break;
                    }

                case WhilePatternStatementSyntax whilePatternStatement
                    when whilePatternStatement.Statement.Span.Contains(expression.Span):
                    {
                        if (TryResolvePatternDesignationSymbol(whilePatternStatement.Pattern, expression, name) is { } patternSymbol)
                            return patternSymbol;

                        break;
                    }

                case MatchArmSyntax matchArm:
                    {
                        if (TryResolveContextualPatternSymbol(matchArm.Pattern, expression, name, out var patternSymbol))
                            return patternSymbol;

                        if (matchArm.Parent is MatchExpressionSyntax matchExpression &&
                            TryGetExpressionType(matchExpression.Expression) is { } inputType &&
                            TryInferPatternDesignationType(matchArm.Pattern, name, inputType) is { } patternType)
                        {
                            return CreateSyntheticInterestLocalSymbol(name, patternType, expression);
                        }

                        break;
                    }
            }
        }

        return null;
    }

    private bool TryResolveContextualPatternSymbol(
        SyntaxNode patternRoot,
        ExpressionSyntax expression,
        string name,
        out ISymbol? symbol)
    {
        symbol = TryResolvePatternDesignationSymbol(patternRoot, expression, name);
        if (symbol is null)
            return false;

        if (GetTypeFromSymbol(symbol) is { TypeKind: not TypeKind.Error } type &&
            type.SpecialType != SpecialType.System_Object)
        {
            return true;
        }

        symbol = null;
        return false;
    }

    private ISymbol? TryResolvePatternDesignationSymbol(SyntaxNode patternRoot, ExpressionSyntax expression, string name)
    {
        var designation = patternRoot
            .DescendantNodesAndSelf()
            .OfType<SingleVariableDesignationSyntax>()
            .Where(single =>
                single.Identifier.ValueText == name &&
                single.Span.Start <= expression.Span.Start)
            .OrderByDescending(static single => single.Span.Start)
            .FirstOrDefault();

        return designation is not null && TryResolveAvailablePatternDesignationSymbol(designation, out var symbol, allowErrorType: true)
            ? symbol
            : null;
    }

    private bool TryResolveAvailablePatternDesignationSymbol(
        SingleVariableDesignationSyntax designation,
        out ILocalSymbol? localSymbol,
        bool allowErrorType = false)
    {
        localSymbol = null;

        if (TryGetCachedBoundNode(designation) is BoundSingleVariableDesignator cachedDesignator &&
            cachedDesignator.Local.Type.TypeKind != TypeKind.Error)
        {
            localSymbol = cachedDesignator.Local;
            return true;
        }

        if (RequiresPatternOwnerBindingForDesignation(designation) &&
            TryGetStablePatternDesignationSymbol(designation, out localSymbol))
        {
            return true;
        }

        if (!TryInferAvailablePatternDesignationType(designation, out var type) ||
            type is null ||
            type.TypeKind == TypeKind.Error)
        {
            if (!allowErrorType)
                return false;

            type = Compilation.ErrorTypeSymbol;
        }

        if (!TryGetContainingExecutableOwnerSymbol(designation, out var containingSymbol) ||
            containingSymbol is null)
        {
            containingSymbol = TryGetAvailableContainingSymbol(designation) ?? Compilation.GlobalNamespace;
        }

        localSymbol = new SourceLocalSymbol(
            designation.Identifier.ValueText,
            type,
            IsMutablePatternDesignation(designation),
            containingSymbol,
            containingSymbol.ContainingType,
            containingSymbol as INamespaceSymbol ?? containingSymbol.ContainingNamespace,
            [designation.Identifier.GetLocation()],
            [designation.GetReference()]);
        return true;
    }

    private bool TryGetStablePatternDesignationSymbol(
        SingleVariableDesignationSyntax designation,
        out ILocalSymbol? localSymbol)
    {
        localSymbol = null;

        if (TryGetCachedBoundNode(designation) is BoundSingleVariableDesignator cachedDesignator &&
            !cachedDesignator.Local.Type.ContainsErrorType())
        {
            localSymbol = cachedDesignator.Local;
            return true;
        }

        if (TryGetPatternDeclarationOwner(designation) is not { } owner)
            return false;

        _ = GetBoundNode(owner, BoundTreeView.Original);

        if (TryGetCachedBoundNode(designation) is BoundSingleVariableDesignator reboundDesignator &&
            !reboundDesignator.Local.Type.ContainsErrorType())
        {
            localSymbol = reboundDesignator.Local;
            return true;
        }

        return false;
    }

    private static SyntaxNode? TryGetPatternDeclarationOwner(SingleVariableDesignationSyntax designation)
    {
        if (designation.GetAncestor<MatchExpressionSyntax>() is { } matchExpression)
            return matchExpression;
        if (designation.GetAncestor<MatchStatementSyntax>() is { } matchStatement)
            return matchStatement;
        if (designation.GetAncestor<IsPatternExpressionSyntax>() is { } isPatternExpression)
            return isPatternExpression;
        if (designation.GetAncestor<IfPatternStatementSyntax>() is { } ifPatternStatement)
            return ifPatternStatement;
        if (designation.GetAncestor<WhilePatternStatementSyntax>() is { } whilePatternStatement)
            return whilePatternStatement;
        if (designation.GetAncestor<ForStatementSyntax>() is { } forStatement)
            return forStatement;
        if (designation.GetAncestor<PatternDeclarationAssignmentStatementSyntax>() is { } patternDeclarationAssignment)
            return patternDeclarationAssignment;

        return null;
    }

    private static bool RequiresPatternOwnerBindingForDesignation(SingleVariableDesignationSyntax designation)
        => designation.Ancestors().OfType<PatternSyntax>().Any(static pattern =>
            pattern is PositionalPatternSyntax or NominalDeconstructionPatternSyntax or PropertyPatternSyntax);

    private bool TryInferAvailablePatternDesignationType(
        SingleVariableDesignationSyntax designation,
        [System.Diagnostics.CodeAnalysis.NotNullWhen(true)] out ITypeSymbol? type)
    {
        type = null;
        var name = designation.Identifier.ValueText;

        if (string.IsNullOrWhiteSpace(name))
            return false;

        if (designation.Ancestors().OfType<DeclarationPatternSyntax>().FirstOrDefault() is { } declarationPattern &&
            declarationPattern.Designation.DescendantNodesAndSelf().Contains(designation) &&
            TryGetAvailableTypeInfo(declarationPattern.Type, out var declarationTypeInfo))
        {
            type = declarationTypeInfo.Type ?? declarationTypeInfo.ConvertedType;
            return type is not null;
        }

        var pattern = designation.AncestorsAndSelf().OfType<PatternSyntax>().FirstOrDefault();
        if (pattern is null)
            return false;

        if (pattern.Ancestors().OfType<ParameterSyntax>().FirstOrDefault() is { Pattern: { } parameterPattern } parameter &&
            parameterPattern.DescendantNodesAndSelf().Contains(designation) &&
            TryGetAvailableParameterType(parameter, out var parameterType) &&
            TryInferPatternDesignationType(parameterPattern, name, parameterType) is { } inferredParameterPatternType)
        {
            type = inferredParameterPatternType;
            return true;
        }

        if (pattern.Ancestors().OfType<MatchArmSyntax>().FirstOrDefault() is { } matchArm &&
            matchArm.Pattern.DescendantNodesAndSelf().Contains(designation) &&
            matchArm.Parent is MatchExpressionSyntax matchExpression &&
            TryGetAvailableTypeInfo(matchExpression.Expression, out var matchInputTypeInfo) &&
            (matchInputTypeInfo.Type ?? matchInputTypeInfo.ConvertedType) is { } matchInputType &&
            TryInferPatternDesignationType(matchArm.Pattern, name, matchInputType) is { } inferredMatchType)
        {
            type = inferredMatchType;
            return true;
        }

        if (pattern.Ancestors().OfType<IsPatternExpressionSyntax>().FirstOrDefault() is { } isPatternExpression &&
            isPatternExpression.Pattern.DescendantNodesAndSelf().Contains(designation) &&
            TryGetAvailableTypeInfo(isPatternExpression.Expression, out var isInputTypeInfo) &&
            (isInputTypeInfo.Type ?? isInputTypeInfo.ConvertedType) is { } isInputType &&
            TryInferPatternDesignationType(isPatternExpression.Pattern, name, isInputType) is { } inferredIsType)
        {
            type = inferredIsType;
            return true;
        }

        if (pattern.Ancestors().OfType<IfPatternStatementSyntax>().FirstOrDefault() is { } ifPatternStatement &&
            ifPatternStatement.Pattern.DescendantNodesAndSelf().Contains(designation) &&
            TryGetAvailableTypeInfo(ifPatternStatement.Expression, out var ifInputTypeInfo) &&
            (ifInputTypeInfo.Type ?? ifInputTypeInfo.ConvertedType) is { } ifInputType &&
            TryInferPatternDesignationType(ifPatternStatement.Pattern, name, ifInputType) is { } inferredIfType)
        {
            type = inferredIfType;
            return true;
        }

        if (pattern.Ancestors().OfType<WhilePatternStatementSyntax>().FirstOrDefault() is { } whilePatternStatement &&
            whilePatternStatement.Pattern.DescendantNodesAndSelf().Contains(designation) &&
            TryGetAvailableTypeInfo(whilePatternStatement.Expression, out var whileInputTypeInfo) &&
            (whileInputTypeInfo.Type ?? whileInputTypeInfo.ConvertedType) is { } whileInputType &&
            TryInferPatternDesignationType(whilePatternStatement.Pattern, name, whileInputType) is { } inferredWhileType)
        {
            type = inferredWhileType;
            return true;
        }

        if (pattern.Ancestors().OfType<ForStatementSyntax>().FirstOrDefault() is { Target: PatternSyntax forPattern } forStatement &&
            forPattern.DescendantNodesAndSelf().Contains(designation) &&
            TryGetForIterationElementType(forStatement.Expression) is { } iterationType &&
            TryInferPatternDesignationType(forPattern, name, iterationType) is { } inferredForType)
        {
            type = inferredForType;
            return true;
        }

        if (pattern.Ancestors().OfType<PatternDeclarationAssignmentStatementSyntax>().FirstOrDefault() is { } assignment &&
            assignment.Left.DescendantNodesAndSelf().Contains(designation) &&
            TryGetAvailableTypeInfo(assignment.Right, out var assignmentTypeInfo) &&
            (assignmentTypeInfo.Type ?? assignmentTypeInfo.ConvertedType) is { } assignmentType &&
            TryInferPatternDesignationType(assignment.Left, name, assignmentType) is { } inferredAssignmentType)
        {
            type = inferredAssignmentType;
            return true;
        }

        return false;
    }

    private static bool IsMutablePatternDesignation(SingleVariableDesignationSyntax designation)
        => designation.BindingKeyword.Kind == SyntaxKind.VarKeyword ||
           designation.Ancestors().OfType<PatternDeclarationAssignmentStatementSyntax>().FirstOrDefault()?.BindingKeyword.Kind == SyntaxKind.VarKeyword ||
           designation.Ancestors().OfType<ForStatementSyntax>().FirstOrDefault()?.BindingKeyword.Kind == SyntaxKind.VarKeyword ||
           designation.Ancestors().OfType<IfPatternStatementSyntax>().FirstOrDefault()?.BindingKeyword.Kind == SyntaxKind.VarKeyword ||
           designation.Ancestors().OfType<WhilePatternStatementSyntax>().FirstOrDefault()?.BindingKeyword.Kind == SyntaxKind.VarKeyword;

    private ITypeSymbol? TryInferPatternDesignationType(PatternSyntax pattern, string name, ITypeSymbol expectedType)
    {
        if (pattern is VariablePatternSyntax { Designation: SingleVariableDesignationSyntax designation } &&
            designation.Identifier.ValueText == name)
        {
            return expectedType;
        }

        if (pattern is DeclarationPatternSyntax { Designation: SingleVariableDesignationSyntax declarationDesignation } declarationPattern &&
            declarationDesignation.Identifier.ValueText == name)
        {
            return TryGetTypeSyntaxType(declarationPattern.Type) ?? expectedType;
        }

        if (pattern is PositionalPatternSyntax positional)
        {
            if (positional.Designation is SingleVariableDesignationSyntax positionalDesignation &&
                positionalDesignation.Identifier.ValueText == name)
            {
                return expectedType;
            }

            var tupleElements = expectedType is INamedTypeSymbol namedExpectedType
                ? namedExpectedType.TupleElements
                : [];
            if (!tupleElements.IsDefaultOrEmpty)
            {
                for (var i = 0; i < positional.Elements.Count && i < tupleElements.Length; i++)
                {
                    if (TryInferPatternDesignationType(positional.Elements[i].Pattern, name, tupleElements[i].Type) is { } elementType)
                        return elementType;
                }
            }
        }

        if (pattern is SequencePatternSyntax sequence)
        {
            if (sequence.Designation is SingleVariableDesignationSyntax sequenceDesignation &&
                sequenceDesignation.Identifier.ValueText == name)
            {
                return expectedType;
            }

            var elementType = TryGetSequenceElementType(expectedType);
            foreach (var element in sequence.Elements)
            {
                var targetType = element.Prefix.DotDotToken.Kind is SyntaxKind.DotDotToken or SyntaxKind.DotDotDotToken
                    ? expectedType
                    : elementType;
                if (targetType is not null &&
                    TryInferPatternDesignationType(element.Pattern, name, targetType) is { } nestedType)
                {
                    return nestedType;
                }
            }
        }

        if (pattern.DescendantNodesAndSelf()
                .OfType<SingleVariableDesignationSyntax>()
                .Any(designation => designation.Identifier.ValueText == name))
        {
            return expectedType;
        }

        return null;
    }

    private ILocalSymbol CreateSyntheticInterestLocalSymbol(string name, ITypeSymbol type, ExpressionSyntax expression)
    {
        var containingSymbol = GetBinder(expression).ContainingSymbol ?? Compilation.GlobalNamespace;
        return new SourceLocalSymbol(
            name,
            type,
            isMutable: false,
            containingSymbol,
            containingSymbol.ContainingType,
            containingSymbol as INamespaceSymbol ?? containingSymbol.ContainingNamespace,
            locations: [],
            declaringSyntaxReferences: []);
    }

    private ITypeSymbol? TryGetForIterationElementType(ExpressionSyntax expression)
    {
        var collectionType = TryGetExpressionType(expression);
        if (collectionType is null || collectionType.TypeKind == TypeKind.Error)
            return null;

        return TryGetSequenceElementType(collectionType);
    }

    private ITypeSymbol? TryGetExpressionType(ExpressionSyntax expression)
    {
        if (TryGetCachedSymbolInfo(expression, out var symbolInfo) &&
            GetTypeFromSymbol(symbolInfo.Symbol?.UnderlyingSymbol) is { } symbolType)
        {
            return symbolType;
        }

        return GetTypeInfo(expression).Type;
    }

    private ITypeSymbol? TryGetTypeSyntaxType(TypeSyntax typeSyntax)
    {
        if (TryGetCachedSymbolInfo(typeSyntax, out var symbolInfo) &&
            symbolInfo.Symbol?.UnderlyingSymbol is ITypeSymbol cachedType)
        {
            return cachedType;
        }

        return GetTypeInfo(typeSyntax).Type;
    }

    private ITypeSymbol? TryGetSequenceElementType(ITypeSymbol collectionType)
    {
        if (collectionType is IArrayTypeSymbol arrayType)
            return arrayType.ElementType;

        if (collectionType.SpecialType == SpecialType.System_String)
            return Compilation.GetSpecialType(SpecialType.System_Char);

        if (collectionType is INamedTypeSymbol namedType)
        {
            foreach (var candidate in EnumerateSelfAndInterfaces(namedType))
            {
                if (candidate.TypeArguments.Length == 1 &&
                    candidate.Name is "IEnumerable" or "IAsyncEnumerable")
                {
                    return candidate.TypeArguments[0];
                }
            }
        }

        return null;

        static IEnumerable<INamedTypeSymbol> EnumerateSelfAndInterfaces(INamedTypeSymbol type)
        {
            yield return type;
            foreach (var iface in type.AllInterfaces)
                yield return iface;
        }
    }

    private IEnumerable<SyntaxNode> EnumerateVisibleValueScopes(SyntaxNode contextNode)
    {
        for (SyntaxNode? current = contextNode; current is not null; current = current.Parent)
        {
            switch (current)
            {
                case BlockStatementSyntax:
                case CompilationUnitSyntax:
                case FunctionStatementSyntax:
                case MethodDeclarationSyntax:
                case AccessorDeclarationSyntax:
                case ConstructorDeclarationSyntax:
                case OperatorDeclarationSyntax:
                case ConversionOperatorDeclarationSyntax:
                case FunctionExpressionSyntax:
                case ArrowExpressionClauseSyntax:
                    yield return current;
                    break;
            }
        }
    }

    private ImmutableArray<Compilation.VisibleValueDeclaration> GetOrCollectVisibleValueDeclarations(SyntaxNode scopeNode)
    {
        if (Compilation.TryGetVisibleValueScopeDeclarations(scopeNode, out var descriptors))
            return ResolveVisibleValueDeclarations(scopeNode, descriptors);

        descriptors = CollectVisibleValueDeclarationDescriptors(scopeNode);
        Compilation.StoreVisibleValueScopeDeclarations(scopeNode, descriptors);
        return ResolveVisibleValueDeclarations(scopeNode, descriptors);
    }

    private ImmutableArray<Compilation.VisibleValueDeclarationDescriptor> CollectVisibleValueDeclarationDescriptors(SyntaxNode scopeNode)
    {
        var builder = ImmutableArray.CreateBuilder<Compilation.VisibleValueDeclarationDescriptor>();

        void AddSymbol(string? symbolName, int start, SyntaxNode? declarationNode)
        {
            if (string.IsNullOrWhiteSpace(symbolName) || declarationNode is null)
                return;

            builder.Add(new Compilation.VisibleValueDeclarationDescriptor(
                symbolName,
                start,
                new Compilation.VisibleValueDeclarationNodeDescriptor(declarationNode.Span, declarationNode.Kind)));
        }

        void AddParameters(IEnumerable<ParameterSyntax> parameters)
        {
            foreach (var parameter in parameters)
                AddSymbol(parameter.Identifier.ValueText, int.MinValue, parameter);
        }

        switch (scopeNode)
        {
            case FunctionStatementSyntax function when function.ParameterList is { } parameterList:
                AddParameters(parameterList.Parameters);
                break;
            case MethodDeclarationSyntax method when method.ParameterList is { } parameterList:
                AddParameters(parameterList.Parameters);
                break;
            case ConstructorDeclarationSyntax constructor when constructor.ParameterList is { } ctorParameterList:
                AddParameters(ctorParameterList.Parameters);
                break;
            case OperatorDeclarationSyntax @operator when @operator.ParameterList is { } opParameterList:
                AddParameters(opParameterList.Parameters);
                break;
            case ConversionOperatorDeclarationSyntax conversion when conversion.ParameterList is { } conversionParameterList:
                AddParameters(conversionParameterList.Parameters);
                break;
            case SimpleFunctionExpressionSyntax simpleFunction when simpleFunction.Parameter is { } simpleParameter:
                AddParameters([simpleParameter]);
                break;
            case ParenthesizedFunctionExpressionSyntax parenthesizedFunction when parenthesizedFunction.ParameterList is { } functionParameterList:
                AddParameters(functionParameterList.Parameters);
                break;
        }

        if (scopeNode is BlockStatementSyntax or CompilationUnitSyntax or ArrowExpressionClauseSyntax)
        {
            foreach (var declarator in DescendantNodesExcludingNestedScopes(scopeNode).OfType<VariableDeclaratorSyntax>())
            {
                AddSymbol(declarator.Identifier.ValueText, declarator.Span.Start, declarator);
            }

            foreach (var designation in DescendantNodesExcludingNestedScopes(scopeNode).OfType<SingleVariableDesignationSyntax>())
            {
                AddSymbol(designation.Identifier.ValueText, designation.Span.Start, designation);
            }
        }

        return builder
            .OrderByDescending(static symbol => symbol.Start)
            .ToImmutableArray();
    }

    private ImmutableArray<Compilation.VisibleValueDeclaration> ResolveVisibleValueDeclarations(
        SyntaxNode scopeNode,
        ImmutableArray<Compilation.VisibleValueDeclarationDescriptor> descriptors)
    {
        if (descriptors.IsDefaultOrEmpty)
            return ImmutableArray<Compilation.VisibleValueDeclaration>.Empty;

        var root = scopeNode.SyntaxTree.GetRoot();
        var builder = ImmutableArray.CreateBuilder<Compilation.VisibleValueDeclaration>(descriptors.Length);

        foreach (var descriptor in descriptors)
        {
            if (!TryResolveVisibleValueDeclarationNode(root, descriptor.Declaration, out var declarationNode))
                continue;

            builder.Add(new Compilation.VisibleValueDeclaration(descriptor.Name, descriptor.Start, declarationNode));
        }

        return builder.ToImmutable();
    }

    private static bool TryResolveVisibleValueDeclarationNode(
        SyntaxNode treeRoot,
        Compilation.VisibleValueDeclarationNodeDescriptor descriptor,
        out SyntaxNode declarationNode)
    {
        var candidate = treeRoot.FindNode(descriptor.Span, getInnermostNodeForTie: true);
        for (var current = candidate; current is not null; current = current.Parent)
        {
            if (current.Kind == descriptor.Kind && current.Span == descriptor.Span)
            {
                declarationNode = current;
                return true;
            }
        }

        declarationNode = null!;
        return false;
    }

    internal ImmutableArray<Compilation.VisibleValueDeclaration> GetVisibleValueDeclarationsForTesting(SyntaxNode scopeNode)
        => GetOrCollectVisibleValueDeclarations(scopeNode);

    private static bool IsNestedExecutableScope(SyntaxNode node)
        => node is FunctionStatementSyntax
            or MethodDeclarationSyntax
            or ConstructorDeclarationSyntax
            or OperatorDeclarationSyntax
            or ConversionOperatorDeclarationSyntax
            or FunctionExpressionSyntax
            or AccessorDeclarationSyntax
            or BlockStatementSyntax
            or ArrowExpressionClauseSyntax;

    private static IEnumerable<SyntaxNode> DescendantNodesExcludingNestedScopes(SyntaxNode scopeNode)
    {
        foreach (var child in scopeNode.ChildNodes())
        {
            if (IsNestedExecutableScope(child))
                continue;

            yield return child;

            foreach (var descendant in DescendantNodesExcludingNestedScopes(child))
                yield return descendant;
        }
    }

    private ISymbol? TryResolveVisibleValueSymbol(Compilation.VisibleValueDeclaration declaration)
        => declaration.DeclarationNode switch
        {
            ParameterSyntax parameter => TryResolveFunctionExpressionParameterSymbolFast(parameter, out var fastFunctionParameter)
                ? fastFunctionParameter
                : parameter.Ancestors().OfType<FunctionExpressionSyntax>().Any()
                ? null
                : TryResolveParameterSymbolFast(parameter, out var parameterSymbol)
                ? parameterSymbol
                : TryResolveAvailableParameterSymbol(parameter, out var availableParameterSymbol)
                ? availableParameterSymbol
                : null,
            VariableDeclaratorSyntax variableDeclarator => IsLocalVariableDeclarator(variableDeclarator) &&
                  TryResolveAvailableLocalSymbol(variableDeclarator, out var localSymbol)
                ? localSymbol
                : TryGetStableLocalDeclarationSymbol(variableDeclarator, out var stableLocalSymbol)
                ? stableLocalSymbol
                : null,
            SingleVariableDesignationSyntax => null,
            _ => null
        };

    private bool TryResolveAvailableLocalSymbol(
        VariableDeclaratorSyntax variableDeclarator,
        out ILocalSymbol? localSymbol,
        bool allowErrorType = false)
    {
        localSymbol = null;

        if (variableDeclarator.Initializer?.Value.DescendantNodesAndSelf().OfType<FunctionExpressionSyntax>().Any() == true)
            return false;

        ITypeSymbol? type = null;
        if (variableDeclarator.TypeAnnotation?.Type is { } typeSyntax &&
            TryGetAvailableTypeInfo(typeSyntax, out var explicitTypeInfo))
        {
            type = explicitTypeInfo.Type ?? explicitTypeInfo.ConvertedType;
        }

        if ((type is null || type.TypeKind == TypeKind.Error) &&
            variableDeclarator.Initializer?.Value is { } initializer &&
            TryGetAvailableTypeInfo(initializer, out var initializerTypeInfo))
        {
            type = initializerTypeInfo.Type ?? initializerTypeInfo.ConvertedType;
        }

        if (type is null || type.TypeKind == TypeKind.Error)
        {
            if (!allowErrorType)
                return false;

            type = Compilation.ErrorTypeSymbol;
        }

        if (type is null)
            return false;

        var containingSymbol = TryGetAvailableContainingSymbol(variableDeclarator) ?? Compilation.GlobalNamespace;
        var containingType = containingSymbol as INamedTypeSymbol;
        var containingNamespace = containingSymbol as INamespaceSymbol;
        var isMutable = variableDeclarator.Parent is VariableDeclarationSyntax variableDeclaration &&
                        variableDeclaration.BindingKeyword.Kind == SyntaxKind.VarKeyword;

        localSymbol = new SourceLocalSymbol(
            variableDeclarator.Identifier.ValueText,
            type,
            isMutable,
            containingSymbol,
            containingType,
            containingNamespace,
            [variableDeclarator.Identifier.GetLocation()],
            [new SyntaxReference(variableDeclarator.SyntaxTree, variableDeclarator.Identifier.Span)]);
        return true;
    }

    private ISymbol? TryGetAvailableContainingSymbol(SyntaxNode node)
    {
        foreach (var ancestor in node.Ancestors())
        {
            switch (ancestor)
            {
                case TypeDeclarationSyntax typeDeclaration when TryGetClassSymbol(typeDeclaration, out var typeSymbol):
                    return typeSymbol;
                case UnionDeclarationSyntax unionDeclaration when TryGetUnionSymbol(unionDeclaration, out var unionSymbol):
                    return unionSymbol;
                case NamespaceDeclarationSyntax:
                    return GetDeclaredSymbol(ancestor);
            }
        }

        return null;
    }

    private bool TryResolveParameterSymbolFast(ParameterSyntax parameterSyntax, out IParameterSymbol? parameterSymbol)
    {
        parameterSymbol = null;

        if (parameterSyntax.Parent?.Parent is TypeDeclarationSyntax parameterContainingType &&
            TryGetClassSymbol(parameterContainingType, out var containingType))
        {
            parameterSymbol = containingType
                .GetMembers(".ctor")
                .OfType<IMethodSymbol>()
                .SelectMany(method => method.Parameters)
                .FirstOrDefault(parameter => SymbolDeclarationUtilities.HasDeclaringSpan(parameter, parameterSyntax));
            return parameterSymbol is not null;
        }

        if (parameterSyntax.Parent?.Parent is MethodDeclarationSyntax methodDeclaration &&
            TryResolveMethodSymbolForDeclaration(methodDeclaration, out var methodSymbol))
        {
            parameterSymbol = methodSymbol.Parameters.FirstOrDefault(parameter =>
                SymbolDeclarationUtilities.HasDeclaringSpan(parameter, parameterSyntax));
            return parameterSymbol is not null;
        }

        if (parameterSyntax.Parent?.Parent is CaseDeclarationSyntax caseDeclaration &&
            TryGetUnionCaseSymbol(caseDeclaration, out var caseSymbol))
        {
            parameterSymbol = caseSymbol.ConstructorParameters.FirstOrDefault(parameter =>
                SymbolDeclarationUtilities.HasDeclaringSpan(parameter, parameterSyntax));
            return parameterSymbol is not null;
        }

        if (parameterSyntax.Parent?.Parent is FunctionStatementSyntax functionStatement &&
            GetDeclaredSymbol(functionStatement) is IMethodSymbol functionSymbol)
        {
            parameterSymbol = functionSymbol.Parameters.FirstOrDefault(parameter =>
                SymbolDeclarationUtilities.HasDeclaringSpan(parameter, parameterSyntax));
            return parameterSymbol is not null;
        }

        return false;
    }

    private bool TryResolveAvailableParameterSymbol(ParameterSyntax parameterSyntax, out IParameterSymbol? parameterSymbol)
    {
        parameterSymbol = null;

        if (parameterSyntax.Ancestors().OfType<FunctionExpressionSyntax>().Any())
            return false;

        var parameterType = parameterSyntax.TypeAnnotation?.Type is { } typeSyntax &&
                            TryGetAvailableFunctionParameterType(typeSyntax, out var annotatedType) &&
                            annotatedType.TypeKind != TypeKind.Error
            ? annotatedType
            : Compilation.ErrorTypeSymbol;

        var containingSymbol =
            parameterSyntax.Parent?.Parent is MethodDeclarationSyntax methodDeclaration &&
            TryResolveMethodSymbolForDeclaration(methodDeclaration, out var methodSymbol)
                ? methodSymbol
                : TryGetAvailableContainingSymbol(parameterSyntax) ?? Compilation.GlobalNamespace;
        var containingType = containingSymbol as INamedTypeSymbol ?? containingSymbol.ContainingType as INamedTypeSymbol;
        var containingNamespace = containingSymbol as INamespaceSymbol ?? containingSymbol.ContainingNamespace;
        var refKind = parameterSyntax.RefKindKeyword.Kind switch
        {
            SyntaxKind.RefKeyword => RefKind.Ref,
            SyntaxKind.OutKeyword => RefKind.Out,
            SyntaxKind.InKeyword => RefKind.In,
            _ => RefKind.None
        };

        parameterSymbol = new SourceParameterSymbol(
            parameterSyntax.Identifier.ValueText,
            parameterType,
            containingSymbol,
            containingType,
            containingNamespace,
            [parameterSyntax.GetLocation()],
            [parameterSyntax.GetReference()],
            refKind,
            hasExplicitDefaultValue: false,
            explicitDefaultValue: null,
            isMutable: parameterSyntax.BindingKeyword.Kind == SyntaxKind.VarKeyword,
            isVarParams: false);
        return true;
    }

    internal bool TryResolveFunctionExpressionParameterSymbolFast(ParameterSyntax parameterSyntax, out IParameterSymbol? parameterSymbol)
    {
        parameterSymbol = null;

        var functionExpression = parameterSyntax.Ancestors().OfType<FunctionExpressionSyntax>().FirstOrDefault();
        if (functionExpression is null ||
            !TryGetFunctionParameterIndex(functionExpression, parameterSyntax, out var parameterIndex))
        {
            return false;
        }

        ITypeSymbol parameterType;
        RefKind refKind;
        bool hasExplicitDefaultValue;
        object? explicitDefaultValue;
        bool isVarParams;

        if (parameterSyntax.TypeAnnotation?.Type is { } typeSyntax &&
            TryGetAvailableFunctionParameterType(typeSyntax, out var annotatedType) &&
            annotatedType.TypeKind != TypeKind.Error)
        {
            parameterType = annotatedType;
            refKind = parameterSyntax.RefKindKeyword.Kind switch
            {
                SyntaxKind.RefKeyword => RefKind.Ref,
                SyntaxKind.OutKeyword => RefKind.Out,
                SyntaxKind.InKeyword => RefKind.In,
                _ => RefKind.None
            };
            hasExplicitDefaultValue = false;
            explicitDefaultValue = null;
            isVarParams = false;
        }
        else
        {
            if (!TryGetAvailableOrCachedFunctionExpressionDelegateType(functionExpression, out var delegateType) ||
                delegateType is not INamedTypeSymbol namedDelegate ||
                namedDelegate.GetDelegateInvokeMethod() is not { } invokeMethod ||
                parameterIndex >= invokeMethod.Parameters.Length)
            {
                return false;
            }

            var delegateParameter = invokeMethod.Parameters[parameterIndex];
            if (delegateParameter.Type is null || delegateParameter.Type.TypeKind == TypeKind.Error)
                return false;

            parameterType = delegateParameter.Type;
            refKind = delegateParameter.RefKind;
            hasExplicitDefaultValue = delegateParameter.HasExplicitDefaultValue;
            explicitDefaultValue = delegateParameter.ExplicitDefaultValue;
            isVarParams = delegateParameter.IsVarParams;
        }

        var containingSymbol = TryGetAvailableContainingSymbol(functionExpression) ?? Compilation.GlobalNamespace;
        var containingType = containingSymbol as INamedTypeSymbol;
        var containingNamespace = containingSymbol as INamespaceSymbol ?? containingSymbol.ContainingNamespace;
        var isMutable = parameterSyntax.BindingKeyword.Kind == SyntaxKind.VarKeyword;

        parameterSymbol = new SourceParameterSymbol(
            parameterSyntax.Identifier.ValueText,
            parameterType,
            containingSymbol,
            containingType,
            containingNamespace,
            [parameterSyntax.GetLocation()],
            [parameterSyntax.GetReference()],
            refKind,
            hasExplicitDefaultValue,
            explicitDefaultValue,
            isMutable,
            isVarParams);
        return true;
    }

    private bool TryGetAvailableFunctionParameterType(TypeSyntax typeSyntax, out ITypeSymbol type)
    {
        if (TryGetAvailableTypeInfo(typeSyntax, out var typeInfo) &&
            (typeInfo.Type ?? typeInfo.ConvertedType) is { } availableType)
        {
            type = availableType;
            return true;
        }

        if (typeSyntax is SimpleNameSyntax typeName &&
            TryLookupAvailableTypeFromBinder(typeName, out var binderType) &&
            binderType is not null)
        {
            type = binderType;
            return true;
        }

        if (typeSyntax is SimpleNameSyntax importedTypeName &&
            TryLookupImportedTypeBySyntax(importedTypeName, out var importedType) &&
            importedType is not null)
        {
            type = importedType;
            return true;
        }

        type = null!;
        return false;
    }

    private bool TryLookupImportedTypeBySyntax(SimpleNameSyntax typeName, out ITypeSymbol? type)
    {
        var metadataTypeName = GetMetadataTypeName(
            typeName.Identifier.ValueText,
            typeName is GenericNameSyntax genericName ? genericName.TypeArgumentList.Arguments.Count : 0);

        foreach (var import in EnumerateImportDirectives(typeName))
        {
            var importName = import.Name.ToString();
            if (!importName.EndsWith(".*", StringComparison.Ordinal))
                continue;

            var namespaceName = importName[..^2];
            type = Compilation.GetTypeByMetadataName(namespaceName + "." + metadataTypeName);
            if (type is not null)
                return true;
        }

        type = null;
        return false;
    }

    private static IEnumerable<ImportDirectiveSyntax> EnumerateImportDirectives(SyntaxNode node)
    {
        foreach (var ancestor in node.AncestorsAndSelf())
        {
            switch (ancestor)
            {
                case CompilationUnitSyntax compilationUnit:
                    foreach (var import in compilationUnit.Imports)
                        yield return import;
                    break;
                case BaseNamespaceDeclarationSyntax namespaceDeclaration:
                    foreach (var import in namespaceDeclaration.Imports)
                        yield return import;
                    break;
            }
        }
    }

    private bool TryGetAvailableOrCachedFunctionExpressionDelegateType(
        FunctionExpressionSyntax functionExpression,
        out ITypeSymbol? delegateType)
    {
        if (TryGetCachedBoundNode(functionExpression) is BoundFunctionExpression cachedFunction &&
            !IsLikelyStaleFunctionBodyNode(cachedFunction) &&
            cachedFunction.DelegateType is not null &&
            cachedFunction.DelegateType.TypeKind != TypeKind.Error)
        {
            delegateType = cachedFunction.DelegateType;
            return true;
        }

        return TryGetAvailableFunctionExpressionDelegateType(functionExpression, out delegateType) &&
               delegateType is not null &&
               delegateType.TypeKind != TypeKind.Error;
    }

    private static ITypeSymbol? GetTypeFromSymbol(ISymbol? symbol)
    {
        while (symbol is not null)
        {
            switch (symbol)
            {
                case ILocalSymbol local:
                    return local.Type;
                case IFieldSymbol field:
                    return field.Type;
                case IPropertySymbol property:
                    return property.Type;
                case IEventSymbol @event:
                    return @event.Type;
                case IParameterSymbol parameter:
                    return parameter.Type;
                case IMethodSymbol method:
                    return method.ReturnType;
                case ITypeSymbol type:
                    return type;
            }

            var underlying = symbol.UnderlyingSymbol;
            if (ReferenceEquals(underlying, symbol))
                break;

            symbol = underlying;
        }

        return null;
    }

    private bool TryResolveInvocationOperatorFromReceiver(InvocationExpressionSyntax invocation, out SymbolInfo info)
    {
        info = SymbolInfo.None;

        if (TryGetAvailableTypeInfo(invocation.Expression, out var expressionTypeInfo))
        {
            var expressionType = expressionTypeInfo.Type ?? expressionTypeInfo.ConvertedType;
            if (TryResolveInvocationOperatorFromReceiverType(expressionType, invocation, out info))
                return true;
        }

        if (TryGetAvailableSymbolInfo(invocation.Expression, out var availableExpressionInfo) &&
            TryResolveInvocationOperatorFromReceiverType(
                GetTypeFromSymbol(availableExpressionInfo.Symbol?.UnderlyingSymbol ?? availableExpressionInfo.Symbol),
                invocation,
                out info))
        {
            return true;
        }

        if (invocation.Expression is IdentifierNameSyntax receiverIdentifier &&
            TryResolveEnclosingParameterType(receiverIdentifier, out var parameterType) &&
            TryResolveInvocationOperatorFromReceiverType(parameterType, invocation, out info))
        {
            return true;
        }

        var binderInfo = GetBinder(invocation.Expression).BindSymbol(invocation.Expression);
        if (TryResolveInvocationOperatorFromReceiverType(
            GetTypeFromSymbol(binderInfo.Symbol?.UnderlyingSymbol ?? binderInfo.Symbol),
            invocation,
            out info))
        {
            return true;
        }

        return false;
    }

    private bool TryResolveEnclosingParameterType(
        IdentifierNameSyntax identifier,
        [System.Diagnostics.CodeAnalysis.NotNullWhen(true)] out ITypeSymbol? type)
    {
        type = null;
        var name = identifier.Identifier.ValueText;
        if (string.IsNullOrWhiteSpace(name))
            return false;

        var methodDeclaration = identifier.Ancestors().OfType<MethodDeclarationSyntax>().FirstOrDefault();
        if (methodDeclaration is not null &&
            TryResolveParameterTypeSyntax(methodDeclaration.ParameterList?.Parameters, name, out type))
        {
            return true;
        }

        if (methodDeclaration is not null &&
            GetDeclaredSymbol(methodDeclaration) is IMethodSymbol method)
        {
            type = method.Parameters.FirstOrDefault(parameter => parameter.Name == name)?.Type;
            return type is not null && type.TypeKind != TypeKind.Error;
        }

        var functionStatement = identifier.Ancestors().OfType<FunctionStatementSyntax>().FirstOrDefault();
        if (functionStatement is not null &&
            TryResolveParameterTypeSyntax(functionStatement.ParameterList?.Parameters, name, out type))
        {
            return true;
        }

        if (functionStatement is not null &&
            GetDeclaredSymbol(functionStatement) is IMethodSymbol function)
        {
            type = function.Parameters.FirstOrDefault(parameter => parameter.Name == name)?.Type;
            return type is not null && type.TypeKind != TypeKind.Error;
        }

        return false;

        bool TryResolveParameterTypeSyntax(
            SeparatedSyntaxList<ParameterSyntax>? parameters,
            string parameterName,
            [System.Diagnostics.CodeAnalysis.NotNullWhen(true)] out ITypeSymbol? parameterType)
        {
            parameterType = null;
            if (parameters is null)
                return false;

            foreach (var parameter in parameters.Value)
            {
                if (parameter.Identifier.ValueText != parameterName ||
                    parameter.TypeAnnotation?.Type is not { } typeSyntax)
                {
                    continue;
                }

                if (TryResolveAvailableTypeSyntax(typeSyntax, out parameterType) &&
                    parameterType.TypeKind != TypeKind.Error)
                {
                    return true;
                }

                if (typeSyntax is NullableTypeSyntax nullableType &&
                    TryResolveAvailableTypeSyntax(nullableType.ElementType, out parameterType) &&
                    parameterType.TypeKind != TypeKind.Error)
                {
                    return true;
                }

                return false;
            }

            return false;
        }
    }

    private static bool TryResolveInvocationOperatorFromReceiverType(
        ITypeSymbol? receiverType,
        InvocationExpressionSyntax invocation,
        out SymbolInfo info)
    {
        info = SymbolInfo.None;

        if (receiverType is NullableTypeSymbol nullableReceiver)
            receiverType = nullableReceiver.UnderlyingType;

        if (receiverType?.GetPlainType() is not INamedTypeSymbol namedReceiverType ||
            namedReceiverType.TypeKind == TypeKind.Error)
        {
            return false;
        }

        var candidates = namedReceiverType
            .GetMembers("Invoke")
            .OfType<IMethodSymbol>()
            .Where(static method => !method.IsStatic)
            .Where(method => method.Parameters.Length == invocation.ArgumentList.Arguments.Count)
            .Cast<ISymbol>()
            .ToImmutableArray();

        if (candidates.Length == 0)
            return false;

        info = candidates.Length == 1
            ? new SymbolInfo(candidates[0])
            : new SymbolInfo(CandidateReason.OverloadResolutionFailure, candidates);
        return true;
    }

    private bool TryBindInterestRegion(ExpressionSyntax expression, out BoundExpression boundExpression)
    {
        var regionRoot = GetInterestBindingRoot(expression, includeExtendedExecutableRoots: false);

        if (regionRoot is not null)
        {
            BoundNode boundRegion = GetBoundNode(regionRoot, BoundTreeView.Original);
            if (TryFindBoundNodeBySyntax(boundRegion, expression, out var reboundNode) &&
                reboundNode is BoundExpression reboundExpression)
            {
                if (IsLikelyStaleInitializerExpression(expression, reboundExpression))
                {
                    BoundNode? reboundContextRoot = null;

                    if (TryGetEnclosingFunctionExpression(expression, out var enclosingFunctionExpression) &&
                        TryGetContextualBindingRoot(enclosingFunctionExpression, out var contextualRoot) &&
                        !ReferenceEquals(contextualRoot, enclosingFunctionExpression))
                    {
                        ClearCachedSemanticState(contextualRoot);
                        reboundContextRoot = GetBoundNode(contextualRoot, BoundTreeView.Original);
                    }

                    if (reboundContextRoot is null)
                    {
                        ClearCachedSemanticState(regionRoot);
                        reboundContextRoot = GetBoundNode(regionRoot, BoundTreeView.Original);
                    }

                    if (TryFindBoundNodeBySyntax(reboundContextRoot, expression, out var reboundRefreshedNode) &&
                        reboundRefreshedNode is BoundExpression reboundRefreshedExpression)
                    {
                        reboundExpression = reboundRefreshedExpression;
                    }
                }

                boundExpression = reboundExpression;
                return true;
            }
        }

        boundExpression = null!;
        return false;
    }

    private bool IsLikelyStaleInitializerExpression(ExpressionSyntax expression, BoundExpression reboundExpression)
    {
        if (reboundExpression.Type is null || !reboundExpression.Type.ContainsErrorType())
            return false;

        var declarator = expression.AncestorsAndSelf().OfType<VariableDeclaratorSyntax>().FirstOrDefault();
        if (declarator?.Initializer?.Value is null)
            return false;

        if (TryGetCachedBoundNode(declarator) is not BoundVariableDeclarator boundDeclarator ||
            boundDeclarator.Local.Type.ContainsErrorType())
        {
            return false;
        }

        return true;
    }

    private void EnsureDeclarationInterestBound(ISymbol symbol)
    {
        foreach (var syntaxReference in symbol.DeclaringSyntaxReferences)
        {
            var syntax = syntaxReference.GetSyntax();
            if (syntax is null)
                continue;

            var declarationRoot = GetInterestBindingRoot(syntax, includeExtendedExecutableRoots: true);

            if (declarationRoot is null)
                continue;

            _ = GetBoundNode(declarationRoot, BoundTreeView.Original);
            return;
        }
    }

    private SyntaxNode? GetInterestBindingRoot(SyntaxNode node, bool includeExtendedExecutableRoots)
    {
        if (!includeExtendedExecutableRoots &&
            Compilation.TryGetInterestBindingRootDescriptor(node, out var cachedDescriptor) &&
            TryResolveInterestBindingRootDescriptor(node.SyntaxTree.GetRoot(), cachedDescriptor, out var cachedRoot))
        {
            return cachedRoot;
        }

        SyntaxNode? root = null;
        if (includeExtendedExecutableRoots)
        {
            root = node.AncestorsAndSelf().FirstOrDefault(current =>
                current is BlockStatementSyntax &&
                current.Parent is BaseMethodDeclarationSyntax or FunctionStatementSyntax or AccessorDeclarationSyntax);

            root ??= node.AncestorsAndSelf().FirstOrDefault(current =>
                current is IfStatementSyntax or IfPatternStatementSyntax or WhileStatementSyntax or WhilePatternStatementSyntax or ForStatementSyntax);
        }

        root ??= node.AncestorsAndSelf().FirstOrDefault(current =>
            current is StatementSyntax or ArrowExpressionClauseSyntax);

        if (root is not null)
        {
            Compilation.StoreInterestBindingRootDescriptor(
                node,
                new Compilation.InterestBindingRootDescriptor(root.Span, root.Kind));
        }

        return root;
    }

    private static bool TryResolveInterestBindingRootDescriptor(
        SyntaxNode treeRoot,
        Compilation.InterestBindingRootDescriptor descriptor,
        out SyntaxNode root)
    {
        var candidate = treeRoot.FindNode(descriptor.Span, getInnermostNodeForTie: true);
        for (var current = candidate; current is not null; current = current.Parent)
        {
            if (current.Kind == descriptor.Kind && current.Span == descriptor.Span)
            {
                root = current;
                return true;
            }
        }

        root = null!;
        return false;
    }

    private bool TryGetExecutableOwner(SyntaxNode node, out SyntaxNode owner)
    {
        if (Compilation.TryGetExecutableOwnerDescriptor(node, out var cachedDescriptor) &&
            TryResolveExecutableOwnerDescriptor(node.SyntaxTree.GetRoot(), cachedDescriptor, out owner))
        {
            return !ReferenceEquals(owner, node);
        }

        owner = node.AncestorsAndSelf().FirstOrDefault(static current =>
            current is FunctionExpressionSyntax
                or BaseMethodDeclarationSyntax
                or BaseConstructorDeclarationSyntax
                or ParameterlessConstructorDeclarationSyntax
                or AccessorDeclarationSyntax
                or PropertyDeclarationSyntax
                or EventDeclarationSyntax
                or GlobalStatementSyntax
                or CompilationUnitSyntax)
            ?? node;

        Compilation.StoreExecutableOwnerDescriptor(
            node,
            new Compilation.ExecutableOwnerDescriptor(owner.Span, owner.Kind));
        return !ReferenceEquals(owner, node);
    }

    private static bool TryResolveExecutableOwnerDescriptor(
        SyntaxNode treeRoot,
        Compilation.ExecutableOwnerDescriptor descriptor,
        out SyntaxNode owner)
    {
        var candidate = treeRoot.FindNode(descriptor.Span, getInnermostNodeForTie: true);
        for (var current = candidate; current is not null; current = current.Parent)
        {
            if (current.Kind == descriptor.Kind && current.Span == descriptor.Span)
            {
                owner = current;
                return true;
            }
        }

        owner = null!;
        return false;
    }

    internal TypedConstant GetConstantValue(ExpressionSyntax expression)
    {
        ArgumentNullException.ThrowIfNull(expression);

        if (ConstantValueEvaluator.TryEvaluate(expression, out var value))
        {
            var typeInfo = GetTypeInfo(expression);
            return TypedConstant.CreatePrimitive(typeInfo.ConvertedType ?? typeInfo.Type, value);
        }

        EnsureDiagnosticBindingCompleted();

        if (GetBoundNode(expression) is not BoundExpression boundExpression)
            return TypedConstant.CreateError(null);

        return CreateTypedConstantCore(boundExpression);
    }

    /// <summary>
    /// Gets type information about a type syntax.
    /// </summary>
    /// <param name="typeSyntax">The type syntax node.</param>
    public TypeInfo GetTypeInfo(TypeSyntax typeSyntax)
    {
        if (typeSyntax is ExpressionSyntax expressionSyntax &&
            !IsExplicitTypeSyntaxContext(typeSyntax))
        {
            return GetTypeInfo(expressionSyntax);
        }

        TypeInfo Cache(TypeInfo info)
        {
            if (HasTypeInfo(info))
                _typeMappings[typeSyntax] = info;

            return info;
        }

        if (TryGetAvailableTypeInfo(typeSyntax, out var availableTypeInfo))
            return Cache(availableTypeInfo);

        if (TryGetTypeFromOwningDeclaration(typeSyntax, out var declaredType))
            return Cache(new TypeInfo(declaredType, declaredType, ComputeConversion(declaredType, declaredType)));

        var binder = GetBinder(typeSyntax);
        try
        {
            var result = binder.BindTypeSyntax(typeSyntax);
            var type = result.Success
                ? result.ResolvedType
                : null;

            if (type is null || type.TypeKind == TypeKind.Error)
                return new TypeInfo(null, null);

            return Cache(new TypeInfo(type, type, ComputeConversion(type, type)));
        }
        catch
        {
            return new TypeInfo(null, null);
        }
    }

    private bool TryGetTypeFromOwningDeclaration(TypeSyntax typeSyntax, out ITypeSymbol type)
    {
        type = null!;

        if (typeSyntax.Parent is not TypeAnnotationClauseSyntax annotation)
            return false;

        type = annotation.Parent switch
        {
            PropertyDeclarationSyntax propertyDeclaration when GetDeclaredSymbol(propertyDeclaration) is IPropertySymbol property
                => property.Type,
            EventDeclarationSyntax eventDeclaration when GetDeclaredSymbol(eventDeclaration) is IEventSymbol @event
                => @event.Type,
            IndexerDeclarationSyntax indexerDeclaration when GetDeclaredSymbol(indexerDeclaration) is IPropertySymbol indexer
                => indexer.Type,
            ParameterSyntax parameterSyntax when GetDeclaredSymbol(parameterSyntax) is IParameterSymbol parameter
                => parameter.Type,
            _ => null!
        };

        return type is not null && type.TypeKind != TypeKind.Error;
    }

    public bool TryGetTypeInfo(TypeSyntax typeSyntax, out TypeInfo typeInfo)
    {
        typeInfo = GetTypeInfo(typeSyntax);
        return HasTypeInfo(typeInfo);
    }

    private static bool IsExplicitTypeSyntaxContext(TypeSyntax typeSyntax)
    {
        return typeSyntax.Parent switch
        {
            null => false,
            TypeAnnotationClauseSyntax => true,
            ArrowTypeClauseSyntax => true,
            TypeSyntax => true,
            TypeOfExpressionSyntax => true,
            SizeOfExpressionSyntax => true,
            DefaultExpressionSyntax => true,
            CastExpressionSyntax cast when ReferenceEquals(cast.Type, typeSyntax) => true,
            _ => false
        };
    }

    private Conversion ComputeConversion(ITypeSymbol? naturalType, ITypeSymbol? convertedType)
    {
        if (naturalType is null || convertedType is null)
            return Conversion.None;

        var conversion = Compilation.ClassifyConversion(naturalType, convertedType, includeUserDefined: true);
        if (conversion.Exists)
            return conversion;

        // Synthesize identity when classifier cannot represent the mapping but
        // symbols are identical (e.g. some pseudo-types in semantic model).
        if (SymbolEqualityComparer.Default.Equals(naturalType, convertedType))
            return new Conversion(isImplicit: true, isIdentity: true);

        return Conversion.None;
    }

    private static TypedConstant CreateTypedConstantCore(BoundExpression expression)
    {
        if (expression is BoundConversionExpression conversion)
            expression = conversion.Expression;

        return expression switch
        {
            BoundLiteralExpression literal when literal.Kind == BoundLiteralExpressionKind.NullLiteral
                => TypedConstant.CreateNull(literal.GetConvertedType() ?? literal.Type),
            BoundLiteralExpression literal
                => TypedConstant.CreatePrimitive(literal.GetConvertedType() ?? literal.Type, literal.Value),
            BoundFieldAccess fieldAccess when fieldAccess.Field is { IsConst: true } field
                => TypedConstant.CreatePrimitive(fieldAccess.Type, field.GetConstantValue()),
            _ => TypedConstant.CreateError(expression.Type)
        };
    }

    /// <summary>
    /// Looks up extension members that apply to the specified receiver type.
    /// </summary>
    /// <param name="receiverType">The type that receives extension members.</param>
    /// <param name="contextNode">Optional lookup context. Use this to include local imports in scope.</param>
    /// <param name="name">Optional member name filter.</param>
    /// <param name="includePartialMatches">Whether prefix-matching should be used for the name filter.</param>
    /// <param name="kinds">The extension member kinds to return.</param>
    public ExtensionMemberLookupResult LookupApplicableExtensionMembers(
        ITypeSymbol receiverType,
        SyntaxNode? contextNode = null,
        string? name = null,
        bool includePartialMatches = false,
        ExtensionMemberKinds kinds = ExtensionMemberKinds.All)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return ExtensionMemberLookupResult.Empty;

        var binder = contextNode is null
            ? GetBinder(SyntaxTree.GetRoot())
            : GetBinder(contextNode);

        return ExtensionMemberLookup.Lookup(
            binder,
            receiverType,
            name,
            includePartialMatches,
            kinds);
    }

    /// <summary>
    /// Get the bound node for a specific syntax node.
    /// </summary>
    /// <param name="node">The syntax node</param>
    /// <returns>The bound node</returns>
    private bool TryGetBoundNodeForSemanticQuery(SyntaxNode node, out BoundNode boundNode)
    {
        EnsureDeclarations();
        EnsureMemberSignaturesDeclared();

        if (TryGetCachedBoundNode(node) is { } cachedNode &&
            !IsLikelyStaleFunctionBodyNode(cachedNode))
        {
            Compilation.PerformanceInstrumentation.SemanticQuery.RecordBoundNodeCacheHit();
            boundNode = cachedNode;
            return true;
        }

        if (TryGetContextualBindingRoot(node, out var contextualRoot) &&
            !ReferenceEquals(contextualRoot, node) &&
            contextualRoot is not CompilationUnitSyntax)
        {
            BoundNode contextualBoundRoot;
            if (TryGetCachedBoundNode(contextualRoot) is { } cachedContextualRoot &&
                !IsLikelyStaleFunctionBodyNode(cachedContextualRoot))
            {
                contextualBoundRoot = cachedContextualRoot;
            }
            else
            {
                var contextualBinder = GetBinderForIncrementalSemanticQuery(contextualRoot);
                contextualBoundRoot = contextualBinder.GetOrBind(contextualRoot);
            }

            if (TryFindBoundNodeBySyntax(contextualBoundRoot, node, out var contextualBoundNode))
            {
                CacheBoundNode(node, contextualBoundNode, GetBinderForIncrementalSemanticQuery(node));
                Compilation.PerformanceInstrumentation.SemanticQuery.RecordBoundNodeContextualCacheHit();
                boundNode = contextualBoundNode;
                return true;
            }
        }

        Compilation.PerformanceInstrumentation.SemanticQuery.RecordBoundNodeBindFallback();
        var binder = GetBinderForIncrementalSemanticQuery(node);
        boundNode = binder.GetOrBind(node);
        return true;
    }

    internal BoundNode GetBoundNode(SyntaxNode node)
    {
        return GetBoundNode(node, BoundTreeView.Original);
    }

    internal BoundNode GetBoundNode(SyntaxNode node, BoundTreeView view)
    {
        if (view is BoundTreeView.Both)
            throw new ArgumentOutOfRangeException(nameof(view));

        Compilation.PerformanceInstrumentation.SemanticQuery.RecordBoundNodeQuery();

        EnsureBindingReady();

        if (view is BoundTreeView.Original &&
            TryGetMacroReplacementSyntax(node, out var replacementNode) &&
            !ReferenceEquals(replacementNode, node))
        {
            var replacementBoundNode = GetBoundNode(replacementNode, view);
            CacheBoundNode(node, replacementBoundNode, GetBinder(node));
            return replacementBoundNode;
        }

        if (view is BoundTreeView.Original &&
            node is IdentifierNameSyntax identifier &&
            identifier.Parent is MemberAccessExpressionSyntax memberAccess &&
            ReferenceEquals(memberAccess.Name, identifier))
        {
            var memberAccessBoundNode = GetBoundNode(memberAccess, view);
            CacheBoundNode(node, memberAccessBoundNode, GetBinder(node));
            return memberAccessBoundNode;
        }

        if (view is BoundTreeView.Lowered &&
            TryResolveLoweringNode(node) is { } loweringNode &&
            !ReferenceEquals(loweringNode, node))
        {
            return GetBoundNode(loweringNode, view);
        }

        if (node is CompilationUnitSyntax compilationUnit)
            EnsureTopLevelCompilationUnitBound(compilationUnit);

        if (view is BoundTreeView.Original)
        {
            if (TryGetCachedBoundNode(node) is { } cachedNode &&
                !IsLikelyStaleFunctionBodyNode(cachedNode))
            {
                Compilation.PerformanceInstrumentation.SemanticQuery.RecordBoundNodeCacheHit();
                return cachedNode;
            }

            if (TryGetContextualBindingRoot(node, out var contextualRoot) &&
                !ReferenceEquals(contextualRoot, node))
            {
                BoundNode? contextualBoundRoot = TryGetCachedBoundNode(contextualRoot);
                if (contextualBoundRoot is not null &&
                    TryFindBoundNodeBySyntax(contextualBoundRoot, node, out var cachedContextualNode) &&
                    IsLikelyStaleFunctionBodyNode(cachedContextualNode))
                {
                    ClearCachedSemanticState(contextualRoot);
                    contextualBoundRoot = null;
                }

                if (contextualBoundRoot is not { })
                {
                    if (contextualRoot is CompilationUnitSyntax contextualCompilationUnit)
                    {
                        EnsureTopLevelCompilationUnitBound(contextualCompilationUnit);
                        contextualBoundRoot = TryGetCachedBoundNode(contextualCompilationUnit)
                            ?? CreateSyntheticTopLevelBlock(contextualCompilationUnit);
                    }
                    else
                    {
                        var contextualBinder = GetBinder(contextualRoot);
                        contextualBoundRoot = contextualBinder.GetOrBind(contextualRoot);
                    }
                }

                if (TryGetCachedBoundNode(node) is { } contextCachedNode &&
                    !IsLikelyStaleFunctionBodyNode(contextCachedNode))
                {
                    Compilation.PerformanceInstrumentation.SemanticQuery.RecordBoundNodeContextualCacheHit();
                    return contextCachedNode;
                }

                if (TryFindBoundNodeBySyntax(contextualBoundRoot, node, out var contextualBoundNode))
                {
                    Compilation.PerformanceInstrumentation.SemanticQuery.RecordBoundNodeContextualCacheHit();
                    CacheBoundNode(node, contextualBoundNode, GetBinder(node));
                    return contextualBoundNode;
                }
            }

            if (TryGetEnclosingFunctionExpression(node, out var enclosingFunctionExpression))
            {
                var cachedInFunctionBody = TryGetCachedBoundNode(node);
                if (cachedInFunctionBody is null || IsLikelyStaleFunctionBodyNode(cachedInFunctionBody))
                {
                    var rebindRoot = GetFunctionExpressionRebindRoot(enclosingFunctionExpression);
                    ClearCachedSemanticState(rebindRoot);
                    var reboundRoot = GetBoundNode(rebindRoot, view);
                    if (TryGetCachedBoundNode(node) is { } reboundFromFunction)
                    {
                        Compilation.PerformanceInstrumentation.SemanticQuery.RecordBoundNodeContextualCacheHit();
                        return reboundFromFunction;
                    }

                    if (TryFindBoundNodeBySyntax(reboundRoot, node, out var reboundFromRoot))
                    {
                        Compilation.PerformanceInstrumentation.SemanticQuery.RecordBoundNodeContextualCacheHit();
                        CacheBoundNode(node, reboundFromRoot, GetBinder(node));
                        return reboundFromRoot;
                    }
                }
                else
                {
                    Compilation.PerformanceInstrumentation.SemanticQuery.RecordBoundNodeCacheHit();
                    return cachedInFunctionBody;
                }
            }

            if (node is CompilationUnitSyntax compilationUnitNode)
            {
                EnsureTopLevelCompilationUnitBound(compilationUnitNode);
                if (TryGetCachedBoundNode(compilationUnitNode) is { } cachedCompilationUnit)
                {
                    Compilation.PerformanceInstrumentation.SemanticQuery.RecordBoundNodeCacheHit();
                    return cachedCompilationUnit;
                }

                return CreateSyntheticTopLevelBlock(compilationUnitNode);
            }

            Compilation.PerformanceInstrumentation.SemanticQuery.RecordBoundNodeBindFallback();
            var binder = GetBinder(node);
            var bound = binder.GetOrBind(node);

            if (node is BlockStatementSyntax blockSyntax &&
                bound is BoundBlockStatement boundBlock &&
                !boundBlock.Statements.Any() &&
                blockSyntax.Statements.Count > 0 &&
                node.Parent is { } methodDeclaration &&
                binder is not MethodBodyBinder &&
                TryResolveMethodSymbolForDeclaration(methodDeclaration, out var methodSymbol))
            {
                var fallbackParentBinder = GetMethodBodyParentBinder(methodDeclaration, binder.ParentBinder, ensureSourceDeclarations: true);
                var methodBodyBinder = new MethodBodyBinder(methodSymbol, fallbackParentBinder);
                CacheBinder(node, methodBodyBinder);
                bound = methodBodyBinder.GetOrBind(node);
            }

            return bound;
        }

        if (TryGetCachedLoweredBoundNode(node) is { } loweredCached)
        {
            Compilation.PerformanceInstrumentation.SemanticQuery.RecordBoundNodeLoweredCacheHit();
            return loweredCached;
        }

        if (node is CompilationUnitSyntax &&
            TryGetCachedBoundNode(node) is not { } &&
            TryGetCachedBoundNode(TryResolveLoweringNode(node) ?? node) is { } loweredTarget)
        {
            CacheLoweredBoundNode(node, loweredTarget, GetBinder(node));
            Compilation.PerformanceInstrumentation.SemanticQuery.RecordBoundNodeLoweredCacheHit();
            return loweredTarget;
        }

        var binderForLowering = GetBinder(node);
        var boundNode = TryGetCachedBoundNode(node);

        if (boundNode is null && node is CompilationUnitSyntax loweredCompilationUnit)
        {
            EnsureTopLevelCompilationUnitBound(loweredCompilationUnit);
            boundNode = TryGetCachedBoundNode(loweredCompilationUnit);

            boundNode ??= CreateSyntheticTopLevelBlock(loweredCompilationUnit);
        }

        boundNode ??= binderForLowering.GetOrBind(node);
        Compilation.PerformanceInstrumentation.SemanticQuery.RecordBoundNodeLoweredFallback();
        var loweredNode = LowerBoundNode(node, binderForLowering, boundNode);
        CacheLoweredBoundNode(node, loweredNode, binderForLowering);
        return loweredNode;
    }

    private bool TryGetContextualBindingRoot(SyntaxNode node, out SyntaxNode root)
    {
        if (Compilation.TryGetContextualBindingRootDescriptor(node, out var cachedDescriptor) &&
            TryResolveContextualBindingRootDescriptor(node.SyntaxTree.GetRoot(), cachedDescriptor, out root))
        {
            return !ReferenceEquals(root, node);
        }

        if (node is CompilationUnitSyntax)
        {
            root = node;
            return false;
        }

        var enclosingIf = node.AncestorsAndSelf().OfType<IfStatementSyntax>().FirstOrDefault();
        if (enclosingIf is not null)
        {
            root = enclosingIf;
            return true;
        }

        var enclosingIfPattern = node.AncestorsAndSelf().OfType<IfPatternStatementSyntax>().FirstOrDefault();
        if (enclosingIfPattern is not null)
        {
            root = enclosingIfPattern;
            return true;
        }

        var enclosingWhilePattern = node.AncestorsAndSelf().OfType<WhilePatternStatementSyntax>().FirstOrDefault();
        if (enclosingWhilePattern is not null)
        {
            root = enclosingWhilePattern;
            return true;
        }

        // Binding a node in isolation can drop scope/flow context (locals, overload shape).
        // Prefer binding the enclosing executable scope first.
        root = node.AncestorsAndSelf().OfType<BlockStatementSyntax>().FirstOrDefault()
               ?? node.AncestorsAndSelf().OfType<ArrowExpressionClauseSyntax>().FirstOrDefault()
               ?? node.AncestorsAndSelf().OfType<CompilationUnitSyntax>().FirstOrDefault()
               ?? node;

        Compilation.StoreContextualBindingRootDescriptor(
            node,
            new Compilation.ContextualBindingRootDescriptor(root.Span, root.Kind));
        return !ReferenceEquals(root, node);
    }

    private static bool TryResolveContextualBindingRootDescriptor(
        SyntaxNode treeRoot,
        Compilation.ContextualBindingRootDescriptor descriptor,
        out SyntaxNode root)
    {
        var candidate = treeRoot.FindNode(descriptor.Span, getInnermostNodeForTie: true);
        for (var current = candidate; current is not null; current = current.Parent)
        {
            if (current.Kind == descriptor.Kind && current.Span == descriptor.Span)
            {
                root = current;
                return true;
            }
        }

        root = null!;
        return false;
    }

    private void ClearCachedSemanticState(SyntaxNode node)
    {
        RemoveCachedBoundNode(node);
        RemoveCachedBinderIfAllowed(node);
        RemoveCachedSymbolMapping(node);
        if (node is FunctionExpressionSyntax functionExpression)
        {
            _functionExpressionSymbolCache.TryRemove(functionExpression, out _);
            _functionExpressionSymbolCreationInProgress.TryRemove(functionExpression, out _);
        }

        foreach (var child in node.DescendantNodes())
        {
            RemoveCachedBoundNode(child);
            RemoveCachedBinderIfAllowed(child);
            RemoveCachedSymbolMapping(child);
            if (child is FunctionExpressionSyntax childFunctionExpression)
            {
                _functionExpressionSymbolCache.TryRemove(childFunctionExpression, out _);
                _functionExpressionSymbolCreationInProgress.TryRemove(childFunctionExpression, out _);
            }
        }

        for (var ancestor = node.Parent; ancestor is not null; ancestor = ancestor.Parent)
        {
            if (ancestor is CompilationUnitSyntax)
                break;

            RemoveCachedBoundNode(ancestor);
            RemoveCachedBinderIfAllowed(ancestor);
            RemoveCachedSymbolMapping(ancestor);
        }
    }

    private void RemoveCachedBinderIfAllowed(SyntaxNode node)
    {
        if (node is CompilationUnitSyntax)
            return;

        RemoveCachedBinder(node);
    }

    private void RemoveCachedSymbolMapping(SyntaxNode node)
    {
        _symbolMappings.TryRemove(node, out _);
        _typeMappings.TryRemove(node, out _);
    }

    private static bool IsLikelyStaleFunctionBodyNode(BoundNode node)
    {
        return node switch
        {
            BoundErrorExpression => true,
            BoundFunctionExpression functionExpression
                when functionExpression.Type?.TypeKind == TypeKind.Error ||
                     functionExpression.DelegateType?.TypeKind == TypeKind.Error ||
                     functionExpression.ReturnType?.TypeKind == TypeKind.Error ||
                     functionExpression.Parameters.Any(static parameter => parameter.Type is null || parameter.Type.TypeKind == TypeKind.Error) => true,
            BoundBlockExpression blockExpression when blockExpression.Type?.TypeKind == TypeKind.Error => true,
            BoundExpression expression when expression.Type?.TypeKind == TypeKind.Error => true,
            _ => false
        };
    }

    private SyntaxNode GetFunctionExpressionRebindRoot(FunctionExpressionSyntax functionExpression)
    {
        if (Compilation.TryGetFunctionExpressionRebindRootDescriptor(functionExpression, out var cachedDescriptor) &&
            TryResolveFunctionExpressionRebindRootDescriptor(functionExpression.SyntaxTree.GetRoot(), cachedDescriptor, out var cachedRoot))
        {
            return cachedRoot;
        }

        ExpressionSyntax? enclosingExpression = null;

        for (var current = functionExpression.Parent; current is not null; current = current.Parent)
        {
            if (current is FunctionExpressionSyntax)
                continue;

            if (current is StatementSyntax statement)
            {
                Compilation.StoreFunctionExpressionRebindRootDescriptor(
                    functionExpression,
                    new Compilation.FunctionExpressionRebindRootDescriptor(statement.Span, statement.Kind));
                return statement;
            }

            if (enclosingExpression is null && current is ExpressionSyntax expression)
                enclosingExpression = expression;
        }

        var root = (SyntaxNode?)enclosingExpression ?? functionExpression;
        Compilation.StoreFunctionExpressionRebindRootDescriptor(
            functionExpression,
            new Compilation.FunctionExpressionRebindRootDescriptor(root.Span, root.Kind));
        return root;
    }

    private static bool TryResolveFunctionExpressionRebindRootDescriptor(
        SyntaxNode treeRoot,
        Compilation.FunctionExpressionRebindRootDescriptor descriptor,
        out SyntaxNode root)
    {
        var candidate = treeRoot.FindNode(descriptor.Span, getInnermostNodeForTie: true);
        for (var current = candidate; current is not null; current = current.Parent)
        {
            if (current.Kind == descriptor.Kind && current.Span == descriptor.Span)
            {
                root = current;
                return true;
            }
        }

        root = null!;
        return false;
    }

    private static bool TryGetEnclosingFunctionExpression(SyntaxNode node, out FunctionExpressionSyntax enclosingFunctionExpression)
    {
        for (var current = node.Parent; current is not null; current = current.Parent)
        {
            if (current is not FunctionExpressionSyntax functionExpression)
                continue;

            var body = (SyntaxNode?)functionExpression.Body ?? functionExpression.ExpressionBody;
            if (body is not null && body.Span.Contains(node.Span))
            {
                enclosingFunctionExpression = functionExpression;
                return true;
            }
        }

        enclosingFunctionExpression = null!;
        return false;
    }

    internal bool TryGetContextualBoundFunctionExpression(
        FunctionExpressionSyntax functionExpression,
        out BoundFunctionExpression boundFunction)
    {
        if (TryGetCachedBoundNode(functionExpression) is BoundFunctionExpression cachedFunction &&
            !IsLikelyStaleFunctionBodyNode(cachedFunction))
        {
            boundFunction = cachedFunction;
            return true;
        }

        if (TryGetContextualBindingRoot(functionExpression, out var contextualRoot) &&
            !ReferenceEquals(contextualRoot, functionExpression))
        {
            if (TryRebindContextualFunctionExpression(functionExpression, contextualRoot, out var reboundFunction))
            {
                boundFunction = reboundFunction;
                return true;
            }
        }

        boundFunction = null!;
        return false;
    }

    internal bool TryGetContextualBoundTrailingBlockFunctionExpression(
        TrailingBlockExpressionSyntax trailingBlock,
        out BoundFunctionExpression boundFunction)
    {
        if (TryGetCachedBoundNode(trailingBlock) is BoundFunctionExpression cachedFunction &&
            !IsLikelyStaleFunctionBodyNode(cachedFunction))
        {
            boundFunction = cachedFunction;
            return true;
        }

        if (TryGetContextualBindingRoot(trailingBlock, out var contextualRoot) &&
            !ReferenceEquals(contextualRoot, trailingBlock))
        {
            if (TryRebindContextualFunctionExpression(trailingBlock, contextualRoot, out var reboundFunction))
            {
                boundFunction = reboundFunction;
                return true;
            }
        }

        boundFunction = null!;
        return false;
    }

    internal Compilation.ContextualBindingRootDescriptor GetContextualBindingRootDescriptorForTesting(SyntaxNode node)
    {
        if (!TryGetContextualBindingRoot(node, out var root))
            return new Compilation.ContextualBindingRootDescriptor(node.Span, node.Kind);

        return new Compilation.ContextualBindingRootDescriptor(root.Span, root.Kind);
    }

    internal Compilation.InterestBindingRootDescriptor? GetInterestBindingRootDescriptorForTesting(
        SyntaxNode node,
        bool includeExtendedExecutableRoots)
    {
        var root = GetInterestBindingRoot(node, includeExtendedExecutableRoots);
        return root is null
            ? null
            : new Compilation.InterestBindingRootDescriptor(root.Span, root.Kind);
    }

    internal Compilation.ExecutableOwnerDescriptor GetExecutableOwnerDescriptorForTesting(SyntaxNode node)
    {
        if (!TryGetExecutableOwner(node, out var owner))
            return new Compilation.ExecutableOwnerDescriptor(node.Span, node.Kind);

        return new Compilation.ExecutableOwnerDescriptor(owner.Span, owner.Kind);
    }

    internal Compilation.FunctionExpressionRebindRootDescriptor GetFunctionExpressionRebindRootDescriptorForTesting(
        FunctionExpressionSyntax functionExpression)
    {
        var root = GetFunctionExpressionRebindRoot(functionExpression);
        return new Compilation.FunctionExpressionRebindRootDescriptor(root.Span, root.Kind);
    }

    internal Compilation.NodeInterestSymbolDescriptor? GetNodeInterestSymbolDescriptorForTesting(SyntaxNode node)
    {
        return Compilation.TryGetNodeInterestSymbolDescriptor(node, out var descriptor)
            ? descriptor
            : null;
    }

    internal Compilation.BinderParentAnchorDescriptor? GetBinderParentAnchorDescriptorForTesting(SyntaxNode node)
    {
        return TryGetBinderParentAnchor(node, out var anchor)
            ? new Compilation.BinderParentAnchorDescriptor(anchor.Span, anchor.Kind)
            : null;
    }

    internal bool IsExecutableOwnerMarkedChangedForTesting(SyntaxNode node)
    {
        var owner = TryGetExecutableOwner(node, out var resolvedOwner)
            ? resolvedOwner
            : node;

        return Compilation.IsChangedExecutableOwner(owner);
    }

    internal Compilation.MatchedExecutableOwner? GetMatchedExecutableOwnerForTesting(SyntaxNode node)
    {
        return Compilation.GetMatchedExecutableOwnerForTesting(node);
    }

    internal bool HasCachedBoundNodeForTesting(SyntaxNode node)
        => TryGetCachedBoundNode(node) is not null;

    internal void EnsureCompilationUnitDeclarationBindersCreated()
    {
        EnsureDeclarations();

        if (SyntaxTree.GetRoot() is CompilationUnitSyntax compilationUnit)
            _ = GetBinder(compilationUnit);
    }

    internal Binder GetIncrementalSemanticQueryBinderForTesting(SyntaxNode node)
    {
        EnsureDeclarations();
        EnsureMemberSignaturesDeclared();
        return GetBinderForIncrementalSemanticQuery(node);
    }

    internal BinderLifecycleSnapshot GetBinderLifecycleSnapshotForTesting(Binder binder)
    {
        if (_binderLifecycleSnapshots.TryGetValue(binder, out var snapshot))
            return snapshot;

        throw new InvalidOperationException("The binder has not been cached by this semantic model.");
    }

    internal BinderLifecycleSnapshot GetBinderLifecycleSnapshotForTesting(SyntaxNode node)
        => GetBinderLifecycleSnapshotForTesting(GetIncrementalSemanticQueryBinderForTesting(node));

    public IParameterSymbol? GetFunctionExpressionParameterSymbol(ParameterSyntax parameterSyntax)
    {
        if (TryResolveFunctionExpressionParameterSymbolFast(parameterSyntax, out var fastParameter))
            return fastParameter;

        EnsureBindingReady();
        EnsureDiagnosticBindingCompleted();

        if (parameterSyntax.Ancestors().OfType<FunctionExpressionSyntax>().FirstOrDefault() is { } functionExpression)
            return GetFunctionExpressionParameterSymbol(functionExpression, parameterSyntax);

        if (parameterSyntax.Ancestors().OfType<TrailingBlockExpressionSyntax>().FirstOrDefault() is { } trailingBlock)
            return GetTrailingBlockParameterSymbol(trailingBlock, parameterSyntax);

        return GetDeclaredSymbol(parameterSyntax) as IParameterSymbol;
    }

    private IParameterSymbol? GetFunctionExpressionParameterSymbol(
        FunctionExpressionSyntax functionExpression,
        ParameterSyntax parameterSyntax)
    {
        if (!_functionExpressionParameterLookupInProgress.TryAdd(functionExpression, 0))
        {
            return null;
        }

        try
        {
            if (TryGetCachedBoundNode(functionExpression) is BoundFunctionExpression cachedLambda &&
                !IsLikelyStaleFunctionBodyNode(cachedLambda) &&
                TryGetFunctionParameterBySyntax(functionExpression, parameterSyntax, cachedLambda.Parameters, out var cachedParameter))
            {
                return cachedParameter;
            }

            if (GetBoundNode(functionExpression) is BoundFunctionExpression boundLambda &&
                TryGetFunctionParameterBySyntax(functionExpression, parameterSyntax, boundLambda.Parameters, out var boundParameter))
            {
                return boundParameter;
            }

            if (TryGetContextualBoundFunctionExpression(functionExpression, out var contextualLambda) &&
                TryGetFunctionParameterBySyntax(functionExpression, parameterSyntax, contextualLambda.Parameters, out var contextualParameter))
            {
                return contextualParameter;
            }

            var functionSymbolInfo = GetSymbolInfo(functionExpression);
            var functionSymbol = functionSymbolInfo.Symbol ?? functionSymbolInfo.CandidateSymbols.FirstOrDefault();
            if (functionSymbol is IMethodSymbol lambdaMethod &&
                TryGetFunctionParameterBySyntax(functionExpression, parameterSyntax, lambdaMethod.Parameters, out var lambdaParameter))
            {
                return lambdaParameter;
            }

            return null;
        }
        finally
        {
            _functionExpressionParameterLookupInProgress.TryRemove(functionExpression, out _);
        }
    }

    private IParameterSymbol? GetTrailingBlockParameterSymbol(
        TrailingBlockExpressionSyntax trailingBlock,
        ParameterSyntax parameterSyntax)
    {
        if (!_functionExpressionParameterLookupInProgress.TryAdd(trailingBlock, 0))
        {
            return null;
        }

        try
        {
            if (TryGetCachedBoundNode(trailingBlock) is BoundFunctionExpression cachedLambda &&
                !IsLikelyStaleFunctionBodyNode(cachedLambda) &&
                TryGetTrailingBlockParameterBySyntax(trailingBlock, parameterSyntax, cachedLambda.Parameters, out var cachedParameter))
            {
                return cachedParameter;
            }

            if (TryGetContextualBoundTrailingBlockFunctionExpression(trailingBlock, out var contextualLambda) &&
                TryGetTrailingBlockParameterBySyntax(trailingBlock, parameterSyntax, contextualLambda.Parameters, out var contextualParameter))
            {
                return contextualParameter;
            }

            if (GetBoundNode(trailingBlock) is BoundFunctionExpression boundLambda &&
                TryGetTrailingBlockParameterBySyntax(trailingBlock, parameterSyntax, boundLambda.Parameters, out var boundParameter))
            {
                return boundParameter;
            }

            return null;
        }
        finally
        {
            _functionExpressionParameterLookupInProgress.TryRemove(trailingBlock, out _);
        }
    }

    private bool TryRebindContextualFunctionExpression(
        FunctionExpressionSyntax functionExpression,
        SyntaxNode contextualRoot,
        out BoundFunctionExpression boundFunction)
        => TryRebindContextualFunctionExpressionCore(functionExpression, contextualRoot, out boundFunction);

    private bool TryRebindContextualFunctionExpression(
        TrailingBlockExpressionSyntax trailingBlock,
        SyntaxNode contextualRoot,
        out BoundFunctionExpression boundFunction)
        => TryRebindContextualFunctionExpressionCore(trailingBlock, contextualRoot, out boundFunction);

    private bool TryRebindContextualFunctionExpressionCore(
        SyntaxNode functionSyntax,
        SyntaxNode contextualRoot,
        out BoundFunctionExpression boundFunction)
    {
        if (!_functionExpressionRebindInProgress.TryAdd(contextualRoot, 0))
        {
            boundFunction = null!;
            return false;
        }

        try
        {
            ClearCachedSemanticState(contextualRoot);
            var reboundRoot = GetBoundNode(contextualRoot, BoundTreeView.Original);
            if (TryFindBoundNodeBySyntax(reboundRoot, functionSyntax, out var reboundFunctionNode) &&
                reboundFunctionNode is BoundFunctionExpression reboundLambda)
            {
                CacheBoundNode(functionSyntax, reboundLambda, GetBinder(functionSyntax));
                if (functionSyntax is FunctionExpressionSyntax functionExpression)
                    _ = TryUpgradeFunctionExpressionSymbolFromBoundFunction(functionExpression, reboundLambda, out _);

                boundFunction = reboundLambda;
                return true;
            }

            boundFunction = null!;
            return false;
        }
        finally
        {
            _functionExpressionRebindInProgress.TryRemove(contextualRoot, out _);
        }
    }

    private bool TryUpgradeFunctionExpressionSymbolFromBoundFunction(
        FunctionExpressionSyntax functionExpression,
        BoundFunctionExpression boundFunction,
        out IMethodSymbol? functionSymbol)
    {
        if (boundFunction.Symbol is not IMethodSymbol boundMethod)
        {
            functionSymbol = null;
            return false;
        }

        var upgradedMethod = boundMethod;
        if (FunctionExpressionSymbolContainsError(boundMethod) &&
            boundMethod is SourceLambdaSymbol sourceLambda)
        {
            sourceLambda.SetParameters(boundFunction.Parameters);

            if (boundFunction.ReturnType.TypeKind != TypeKind.Error)
                sourceLambda.SetReturnType(boundFunction.ReturnType);

            if (boundFunction.DelegateType.TypeKind != TypeKind.Error)
                sourceLambda.SetDelegateType(boundFunction.DelegateType);

            sourceLambda.SetCapturedVariables(boundFunction.CapturedVariables);
            upgradedMethod = sourceLambda;
        }

        if (FunctionExpressionSymbolContainsError(upgradedMethod))
        {
            functionSymbol = null;
            return false;
        }

        _functionExpressionSymbolCache.AddOrUpdate(
            functionExpression,
            upgradedMethod,
            (_, _) => upgradedMethod);
        functionSymbol = upgradedMethod;
        return true;
    }

    /// <summary>
    /// Get the bound expression for a specific expression syntax node.
    /// </summary>
    /// <param name="expression">The expression syntax node</param>
    /// <returns>The bound expression</returns>
    /// <remarks>Convenience overload</remarks>
    internal BoundExpression GetBoundNode(ExpressionSyntax expression)
    {
        return (BoundExpression)GetBoundNode((SyntaxNode)expression);
    }

    private static bool TryGetFunctionParameterBySyntax(
        FunctionExpressionSyntax functionExpression,
        ParameterSyntax parameterSyntax,
        IEnumerable<IParameterSymbol> parameters,
        out IParameterSymbol parameterSymbol)
    {
        if (TryGetFunctionParameterIndex(functionExpression, parameterSyntax, out var parameterIndex))
        {
            parameterSymbol = parameters.ElementAtOrDefault(parameterIndex)!;
            if (parameterSymbol is not null)
                return true;
        }

        parameterSymbol = parameters.FirstOrDefault(parameter =>
            parameter.DeclaringSyntaxReferences.Any(reference =>
                reference.SyntaxTree == parameterSyntax.SyntaxTree &&
                reference.Span == parameterSyntax.Span))!;

        return parameterSymbol is not null;
    }

    private static bool TryGetFunctionParameterIndex(
        FunctionExpressionSyntax functionExpression,
        ParameterSyntax parameterSyntax,
        out int parameterIndex)
    {
        switch (functionExpression)
        {
            case ParenthesizedFunctionExpressionSyntax parenthesized:
                for (var i = 0; i < parenthesized.ParameterList.Parameters.Count; i++)
                {
                    if (IsSameSyntaxNode(parenthesized.ParameterList.Parameters[i], parameterSyntax))
                    {
                        parameterIndex = i;
                        return true;
                    }
                }

                break;

            case SimpleFunctionExpressionSyntax simple when IsSameSyntaxNode(simple.Parameter, parameterSyntax):
                parameterIndex = 0;
                return true;
        }

        parameterIndex = -1;
        return false;
    }

    private static bool TryGetTrailingBlockParameterBySyntax(
        TrailingBlockExpressionSyntax trailingBlock,
        ParameterSyntax parameterSyntax,
        IEnumerable<IParameterSymbol> parameters,
        out IParameterSymbol parameterSymbol)
    {
        if (TryGetTrailingBlockParameterIndex(trailingBlock, parameterSyntax, out var parameterIndex))
        {
            parameterSymbol = parameters.ElementAtOrDefault(parameterIndex)!;
            if (parameterSymbol is not null)
                return true;
        }

        parameterSymbol = parameters.FirstOrDefault(parameter =>
            parameter.DeclaringSyntaxReferences.Any(reference =>
                reference.SyntaxTree == parameterSyntax.SyntaxTree &&
                reference.Span == parameterSyntax.Span))!;

        return parameterSymbol is not null;
    }

    private static bool TryGetTrailingBlockParameterIndex(
        TrailingBlockExpressionSyntax trailingBlock,
        ParameterSyntax parameterSyntax,
        out int parameterIndex)
    {
        if (ReferenceEquals(trailingBlock.Parameter, parameterSyntax))
        {
            parameterIndex = 0;
            return true;
        }

        if (trailingBlock.ParameterList is not null)
        {
            for (var i = 0; i < trailingBlock.ParameterList.Parameters.Count; i++)
            {
                if (ReferenceEquals(trailingBlock.ParameterList.Parameters[i], parameterSyntax))
                {
                    parameterIndex = i;
                    return true;
                }
            }
        }

        parameterIndex = -1;
        return false;
    }

    private bool TryFindBoundNodeBySyntax(BoundNode root, SyntaxNode targetSyntax, out BoundNode boundNode)
    {
        var stack = new Stack<BoundNode>();
        var visited = new HashSet<BoundNode>(ReferenceEqualityComparer.Instance);
        stack.Push(root);

        while (stack.Count > 0)
        {
            var current = stack.Pop();
            if (!visited.Add(current))
                continue;

            var currentSyntax = GetSyntax(current);
            if (currentSyntax is not null && ReferenceEquals(currentSyntax, targetSyntax))
            {
                boundNode = current;
                return true;
            }

            if (currentSyntax is not null &&
                currentSyntax.Kind == targetSyntax.Kind &&
                currentSyntax.Span == targetSyntax.Span)
            {
                boundNode = current;
                return true;
            }

            foreach (var child in EnumerateBoundChildren(current))
                stack.Push(child);
        }

        boundNode = null!;
        return false;
    }
    private BoundNode LowerBoundNode(SyntaxNode syntaxNode, Binder binder, BoundNode boundNode)
    {
        boundNode = RewriteAsyncIfNeeded(syntaxNode, binder, boundNode);

        var containingSymbol = binder.ContainingSymbol;
        if (containingSymbol is null)
            return boundNode;

        try
        {
            return boundNode switch
            {
                BoundBlockStatement block => Lowerer.LowerBlock(containingSymbol, block),
                BoundStatement statement => Lowerer.LowerStatement(containingSymbol, statement),
                BoundExpression expression => Lowerer.LowerExpression(containingSymbol, expression),
                _ => boundNode
            };
        }
        catch
        {
            return boundNode;
        }
    }

    private BoundNode RewriteAsyncIfNeeded(SyntaxNode syntaxNode, Binder binder, BoundNode boundNode)
    {
        if (boundNode is BoundExpression expression &&
            syntaxNode is ArrowExpressionClauseSyntax &&
            binder.ContainingSymbol is SourceMethodSymbol expressionBodiedMethod)
        {
            var expressionBody = ConvertExpressionBodyToBlock(expressionBodiedMethod, expression);
            if (AsyncLowerer.ShouldRewrite(expressionBodiedMethod, expressionBody))
                return AsyncLowerer.Rewrite(expressionBodiedMethod, expressionBody);

            return boundNode;
        }

        if (boundNode is not BoundBlockStatement block)
            return boundNode;

        var sourceMethod = ResolveCanonicalSourceMethodForSyntax(
            syntaxNode,
            binder.ContainingSymbol as SourceMethodSymbol ?? TryGetEnclosingSourceMethod(syntaxNode));

        if (sourceMethod is not null &&
            AsyncLowerer.ShouldRewrite(sourceMethod, block))
        {
            return AsyncLowerer.Rewrite(sourceMethod, block);
        }

        if (binder.ContainingSymbol is SourceLambdaSymbol sourceLambda &&
            AsyncLowerer.ShouldRewrite(sourceLambda, block))
        {
            return AsyncLowerer.Rewrite(sourceLambda, block).Body;
        }

        if (syntaxNode is CompilationUnitSyntax && TryGetTopLevelMainMethod(binder) is { } topLevelMain &&
            AsyncLowerer.ShouldRewrite(topLevelMain, block))
        {
            return AsyncLowerer.Rewrite(topLevelMain, block);
        }

        return boundNode;
    }

    private SourceMethodSymbol? TryGetEnclosingSourceMethod(SyntaxNode syntaxNode)
    {
        for (var current = syntaxNode; current is not null; current = current.Parent)
        {
            switch (current)
            {
                case FunctionStatementSyntax functionStatement:
                    return GetDeclaredSymbol(functionStatement) as SourceMethodSymbol;
                case BaseMethodDeclarationSyntax methodDeclaration:
                    return ResolveCanonicalSourceMethod(methodDeclaration);
            }
        }

        return null;
    }

    private SourceMethodSymbol? ResolveCanonicalSourceMethod(BaseMethodDeclarationSyntax methodDeclaration)
    {
        var declared = GetDeclaredSymbol(methodDeclaration) as SourceMethodSymbol;

        if (declared is null)
            return null;

        if (declared.IsAsync && !declared.IsSignatureSkeleton)
            return declared;

        if (methodDeclaration is not MethodDeclarationSyntax methodSyntax)
            return declared;

        if (!methodSyntax.Modifiers.Any(modifier => modifier.Kind == SyntaxKind.AsyncKeyword))
            return declared;

        if (declared.ContainingType is not INamedTypeSymbol containingType)
            return FindCompilationWideAsyncMethodBySyntax(methodSyntax) ?? declared;

        var parameterCount = methodSyntax.ParameterList?.Parameters.Count ?? 0;
        var arity = methodSyntax.TypeParameterList?.Parameters.Count ?? 0;

        var candidate = containingType
            .GetMembers(declared.Name)
            .OfType<SourceMethodSymbol>()
            .FirstOrDefault(candidate =>
                candidate.IsAsync &&
                !candidate.IsSignatureSkeleton &&
                candidate.Parameters.Length == parameterCount &&
                candidate.TypeParameters.Length == arity &&
                candidate.DeclaringSyntaxReferences.Any(reference =>
                    reference.SyntaxTree == methodSyntax.SyntaxTree &&
                    reference.Span == methodSyntax.Span))
            ?? FindCompilationWideAsyncMethodBySyntax(methodSyntax);

        return candidate ?? declared;
    }

    private SourceMethodSymbol? FindCompilationWideAsyncMethodBySyntax(MethodDeclarationSyntax methodSyntax)
    {
        var targetTree = methodSyntax.SyntaxTree;
        var targetSpan = methodSyntax.Span;

        return Compilation.Module.GlobalNamespace
            .GetAllMembersRecursive()
            .OfType<INamedTypeSymbol>()
            .SelectMany(type => type.GetMembers(methodSyntax.Identifier.ValueText).OfType<SourceMethodSymbol>())
            .FirstOrDefault(method =>
                method.IsAsync &&
                method.DeclaringSyntaxReferences.Any(reference =>
                    reference.SyntaxTree == targetTree &&
                    reference.Span == targetSpan));
    }

    private SourceMethodSymbol? ResolveCanonicalSourceMethodForSyntax(SyntaxNode syntaxNode, SourceMethodSymbol? fallback)
    {
        for (var current = syntaxNode; current is not null; current = current.Parent)
        {
            if (current is BaseMethodDeclarationSyntax methodDeclaration)
                return ResolveCanonicalSourceMethod(methodDeclaration);
        }

        return fallback;
    }

    private SyntaxNode? TryResolveLoweringNode(SyntaxNode syntaxNode)
    {
        return syntaxNode switch
        {
            ArrowExpressionClauseSyntax arrow => arrow.Expression,
            _ => null
        };
    }

    private static BoundBlockStatement ConvertExpressionBodyToBlock(SourceMethodSymbol method, BoundExpression expression)
    {
        if (expression is BoundBlockExpression blockExpression)
            return new BoundBlockStatement(blockExpression.Statements, blockExpression.LocalsToDispose);

        if (method.ReturnType.SpecialType == SpecialType.System_Unit)
            return new BoundBlockStatement(new[] { new BoundExpressionStatement(expression) });

        return new BoundBlockStatement(new[] { new BoundReturnStatement(expression) });
    }

    private static SourceMethodSymbol? TryGetTopLevelMainMethod(Binder binder)
    {
        for (var current = binder; current is not null; current = current.ParentBinder)
        {
            if (current is TopLevelBinder topLevelBinder && topLevelBinder.MainMethod is SourceMethodSymbol mainMethod)
                return mainMethod;
        }

        return null;
    }

    private void EnsureTopLevelCompilationUnitBound(CompilationUnitSyntax compilationUnit)
    {
        if (TryGetCachedBoundNode(compilationUnit) is not null)
            return;

        static TopLevelBinder? FindTopLevelBinder(Binder? binder)
        {
            for (var current = binder; current is not null; current = current.ParentBinder)
            {
                if (current is TopLevelBinder topLevel)
                    return topLevel;
            }

            return null;
        }

        var globals = GetTopLevelGlobalStatements(compilationUnit).ToArray();
        if (globals.Length == 0)
            return;

        var topLevelBinder = FindTopLevelBinder(GetBinder(compilationUnit))
            ?? FindTopLevelBinder(GetBinder(globals[0]));
        if (topLevelBinder is null)
            return;

        topLevelBinder.BindGlobalStatements(globals);
    }

    internal void EnsureTopLevelFunctionDeclarations(CompilationUnitSyntax compilationUnit)
    {
        static TopLevelBinder? FindTopLevelBinder(Binder? binder)
        {
            for (var current = binder; current is not null; current = current.ParentBinder)
            {
                if (current is TopLevelBinder topLevel)
                    return topLevel;
            }

            return null;
        }

        var globals = GetTopLevelGlobalStatements(compilationUnit).ToArray();
        if (globals.Length == 0)
            return;

        var topLevelBinder = FindTopLevelBinder(GetBinder(compilationUnit))
            ?? FindTopLevelBinder(GetBinder(globals[0]));
        topLevelBinder?.DeclareGlobalFunctions(globals);

        foreach (var global in globals)
        {
            if (global.Statement is FunctionStatementSyntax function)
            {
                var declared = GetDeclaredSymbol(function) as IMethodSymbol;
                if (declared is null ||
                    !declared.DeclaringSyntaxReferences.Any(reference =>
                        reference.SyntaxTree == function.SyntaxTree &&
                        reference.Span == function.Span))
                {
                    var parentBinder = (Binder?)topLevelBinder ?? GetBinder(compilationUnit);
                    var functionBinder = new FunctionBinder(parentBinder, function);
                    _ = functionBinder.GetMethodSymbol();
                }
            }
        }
    }

    private static IEnumerable<GlobalStatementSyntax> GetTopLevelGlobalStatements(CompilationUnitSyntax compilationUnit)
    {
        foreach (var member in compilationUnit.Members)
        {
            switch (member)
            {
                case GlobalStatementSyntax global:
                    yield return global;
                    break;
                case FileScopedNamespaceDeclarationSyntax fileScoped:
                    foreach (var nested in fileScoped.Members.OfType<GlobalStatementSyntax>())
                        yield return nested;
                    break;
            }
        }
    }

    private BoundBlockStatement CreateSyntheticTopLevelBlock(CompilationUnitSyntax compilationUnit)
    {
        var statements = new List<BoundStatement>();
        var localsToDispose = ImmutableArray.CreateBuilder<ILocalSymbol>();

        foreach (var global in GetTopLevelGlobalStatements(compilationUnit))
        {
            if (TryGetCachedBoundNode(global.Statement) is BoundStatement cachedStatement)
            {
                statements.Add(cachedStatement);
            }
            else if (GetBinder(global.Statement).GetOrBind(global.Statement) is BoundStatement boundStatement)
            {
                statements.Add(boundStatement);
            }

            if (global.Statement is UseDeclarationStatementSyntax { InBlockClause: null } useDeclaration)
            {
                foreach (var declarator in useDeclaration.Declaration.Declarators)
                {
                    if (GetDeclaredSymbol(declarator) is ILocalSymbol localSymbol)
                        localsToDispose.Add(localSymbol);
                }
            }
        }

        return new BoundBlockStatement(statements, localsToDispose.ToImmutable());
    }

    /// <summary>
    /// Resolves the binder for a specific syntax node.
    /// </summary>
    /// <param name="node">The syntax node</param>
    /// <param name="parentBinder">Be careful</param>
    /// <returns>The binder for the specified syntax node</returns>
    /// <remarks>Might return a cached binder</remarks>
    internal Binder GetBinder(SyntaxNode node, Binder? parentBinder = null)
        => GetBinderCore(node, parentBinder, ensureSourceDeclarations: true);

    private Binder GetBinderForIncrementalSemanticQuery(SyntaxNode node, Binder? parentBinder = null)
        => GetBinderCore(node, parentBinder, ensureSourceDeclarations: false);

    private Binder GetBinderCore(SyntaxNode node, Binder? parentBinder, bool ensureSourceDeclarations)
    {
        if (ensureSourceDeclarations && !Compilation.SourceDeclarationsDeclared)
            Compilation.EnsureSourceDeclarationsDeclared();

        var nodeKey = GetSyntaxNodeMapKey(node);
        var useStructuralCache = CanUseStructuralBinderCache(node);
        if (_binderCache.TryGetValue(node, out var existingBinder) ||
            (useStructuralCache && _binderCacheByKey.TryGetValue(nodeKey, out existingBinder)))
        {
            if (parentBinder is not null &&
                !ReferenceEquals(existingBinder.ParentBinder, parentBinder) &&
                (parentBinder is FunctionExpressionBinder || parentBinder.ContainingSymbol is ILambdaSymbol))
            {
                // Lambda rebinds must not reuse cached binders from other scopes,
                // or lambda parameters may resolve incorrectly.
                return Compilation.BinderFactory.GetBinder(node, parentBinder) ?? existingBinder;
            }

            return existingBinder;
        }

        // special case for CompilationUnitSyntax
        if (node is CompilationUnitSyntax cu)
        {
            var binder = BindCompilationUnit(cu, parentBinder ?? Compilation.GlobalBinder);
            CacheBinder(cu, binder);
            return binder;
        }

        // Ensure parent binder is constructed and cached first
        Binder? actualParentBinder = parentBinder;

        if (actualParentBinder == null)
        {
            if (TryGetBinderParentAnchor(node, out var parentAnchor))
            {
                actualParentBinder = GetBinderCore(parentAnchor, null, ensureSourceDeclarations);
            }
            else if (!_binderCache.TryGetValue(node.Parent, out actualParentBinder) &&
                     !(CanUseStructuralBinderCache(node.Parent) &&
                       _binderCacheByKey.TryGetValue(GetSyntaxNodeMapKey(node.Parent), out actualParentBinder)))
            {
                // Recursively create and cache the parent binder first
                actualParentBinder = GetBinderCore(node.Parent, null, ensureSourceDeclarations);
            }
        }

        Binder? newBinder;

        if (node is TypeDeclarationSyntax typeDeclaration &&
            actualParentBinder is not TypeDeclarationBinder)
        {
            var typeParentBinder = actualParentBinder ??
                (node.Parent is not null ? GetBinderCore(node.Parent, null, ensureSourceDeclarations) : Compilation.GlobalBinder);
            var typeSymbol = GetDeclaredTypeSymbol(typeDeclaration);
            newBinder = typeDeclaration switch
            {
                InterfaceDeclarationSyntax interfaceDeclaration => new InterfaceDeclarationBinder(typeParentBinder, typeSymbol, interfaceDeclaration),
                _ => new ClassDeclarationBinder(typeParentBinder, typeSymbol, typeDeclaration)
            };
        }
        else if (node is BaseMethodDeclarationSyntax &&
            actualParentBinder is not MethodBinder &&
            TryResolveMethodSymbolForDeclaration(node, out var recoveredDeclarationMethodSymbol))
        {
            var methodParentBinder = actualParentBinder ??
                (node.Parent is not null ? GetBinderCore(node.Parent, null, ensureSourceDeclarations) : Compilation.GlobalBinder);
            newBinder = new MethodBinder(recoveredDeclarationMethodSymbol, methodParentBinder);
        }
        else if ((node is BlockSyntax or BlockStatementSyntax or ArrowExpressionClauseSyntax) &&
            node.Parent is { } parentMethodDeclaration &&
            actualParentBinder is not MethodBinder &&
            TryResolveMethodSymbolForDeclaration(parentMethodDeclaration, out var recoveredMethodSymbol))
        {
            var methodBodyParentBinder = GetMethodBodyParentBinder(
                parentMethodDeclaration,
                actualParentBinder,
                ensureSourceDeclarations);
            newBinder = new MethodBodyBinder(recoveredMethodSymbol, methodBodyParentBinder);
        }
        else
        {
            newBinder = Compilation.BinderFactory.GetBinder(node, actualParentBinder);
        }

        CacheBinder(node, newBinder);
        return newBinder;
    }

    private Binder GetMethodBodyParentBinder(
        SyntaxNode methodDeclaration,
        Binder? actualParentBinder,
        bool ensureSourceDeclarations)
    {
        if (actualParentBinder is MethodBinder methodBinder)
            return methodBinder;

        if (actualParentBinder is FunctionBinder functionBinder)
            return functionBinder.GetMethodBodyBinder();

        var declarationBinder = actualParentBinder ?? GetBinderCore(methodDeclaration, null, ensureSourceDeclarations);
        return declarationBinder switch
        {
            MethodBinder recoveredMethodBinder => recoveredMethodBinder,
            FunctionBinder recoveredFunctionBinder => recoveredFunctionBinder.GetMethodBodyBinder(),
            _ => declarationBinder
        };
    }

    internal void CacheBinderForNode(SyntaxNode node, Binder binder)
        => CacheBinder(node, binder);

    private void CacheBinder(SyntaxNode node, Binder binder)
    {
        _binderCache[node] = binder;
        if (CanUseStructuralBinderCache(node))
            _binderCacheByKey[GetSyntaxNodeMapKey(node)] = binder;

        _binderLifecycleSnapshots[binder] = CreateBinderLifecycleSnapshot(node, binder);

        if (TryComputeBinderParentAnchor(node, out var anchor))
        {
            Compilation.StoreBinderParentAnchorDescriptor(
                node,
                new Compilation.BinderParentAnchorDescriptor(anchor.Span, anchor.Kind));
        }
    }

    private BinderLifecycleSnapshot CreateBinderLifecycleSnapshot(SyntaxNode node, Binder binder)
    {
        var isStructuralCacheable = CanUseStructuralBinderCache(node);
        return new BinderLifecycleSnapshot(
            binder.GetType().Name,
            node.Kind,
            node.Span,
            isStructuralCacheable ? "StructuralNode" : "ExactNode",
            isStructuralCacheable,
            CreateBinderSymbolKey(binder.ContainingSymbol),
            binder.ParentBinder?.GetType().Name,
            binder.ParentBinder is null ? null : CreateBinderSymbolKey(binder.ParentBinder.ContainingSymbol));
    }

    private static string CreateBinderSymbolKey(ISymbol? symbol)
    {
        return symbol switch
        {
            null => "<null>",
            IMethodSymbol method => CreateMethodBinderSymbolKey(method),
            INamedTypeSymbol type => CreateNamedTypeBinderSymbolKey(type),
            IParameterSymbol parameter => $"{parameter.Kind}:{CreateBinderSymbolKey(parameter.ContainingSymbol)}.{parameter.Name}:{CreateTypeKey(parameter.Type)}:{parameter.RefKind}",
            _ => $"{symbol.Kind}:{CreateBinderSymbolKey(symbol.ContainingSymbol)}.{symbol.Name}"
        };
    }

    private static string CreateMethodBinderSymbolKey(IMethodSymbol method)
    {
        var parameters = string.Join(
            ",",
            method.Parameters.Select(parameter => $"{parameter.RefKind}:{parameter.Name}:{CreateTypeKey(parameter.Type)}"));

        return $"{method.Kind}:{CreateBinderSymbolKey(method.ContainingSymbol)}.{method.Name}<{method.Arity}>({parameters}):{CreateTypeKey(method.ReturnType)}";
    }

    private static string CreateNamedTypeBinderSymbolKey(INamedTypeSymbol type)
    {
        return $"{type.Kind}:{CreateBinderSymbolKey(type.ContainingSymbol)}.{type.MetadataName}";
    }

    private static string CreateTypeKey(ITypeSymbol? type)
    {
        return type is null
            ? "<null>"
            : type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
    }

    private static bool CanUseStructuralBinderCache(SyntaxNode node)
    {
        return node is
            CompilationUnitSyntax or
            BaseNamespaceDeclarationSyntax or
            TypeDeclarationSyntax or
            UnionDeclarationSyntax or
            MethodDeclarationSyntax or
            ConstructorDeclarationSyntax or
            OperatorDeclarationSyntax or
            ConversionOperatorDeclarationSyntax or
            FunctionStatementSyntax or
            AccessorDeclarationSyntax or
            PropertyDeclarationSyntax or
            EventDeclarationSyntax or
            IndexerDeclarationSyntax or
            ExtensionDeclarationSyntax;
    }

    private bool TryGetBinderParentAnchor(SyntaxNode node, out SyntaxNode anchor)
    {
        if (node.Parent is null)
        {
            anchor = null!;
            return false;
        }

        if (Compilation.TryGetBinderParentAnchorDescriptor(node, out var cachedDescriptor) &&
            TryResolveBinderParentAnchorDescriptor(node.SyntaxTree.GetRoot(), cachedDescriptor, out anchor))
        {
            return true;
        }

        if (TryComputeBinderParentAnchor(node, out anchor))
        {
            Compilation.StoreBinderParentAnchorDescriptor(
                node,
                new Compilation.BinderParentAnchorDescriptor(anchor.Span, anchor.Kind));
            return true;
        }

        anchor = null!;
        return false;
    }

    private static bool TryComputeBinderParentAnchor(SyntaxNode node, out SyntaxNode anchor)
    {
        for (var current = node.Parent; current is not null; current = current.Parent)
        {
            if (CreatesDistinctBinder(current) || CanUseStructuralBinderCache(current))
            {
                anchor = current;
                return true;
            }
        }

        anchor = null!;
        return false;
    }

    private static bool TryResolveBinderParentAnchorDescriptor(
        SyntaxNode treeRoot,
        Compilation.BinderParentAnchorDescriptor descriptor,
        out SyntaxNode anchor)
    {
        var candidate = treeRoot.FindNode(descriptor.Span, getInnermostNodeForTie: true);
        for (var current = candidate; current is not null; current = current.Parent)
        {
            if (current.Kind == descriptor.Kind && current.Span == descriptor.Span)
            {
                anchor = current;
                return true;
            }
        }

        anchor = null!;
        return false;
    }

    private static bool CreatesDistinctBinder(SyntaxNode node)
    {
        return node is
            AttributeListSyntax or
            AttributeSyntax or
            BlockSyntax or
            BlockStatementSyntax or
            ArrowExpressionClauseSyntax or
            IfExpressionSyntax or
            IfStatementSyntax or
            IfPatternStatementSyntax or
            ElseExpressionClauseSyntax or
            WhileStatementSyntax or
            WhilePatternStatementSyntax or
            ForStatementSyntax or
            FunctionStatementSyntax;
    }

    private bool TryResolveMethodSymbolForDeclaration(SyntaxNode declaration, out IMethodSymbol methodSymbol)
    {
        return declaration switch
        {
            MethodDeclarationSyntax methodDeclaration => TryResolveOrdinaryMethodSymbolForDeclaration(methodDeclaration, out methodSymbol),
            FunctionStatementSyntax functionStatement => TryResolveFunctionStatementSymbolForDeclaration(functionStatement, out methodSymbol),
            OperatorDeclarationSyntax operatorDeclaration => TryResolveOperatorMethodSymbolForDeclaration(operatorDeclaration, out methodSymbol),
            ConversionOperatorDeclarationSyntax conversionDeclaration => TryResolveConversionMethodSymbolForDeclaration(conversionDeclaration, out methodSymbol),
            _ => ReturnFalse(out methodSymbol)
        };

        static bool ReturnFalse(out IMethodSymbol symbol)
        {
            symbol = null!;
            return false;
        }
    }

    private bool TryResolveFunctionStatementSymbolForDeclaration(
        FunctionStatementSyntax functionStatement,
        out IMethodSymbol methodSymbol)
    {
        if (GetDeclaredSymbol(functionStatement) is IMethodSymbol symbol)
        {
            methodSymbol = symbol;
            return true;
        }

        methodSymbol = null!;
        return false;
    }

    private bool TryResolveOrdinaryMethodSymbolForDeclaration(MethodDeclarationSyntax methodDeclaration, out IMethodSymbol methodSymbol)
    {
        if (TryGetMethodSymbol(methodDeclaration, out methodSymbol))
            return true;

        if (methodDeclaration.Parent is TypeDeclarationSyntax containingTypeSyntax &&
            TryGetClassSymbol(containingTypeSyntax, out var containingType))
        {
            var targetTree = methodDeclaration.SyntaxTree;
            var targetSpan = methodDeclaration.Span;
            var parameterCount = methodDeclaration.ParameterList?.Parameters.Count ?? 0;
            var arity = methodDeclaration.TypeParameterList?.Parameters.Count ?? 0;

            var exact = containingType
                .GetMembers(methodDeclaration.Identifier.ValueText)
                .OfType<IMethodSymbol>()
                .OrderBy(method => method is SourceMethodSymbol { IsSignatureSkeleton: true } ? 1 : 0)
                .FirstOrDefault(method =>
                    method.Parameters.Length == parameterCount &&
                    method.Arity == arity &&
                    method.DeclaringSyntaxReferences.Any(reference =>
                        reference.SyntaxTree == targetTree &&
                        reference.Span == targetSpan));

            if (exact is not null)
            {
                methodSymbol = exact;
                return true;
            }
        }

        methodSymbol = null!;
        return false;
    }

    private bool TryResolveOperatorMethodSymbolForDeclaration(OperatorDeclarationSyntax operatorDeclaration, out IMethodSymbol methodSymbol)
    {
        var parameterCount = operatorDeclaration.ParameterList.Parameters.Count;
        if (!OperatorFacts.TryGetUserDefinedOperatorInfo(operatorDeclaration.OperatorToken.Kind, parameterCount, out var operatorInfo))
        {
            methodSymbol = null!;
            return false;
        }

        return TryResolveSpecialMethodSymbolForDeclaration(
            operatorDeclaration,
            operatorInfo.MetadataName,
            parameterCount,
            arity: 0,
            out methodSymbol);
    }

    private bool TryResolveConversionMethodSymbolForDeclaration(ConversionOperatorDeclarationSyntax conversionDeclaration, out IMethodSymbol methodSymbol)
    {
        if (!OperatorFacts.TryGetConversionOperatorMetadataName(conversionDeclaration.ConversionKindKeyword.Kind, out var metadataName))
        {
            methodSymbol = null!;
            return false;
        }

        return TryResolveSpecialMethodSymbolForDeclaration(
            conversionDeclaration,
            metadataName,
            conversionDeclaration.ParameterList.Parameters.Count,
            arity: 0,
            out methodSymbol);
    }

    private bool TryResolveSpecialMethodSymbolForDeclaration(
        SyntaxNode declaration,
        string metadataName,
        int parameterCount,
        int arity,
        out IMethodSymbol methodSymbol)
    {
        if (declaration.Parent is not { } containingDeclaration ||
            !Compilation.TryGetDeclaredTypeSymbol(containingDeclaration, out var containingType))
        {
            methodSymbol = null!;
            return false;
        }

        var targetTree = declaration.SyntaxTree;
        var targetSpan = declaration.Span;
        var exact = containingType
            .GetMembers(metadataName)
            .OfType<IMethodSymbol>()
            .FirstOrDefault(method =>
                method.Parameters.Length == parameterCount &&
                method.Arity == arity &&
                method.DeclaringSyntaxReferences.Any(reference =>
                    reference.SyntaxTree == targetTree &&
                    reference.Span == targetSpan));

        if (exact is null)
        {
            methodSymbol = null!;
            return false;
        }

        methodSymbol = exact;
        return true;
    }

    internal void EnsureRootBinderCreated()
    {
        Compilation.PerformanceInstrumentation.Setup.RecordEnsureRootBinderCreatedCall();

        if (_rootBinderCreated)
            return;

        var currentThreadId = Environment.CurrentManagedThreadId;

        lock (_bindingSetupGate)
        {
            while (_isCreatingRootBinder && _rootBinderThreadId != currentThreadId)
                Monitor.Wait(_bindingSetupGate);

            if (_rootBinderCreated || _isCreatingRootBinder)
                return;

            _isCreatingRootBinder = true;
            _rootBinderThreadId = currentThreadId;

            try
            {
                var root = SyntaxTree.GetRoot();
                _ = GetBinder(root);
                _rootBinderCreated = true;
                Compilation.PerformanceInstrumentation.Setup.RecordRootBinderCreated();
            }
            finally
            {
                _rootBinderThreadId = 0;
                _isCreatingRootBinder = false;
                Monitor.PulseAll(_bindingSetupGate);
            }
        }
    }

    internal bool RootBinderCreated => _rootBinderCreated;

    private void EnsureBindingReady()
    {
        if (!DeclarationsComplete)
            EnsureDeclarations();

        if (!RootBinderCreated)
            EnsureRootBinderCreated();
    }

    private INamespaceSymbol? GetMergedNamespace(INamespaceSymbol? namespaceSymbol)
    {
        if (namespaceSymbol is null)
            return null;

        var merged = Compilation.GlobalNamespace;
        if (namespaceSymbol.IsGlobalNamespace)
            return merged;

        var namespaceName = namespaceSymbol.ToMetadataName();
        if (string.IsNullOrEmpty(namespaceName))
            return merged;

        foreach (var part in namespaceName.Split('.', StringSplitOptions.RemoveEmptyEntries))
        {
            merged = merged.LookupNamespace(part);

            if (merged is null)
                break;
        }

        return merged;
    }
}
