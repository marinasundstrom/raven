using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
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
    private ConcurrentDictionary<SyntaxNode, SymbolInfo> _symbolMappings => _bindingState.SymbolMappings;
    private ConcurrentDictionary<SyntaxNode, BoundNode> _boundNodeCache => _bindingState.BoundNodeCache;
    private ConcurrentDictionary<SyntaxNode, (Binder, BoundNode)> _boundNodeCache2 => _bindingState.BoundNodeCacheWithBinder;
    private ConcurrentDictionary<SyntaxNode, BoundNode> _loweredBoundNodeCache => _bindingState.LoweredBoundNodeCache;
    private ConcurrentDictionary<SyntaxNode, (Binder, BoundNode)> _loweredBoundNodeCache2 => _bindingState.LoweredBoundNodeCacheWithBinder;
    private ConcurrentDictionary<FunctionExpressionSyntax, IMethodSymbol> _functionExpressionSymbolCache => _bindingState.FunctionExpressionSymbolCache;
    private ConcurrentDictionary<FunctionExpressionSyntax, byte> _functionExpressionSymbolCreationInProgress => _bindingState.FunctionExpressionSymbolCreationInProgress;
    private ConcurrentDictionary<FunctionExpressionSyntax, byte> _functionExpressionParameterLookupInProgress => _bindingState.FunctionExpressionParameterLookupInProgress;
    private ConcurrentDictionary<SyntaxNode, byte> _functionExpressionRebindInProgress => _bindingState.FunctionExpressionRebindInProgress;
    private ConcurrentDictionary<BoundNode, SyntaxNode> _syntaxCache => _bindingState.SyntaxCache;
    private ConcurrentDictionary<BoundNode, SyntaxNode> _loweredSyntaxCache => _bindingState.LoweredSyntaxCache;
    private ConcurrentDictionary<SyntaxNode, ImmutableArray<Compilation.VisibleValueDeclaration>> _visibleValueScopeCache => _bindingState.VisibleValueScopeCache;
    private IImmutableList<Diagnostic>? _diagnostics
    {
        get => _bindingState.Diagnostics;
        set => _bindingState.Diagnostics = value;
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
        public ConcurrentDictionary<SyntaxNode, SymbolInfo> SymbolMappings { get; } = new();
        public ConcurrentDictionary<SyntaxNode, BoundNode> BoundNodeCache { get; } = new();
        public ConcurrentDictionary<SyntaxNode, (Binder, BoundNode)> BoundNodeCacheWithBinder { get; } = new();
        public ConcurrentDictionary<SyntaxNode, BoundNode> LoweredBoundNodeCache { get; } = new();
        public ConcurrentDictionary<SyntaxNode, (Binder, BoundNode)> LoweredBoundNodeCacheWithBinder { get; } = new();
        public ConcurrentDictionary<FunctionExpressionSyntax, IMethodSymbol> FunctionExpressionSymbolCache { get; } = new();
        public ConcurrentDictionary<FunctionExpressionSyntax, byte> FunctionExpressionSymbolCreationInProgress { get; } = new();
        public ConcurrentDictionary<FunctionExpressionSyntax, byte> FunctionExpressionParameterLookupInProgress { get; } = new();
        public ConcurrentDictionary<SyntaxNode, byte> FunctionExpressionRebindInProgress { get; } = new();
        public ConcurrentDictionary<BoundNode, SyntaxNode> SyntaxCache { get; } = new(ReferenceEqualityComparer.Instance);
        public ConcurrentDictionary<BoundNode, SyntaxNode> LoweredSyntaxCache { get; } = new(ReferenceEqualityComparer.Instance);
        public ConcurrentDictionary<SyntaxNode, ImmutableArray<Compilation.VisibleValueDeclaration>> VisibleValueScopeCache { get; } = new();
        public DiagnosticBag DeclarationDiagnostics { get; } = new();
        public IImmutableList<Diagnostic>? Diagnostics { get; set; }
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
            EnsureDiagnosticBindingCompleted();

        return _diagnostics;
    }

    private void EnsureDiagnosticBindingCompleted()
    {
        lock (_diagnosticsCollectionGate)
        {
            if (_diagnostics is not null)
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
                var binder = GetBinder(root);

                Traverse(root, binder);

                DocumentationCommentValidator.Analyze(this, root, binder.Diagnostics);

                _diagnostics = _binderCache.Values
                    .SelectMany(static binderState => binderState.Diagnostics.AsEnumerable())
                    .Concat(_declarationDiagnostics.AsEnumerable())
                    .Distinct()
                    .ToImmutableArray();
            }
            finally
            {
                _isCollectingDiagnostics = false;
                _diagnosticCollectionThreadId = 0;
            }
        }

        void Traverse(SyntaxNode node, Binder currentBinder)
        {
            foreach (var child in node.ChildNodes())
            {
                var childBinder = GetBinder(child, currentBinder);

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

                if (child is ExpressionSyntax || child is StatementSyntax)
                {
                    childBinder.GetOrBind(child);
                    BindStatementAttributeSyntaxes(child, childBinder);
                    continue;
                }

                Traverse(child, childBinder);
            }
        }

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
        if (node is IdentifierNameSyntax invokedMemberName &&
            invokedMemberName.Parent is MemberAccessExpressionSyntax invokedMemberAccess &&
            ReferenceEquals(invokedMemberAccess.Name, invokedMemberName) &&
            invokedMemberAccess.Parent is InvocationExpressionSyntax invokedMemberInvocation &&
            ReferenceEquals(invokedMemberInvocation.Expression, invokedMemberAccess))
        {
            var invocationInfo = GetSymbolInfo(invokedMemberInvocation, cancellationToken);
            if (invocationInfo.Symbol is not null || !invocationInfo.CandidateSymbols.IsDefaultOrEmpty)
            {
                invocationInfo = ProjectBackingFieldSymbolsToAssociatedProperty(node, invocationInfo);
                _symbolMappings[node] = invocationInfo;
                return invocationInfo;
            }
        }

        if (node is IdentifierNameSyntax pipelineIdentifier &&
            pipelineIdentifier.Parent is InvocationExpressionSyntax pipelineInvocation &&
            ReferenceEquals(pipelineInvocation.Expression, pipelineIdentifier))
        {
            if (TryLookupPipelineInvocationSymbol(pipelineInvocation, pipelineIdentifier, pipelineIdentifier.Identifier.ValueText, out var directPipelineInfo))
            {
                directPipelineInfo = ProjectBackingFieldSymbolsToAssociatedProperty(node, directPipelineInfo);
                _symbolMappings[node] = directPipelineInfo;
                return directPipelineInfo;
            }
        }

        if (_symbolMappings.TryGetValue(node, out var symbolInfo))
        {
            if (symbolInfo.Symbol is not null || !symbolInfo.CandidateSymbols.IsDefaultOrEmpty)
                return symbolInfo;

            _symbolMappings.TryRemove(node, out _);
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
            ReferenceEquals(invokedCall.Expression, invokedName))
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
                    var binder = GetBinder(node);
                    info = binder.BindSymbol(node);
                }
            }
        }
        else if (node is IdentifierNameSyntax identifier &&
            identifier.Parent is MemberAccessExpressionSyntax memberAccess &&
            ReferenceEquals(memberAccess.Name, identifier))
        {
            if (TryBindExactSymbol(node, out var exactInfo))
            {
                info = exactInfo;
            }
            else if (TryBindExactSymbol(memberAccess, out var memberAccessInfo))
            {
                info = memberAccessInfo;
            }
            else if (memberAccess.Parent is InvocationExpressionSyntax invocation &&
                     ReferenceEquals(invocation.Expression, memberAccess) &&
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

                var boundMemberAccess = (BoundExpression)GetBoundNode(memberAccess);
                info = boundMemberAccess.GetSymbolInfo();

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
                    ReferenceEquals(refreshedInvocationSyntax.Expression, memberAccess))
                {
                    ClearCachedSemanticState(memberAccess);
                    ClearCachedSemanticState(refreshedInvocationSyntax);

                    if (GetBoundNode(refreshedInvocationSyntax) is BoundInvocationExpression refreshedInvocation)
                        info = new SymbolInfo(refreshedInvocation.Method);
                }

            }
        }
        else if (node is IdentifierNameSyntax receiverIdentifier &&
                 receiverIdentifier.Parent is MemberAccessExpressionSyntax receiverMemberAccess &&
                 ReferenceEquals(receiverMemberAccess.Expression, receiverIdentifier))
        {
            if (TryBindExactSymbol(node, out var exactInfo))
            {
                info = exactInfo;
            }
            else
            {
                var boundMemberAccess = (BoundExpression)GetBoundNode(receiverMemberAccess);
                var receiverInfo = boundMemberAccess switch
                {
                    BoundMemberAccessExpression memberAccessExpression => memberAccessExpression.Receiver.GetSymbolInfo(),
                    _ => boundMemberAccess.GetSymbolInfo()
                };

                if (receiverInfo.Symbol is not null || !receiverInfo.CandidateSymbols.IsDefaultOrEmpty)
                {
                    info = receiverInfo;
                }
                else if (TryFindBoundNodeBySyntax(boundMemberAccess, receiverIdentifier, out var boundReceiverNode))
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
                        var binder = GetBinder(node);
                        info = binder.BindSymbol(node);
                    }
                }
                else
                {
                    var binder = GetBinder(node);
                    info = binder.BindSymbol(node);
                }
            }
        }
        else if (node is IdentifierNameSyntax memberBindingIdentifier &&
                 memberBindingIdentifier.Parent is MemberBindingExpressionSyntax memberBinding &&
                 ReferenceEquals(memberBinding.Name, memberBindingIdentifier))
        {
            if (TryBindExactSymbol(node, out var exactInfo))
            {
                info = exactInfo;
            }
            else if (TryBindExactSymbol(memberBinding, out var memberBindingInfo))
            {
                info = memberBindingInfo;
            }
            else
            {
                var boundMemberBinding = (BoundExpression)GetBoundNode(memberBinding);
                info = boundMemberBinding.GetSymbolInfo();
            }
        }
        else if (node is ExpressionSyntax expression)
        {
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
                TryBindExactSymbol(expression, out var exactInfo))
            {
                info = exactInfo;
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

            var boundExpression = GetBoundNode(expression);
            var boundInfo = boundExpression.GetSymbolInfo();
            if (boundInfo.Symbol is not null || !boundInfo.CandidateSymbols.IsDefaultOrEmpty)
            {
                info = boundInfo;
                goto Complete;
            }

            if (TryResolveInterestLocalSymbol(expression) is { } interestLocalSymbol)
            {
                info = new SymbolInfo(interestLocalSymbol);
                goto Complete;
            }

            var binder = GetBinder(expression);
            var binderInfo = binder.BindSymbol(expression);
            if (binderInfo.Symbol is not null || !binderInfo.CandidateSymbols.IsDefaultOrEmpty)
            {
                info = binderInfo;
                goto Complete;
            }

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
                info = boundExpression.GetSymbolInfo();

            }
        }
        else if (node is StatementSyntax statement)
        {
            EnsureDiagnosticBindingCompleted();
            var boundStatement = (BoundStatement)GetBoundNode(statement);
            info = boundStatement.GetSymbolInfo();
        }
        else
        {
            var binder = GetBinder(node);
            info = binder.BindSymbol(node);
        }

        if (info.Symbol is null &&
            info.CandidateSymbols.IsDefaultOrEmpty &&
            node is SimpleNameSyntax pipelineName &&
            pipelineName.Parent is InvocationExpressionSyntax fallbackPipelineInvocation &&
            ReferenceEquals(fallbackPipelineInvocation.Expression, pipelineName))
        {
            if (TryLookupPipelineInvocationSymbol(fallbackPipelineInvocation, pipelineName, pipelineName.Identifier.ValueText, out var pipelineInfo))
            {
                info = pipelineInfo;
            }
        }

    Complete:
        info = ProjectBackingFieldSymbolsToAssociatedProperty(node, info);
        _symbolMappings[node] = info;
        return info;
    }

    private bool TryBindExactSymbol(SyntaxNode node, out SymbolInfo info)
    {
        var binder = GetBinder(node);
        info = binder.BindSymbol(node);
        return info.Symbol is not null || !info.CandidateSymbols.IsDefaultOrEmpty;
    }

    internal bool TryGetNodeInterestSymbolInfo(SyntaxNode node, out SymbolInfo info)
    {
        info = default;

        if (Compilation.TryGetNodeInterestSymbolDescriptor(node, out var cachedDescriptor) &&
            TryResolveNodeInterestSymbolDescriptor(cachedDescriptor, out var cachedSymbol))
        {
            info = new SymbolInfo(cachedSymbol);
            return true;
        }

        switch (node)
        {
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
                    var parameterSymbol = GetFunctionExpressionParameterSymbol(parameter) ?? GetDeclaredSymbol(parameter);
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
                    var designatedSymbol = GetDeclaredSymbol(designation);
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
            !ReferenceEquals(pipeExpression.Right, invocation))
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
                VariableDeclaratorSyntax variableDeclarator when variableDeclarator.Initializer?.Value.DescendantNodesAndSelf().OfType<FunctionExpressionSyntax>().Any() == true
                    => null!,
                FunctionExpressionSyntax functionExpression when TryGetFunctionExpressionSymbol(functionExpression, out var functionSymbol)
                    => functionSymbol!,
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
            Compilation.StoreNodeInterestSymbolDescriptor(queryNode, descriptor);
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
        => _declaredSymbolLookup.Lookup(node);

    internal bool TryGetStableLocalDeclarationSymbol(
        VariableDeclaratorSyntax variableDeclarator,
        out ILocalSymbol? localSymbol)
    {
        if (TryGetCachedBoundNode(variableDeclarator) is BoundVariableDeclarator cachedDeclarator &&
            !cachedDeclarator.Local.Type.ContainsErrorType())
        {
            localSymbol = cachedDeclarator.Local;
            return true;
        }

        if (variableDeclarator.TypeAnnotation is not null ||
            variableDeclarator.Initializer?.Value.DescendantNodesAndSelf().OfType<FunctionExpressionSyntax>().Any() != true)
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

    private void PrimeContextualFunctionExpressions(SyntaxNode root)
    {
        foreach (var parameter in root.DescendantNodes()
                     .OfType<ParameterSyntax>()
                     .Where(static parameter => parameter.Ancestors().OfType<FunctionExpressionSyntax>().Any()))
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
        if (expr is FunctionExpressionSyntax functionExpression &&
            TryGetFunctionExpressionDelegateType(functionExpression, out var functionDelegateType) &&
            functionDelegateType is not null &&
            functionDelegateType.TypeKind != TypeKind.Error)
        {
            return new TypeInfo(
                functionDelegateType,
                functionDelegateType,
                ComputeConversion(functionDelegateType, functionDelegateType));
        }

        if (TryGetNodeInterestSymbolType(expr, out var nodeInterestType) &&
            nodeInterestType is not null &&
            nodeInterestType.TypeKind != TypeKind.Error)
        {
            return new TypeInfo(nodeInterestType, nodeInterestType, ComputeConversion(nodeInterestType, nodeInterestType));
        }

        var symbolInfo = GetSymbolInfo(expr);
        var symbolType = GetTypeFromSymbol(symbolInfo.Symbol);
        if (symbolType is null && symbolInfo.Symbol is not null)
        {
            EnsureDeclarationInterestBound(symbolInfo.Symbol);
            symbolType = GetTypeFromSymbol(symbolInfo.Symbol);
        }
        if (symbolType is not null && symbolType.TypeKind != TypeKind.Error)
            return new TypeInfo(symbolType, symbolType, ComputeConversion(symbolType, symbolType));

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
                EnsureDiagnosticBindingCompleted();
                boundExpr = GetBoundNode(expr) as BoundExpression;
            }
        }

        if (boundExpr is null)
            return new TypeInfo(null, null);

        ITypeSymbol? naturalType = boundExpr.Type;

        ITypeSymbol? convertedType = boundExpr.GetConvertedType() ?? boundExpr.Type;

        var conversion = boundExpr switch
        {
            BoundConversionExpression cast => cast.Conversion,
            BoundAsExpression asExpression => asExpression.Conversion,
            _ => ComputeConversion(naturalType, convertedType)
        };

        return new TypeInfo(naturalType, convertedType, conversion);
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
        if (!TryGetContainingExecutableOwnerSymbol(functionExpression, out var containingSymbol) || containingSymbol is null)
        {
            functionSymbol = null;
            return false;
        }

        var containingType = containingSymbol.ContainingType as INamedTypeSymbol;
        var containingNamespace = containingSymbol.ContainingNamespace;
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

        var returnType = TryResolveShallowFunctionExpressionType(annotatedReturnTypeSyntax, out var resolvedReturnType)
            ? resolvedReturnType
            : defaultReturnType;

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
                => [CreateShallowFunctionExpressionParameter(lambdaSymbol, simple.Parameter)],
            ParenthesizedFunctionExpressionSyntax parenthesized when parenthesized.ParameterList is not null
                => parenthesized.ParameterList.Parameters.Select(parameter => CreateShallowFunctionExpressionParameter(lambdaSymbol, parameter)).ToArray(),
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
        ParameterSyntax parameterSyntax)
    {
        var parameterType = TryResolveShallowFunctionExpressionType(parameterSyntax.TypeAnnotation?.Type, out var resolvedType)
            ? resolvedType
            : Compilation.ErrorTypeSymbol;

        var refKind = parameterSyntax.RefKindKeyword.Kind switch
        {
            SyntaxKind.RefKeyword => RefKind.Ref,
            SyntaxKind.OutKeyword => RefKind.Out,
            SyntaxKind.InKeyword => RefKind.In,
            _ => RefKind.None
        };

        return new SourceParameterSymbol(
            parameterSyntax.Identifier.ValueText,
            parameterType,
            lambdaSymbol,
            lambdaSymbol.ContainingType as INamedTypeSymbol,
            lambdaSymbol.ContainingNamespace,
            [parameterSyntax.GetLocation()],
            [parameterSyntax.GetReference()],
            refKind);
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

                        if (forStatement.Target is PatternSyntax pattern &&
                            TryResolvePatternDesignationSymbol(pattern, expression, name) is { } patternSymbol)
                        {
                            return patternSymbol;
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

                case MatchArmSyntax matchArm:
                    {
                        if (TryResolvePatternDesignationSymbol(matchArm.Pattern, expression, name) is { } patternSymbol)
                            return patternSymbol;

                        break;
                    }
            }
        }

        return null;
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

        return designation is not null
            ? GetDeclaredSymbol(designation)
            : null;
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
        var collectionType = GetTypeInfo(expression).Type;
        if (collectionType is null || collectionType.TypeKind == TypeKind.Error)
            return null;

        if (collectionType is IArrayTypeSymbol arrayType)
            return arrayType.ElementType;

        if (collectionType.SpecialType == SpecialType.System_String)
            return Compilation.GetSpecialType(SpecialType.System_Char);

        return null;
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
            ParameterSyntax parameter => GetDeclaredSymbol(parameter),
            VariableDeclaratorSyntax variableDeclarator => GetDeclaredSymbol(variableDeclarator),
            SingleVariableDesignationSyntax designation => GetDeclaredSymbol(designation),
            _ => null
        };

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
            }

            var underlying = symbol.UnderlyingSymbol;
            if (ReferenceEquals(underlying, symbol))
                break;

            symbol = underlying;
        }

        return null;
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
        if (Compilation.TryGetInterestBindingRootDescriptor(node, out var cachedDescriptor) &&
            TryResolveInterestBindingRootDescriptor(node.SyntaxTree.GetRoot(), cachedDescriptor, out var cachedRoot))
        {
            return cachedRoot;
        }

        var root = node.AncestorsAndSelf().FirstOrDefault(current =>
            current is StatementSyntax or ArrowExpressionClauseSyntax ||
            (includeExtendedExecutableRoots && current is ForStatementSyntax or IfPatternStatementSyntax));

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

        var binder = GetBinder(typeSyntax);
        try
        {
            var result = binder.BindTypeSyntax(typeSyntax);
            var type = result.Success
                ? result.ResolvedType
                : null;

            if (type is null || type.TypeKind == TypeKind.Error)
                return new TypeInfo(null, null);

            return new TypeInfo(type, type, ComputeConversion(type, type));
        }
        catch
        {
            return new TypeInfo(null, null);
        }
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
    internal BoundNode GetBoundNode(SyntaxNode node)
    {
        return GetBoundNode(node, BoundTreeView.Original);
    }

    internal BoundNode GetBoundNode(SyntaxNode node, BoundTreeView view)
    {
        if (view is BoundTreeView.Both)
            throw new ArgumentOutOfRangeException(nameof(view));

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
                    return contextCachedNode;
                }

                if (TryFindBoundNodeBySyntax(contextualBoundRoot, node, out var contextualBoundNode))
                {
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
                        return reboundFromFunction;

                    if (TryFindBoundNodeBySyntax(reboundRoot, node, out var reboundFromRoot))
                    {
                        CacheBoundNode(node, reboundFromRoot, GetBinder(node));
                        return reboundFromRoot;
                    }
                }
                else
                {
                    return cachedInFunctionBody;
                }
            }

            if (node is CompilationUnitSyntax compilationUnitNode)
            {
                EnsureTopLevelCompilationUnitBound(compilationUnitNode);
                if (TryGetCachedBoundNode(compilationUnitNode) is { } cachedCompilationUnit)
                    return cachedCompilationUnit;

                return CreateSyntheticTopLevelBlock(compilationUnitNode);
            }

            var binder = GetBinder(node);
            var bound = binder.GetOrBind(node);

            if (node is BlockStatementSyntax blockSyntax &&
                bound is BoundBlockStatement boundBlock &&
                !boundBlock.Statements.Any() &&
                blockSyntax.Statements.Count > 0 &&
                node.Parent is MethodDeclarationSyntax methodDeclaration &&
                binder is not MethodBodyBinder &&
                TryResolveMethodSymbolForDeclaration(methodDeclaration, out var methodSymbol))
            {
                var fallbackParentBinder = binder.ParentBinder ?? GetBinder(methodDeclaration);
                var methodBodyBinder = new MethodBodyBinder(methodSymbol, fallbackParentBinder);
                CacheBinder(node, methodBodyBinder);
                bound = methodBodyBinder.GetOrBind(node);
            }

            return bound;
        }

        if (TryGetCachedLoweredBoundNode(node) is { } loweredCached)
            return loweredCached;

        if (node is CompilationUnitSyntax &&
            TryGetCachedBoundNode(node) is not { } &&
            TryGetCachedBoundNode(TryResolveLoweringNode(node) ?? node) is { } loweredTarget)
        {
            CacheLoweredBoundNode(node, loweredTarget, GetBinder(node));
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

    public IParameterSymbol? GetFunctionExpressionParameterSymbol(ParameterSyntax parameterSyntax)
    {
        EnsureBindingReady();
        EnsureDiagnosticBindingCompleted();

        if (parameterSyntax.Ancestors().OfType<FunctionExpressionSyntax>().FirstOrDefault() is not { } functionExpression)
            return GetDeclaredSymbol(parameterSyntax) as IParameterSymbol;

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

    private bool TryRebindContextualFunctionExpression(
        FunctionExpressionSyntax functionExpression,
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
            if (TryFindBoundNodeBySyntax(reboundRoot, functionExpression, out var reboundFunctionNode) &&
                reboundFunctionNode is BoundFunctionExpression reboundLambda)
            {
                CacheBoundNode(functionExpression, reboundLambda, GetBinder(functionExpression));
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
                    if (ReferenceEquals(parenthesized.ParameterList.Parameters[i], parameterSyntax))
                    {
                        parameterIndex = i;
                        return true;
                    }
                }

                break;

            case SimpleFunctionExpressionSyntax simple when ReferenceEquals(simple.Parameter, parameterSyntax):
                parameterIndex = 0;
                return true;
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
            if (currentSyntax is null)
                continue;

            if (ReferenceEquals(currentSyntax, targetSyntax))
            {
                boundNode = current;
                return true;
            }

            if (currentSyntax.Kind == targetSyntax.Kind &&
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

        if (declared.IsAsync || methodDeclaration is not MethodDeclarationSyntax methodSyntax)
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
    {
        if (!Compilation.SourceDeclarationsComplete)
            Compilation.EnsureSourceDeclarationsComplete();

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
                actualParentBinder = GetBinder(parentAnchor);
            }
            else if (!_binderCache.TryGetValue(node.Parent, out actualParentBinder) &&
                     !(CanUseStructuralBinderCache(node.Parent) &&
                       _binderCacheByKey.TryGetValue(GetSyntaxNodeMapKey(node.Parent), out actualParentBinder)))
            {
                // Recursively create and cache the parent binder first
                actualParentBinder = GetBinder(node.Parent);
            }
        }

        Binder? newBinder;

        if ((node is BlockStatementSyntax or ArrowExpressionClauseSyntax) &&
            node.Parent is MethodDeclarationSyntax parentMethodDeclaration &&
            actualParentBinder is not MethodBinder &&
            TryResolveMethodSymbolForDeclaration(parentMethodDeclaration, out var recoveredMethodSymbol))
        {
            newBinder = new MethodBodyBinder(recoveredMethodSymbol, actualParentBinder);
        }
        else
        {
            newBinder = Compilation.BinderFactory.GetBinder(node, actualParentBinder);
        }

        CacheBinder(node, newBinder);
        return newBinder;
    }

    internal void CacheBinderForNode(SyntaxNode node, Binder binder)
        => CacheBinder(node, binder);

    private void CacheBinder(SyntaxNode node, Binder binder)
    {
        _binderCache[node] = binder;
        if (CanUseStructuralBinderCache(node))
            _binderCacheByKey[GetSyntaxNodeMapKey(node)] = binder;

        if (TryComputeBinderParentAnchor(node, out var anchor))
        {
            Compilation.StoreBinderParentAnchorDescriptor(
                node,
                new Compilation.BinderParentAnchorDescriptor(anchor.Span, anchor.Kind));
        }
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
            ForStatementSyntax or
            FunctionStatementSyntax;
    }

    private bool TryResolveMethodSymbolForDeclaration(MethodDeclarationSyntax methodDeclaration, out IMethodSymbol methodSymbol)
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
