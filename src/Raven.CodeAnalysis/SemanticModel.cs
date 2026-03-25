using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
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
    private readonly ConcurrentDictionary<SyntaxNode, Binder> _binderCache = new();
    private readonly ConcurrentDictionary<SyntaxNodeMapKey, Binder> _binderCacheByKey = new();
    private readonly ConcurrentDictionary<SyntaxNode, SymbolInfo> _symbolMappings = new();
    private readonly ConcurrentDictionary<SyntaxNode, BoundNode> _boundNodeCache = new();
    private readonly ConcurrentDictionary<SyntaxNode, (Binder, BoundNode)> _boundNodeCache2 = new();
    private readonly ConcurrentDictionary<SyntaxNode, BoundNode> _loweredBoundNodeCache = new();
    private readonly ConcurrentDictionary<SyntaxNode, (Binder, BoundNode)> _loweredBoundNodeCache2 = new();
    private readonly ConcurrentDictionary<SyntaxNodeMapKey, byte> _asyncLoweringInProgress = new();
    private readonly ConcurrentDictionary<SyntaxNode, ImmutableDictionary<AttributeSyntax, MacroExpansionResult?>> _macroExpansionCache = new();
    private readonly ConcurrentDictionary<FreestandingMacroExpressionSyntax, FreestandingMacroExpansionResult?> _freestandingMacroExpansionCache = new();
    private readonly ConcurrentDictionary<SyntaxNode, SyntaxNode> _macroReplacementSyntaxMap = new();
    private readonly ConcurrentDictionary<TypeDeclarationSyntax, TypeDeclarationSyntax> _macroContainingTypeSyntaxMap = new();

    private readonly ConcurrentDictionary<BoundNode, SyntaxNode> _syntaxCache = new(ReferenceEqualityComparer.Instance);
    private readonly ConcurrentDictionary<BoundNode, SyntaxNode> _loweredSyntaxCache = new(ReferenceEqualityComparer.Instance);
    private readonly DeclaredSymbolLookup _declaredSymbolLookup;
    private IImmutableList<Diagnostic>? _diagnostics;
    private readonly DiagnosticBag _declarationDiagnostics = new();
    private readonly object _diagnosticsCollectionGate = new();
    private bool _declarationsComplete;
    private bool _rootBinderCreated;

    public bool IsDebuggingEnabled { get; set; } = true;

    public SemanticModel(Compilation compilation, SyntaxTree syntaxTree)
    {
        Compilation = compilation;
        SyntaxTree = syntaxTree;
        _declaredSymbolLookup = new DeclaredSymbolLookup(this);
    }

    public Compilation Compilation { get; }

    public SyntaxTree SyntaxTree { get; }

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

        return s_completionService.GetCompletions(Compilation, SyntaxTree, position);
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

        return s_completionService.GetCompletionsAsync(Compilation, SyntaxTree, position, cancellationToken);
    }

    public IImmutableList<Diagnostic> GetDiagnostics(CancellationToken cancellationToken = default)
    {
        if (_diagnostics is null)
        {
            EnsureDiagnosticsCollected();

            _diagnostics = _binderCache.Values
                .SelectMany(b => b.Diagnostics.AsEnumerable())
                .Concat(_declarationDiagnostics.AsEnumerable())
                .Distinct()
                .ToImmutableArray();
        }

        return _diagnostics;
    }

    private void EnsureDiagnosticsCollected()
    {
        lock (_diagnosticsCollectionGate)
        {
            var root = SyntaxTree.GetRoot();
            var binder = GetBinder(root);

            Traverse(root, binder);

            DocumentationCommentValidator.Analyze(this, root, binder.Diagnostics);
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
        if (_symbolMappings.TryGetValue(node, out var symbolInfo))
            return symbolInfo;

        SymbolInfo info;

        if (node is IdentifierNameSyntax identifier &&
            identifier.Parent is MemberAccessExpressionSyntax memberAccess &&
            ReferenceEquals(memberAccess.Name, identifier))
        {
            EnsureDiagnosticsCollected();
            var boundMemberAccess = (BoundExpression)GetBoundNode(memberAccess);
            info = boundMemberAccess.GetSymbolInfo();
        }
        else if (node is IdentifierNameSyntax receiverIdentifier &&
                 receiverIdentifier.Parent is MemberAccessExpressionSyntax receiverMemberAccess &&
                 ReferenceEquals(receiverMemberAccess.Expression, receiverIdentifier))
        {
            EnsureDiagnosticsCollected();
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
        else if (node is IdentifierNameSyntax memberBindingIdentifier &&
                 memberBindingIdentifier.Parent is MemberBindingExpressionSyntax memberBinding &&
                 ReferenceEquals(memberBinding.Name, memberBindingIdentifier))
        {
            EnsureDiagnosticsCollected();
            var boundMemberBinding = (BoundExpression)GetBoundNode(memberBinding);
            info = boundMemberBinding.GetSymbolInfo();
        }
        else if (node is ExpressionSyntax expression)
        {
            EnsureDiagnosticsCollected();

            if (expression is IdentifierNameSyntax)
            {
                var boundExpression = GetBoundNode(expression);
                var boundInfo = boundExpression.GetSymbolInfo();
                if (boundInfo.Symbol is not null || !boundInfo.CandidateSymbols.IsDefaultOrEmpty)
                {
                    info = boundInfo;
                    goto Complete;
                }
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
                var boundExpression = GetBoundNode(expression);
                info = boundExpression.GetSymbolInfo();
            }
        }
        else if (node is StatementSyntax statement)
        {
            EnsureDiagnosticsCollected();
            var boundStatement = (BoundStatement)GetBoundNode(statement);
            info = boundStatement.GetSymbolInfo();
        }
        else
        {
            var binder = GetBinder(node);
            info = binder.BindSymbol(node);
        }

Complete:
        info = ProjectBackingFieldSymbolsToAssociatedProperty(node, info);
        _symbolMappings[node] = info;
        return info;
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
        // Ensure the tree is fully bound so local declarations preceding the
        // expression are available for lookup. Without this, requesting type
        // information for expressions that reference earlier locals may return
        // null because those locals haven't been bound yet.
        EnsureDiagnosticsCollected();

        var boundExpr = GetBoundNode(expr) as BoundExpression;

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

    internal TypedConstant GetConstantValue(ExpressionSyntax expression)
    {
        ArgumentNullException.ThrowIfNull(expression);

        if (ConstantValueEvaluator.TryEvaluate(expression, out var value))
        {
            var typeInfo = GetTypeInfo(expression);
            return TypedConstant.CreatePrimitive(typeInfo.ConvertedType ?? typeInfo.Type, value);
        }

        EnsureDiagnosticsCollected();

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

        EnsureDeclarations();
        EnsureRootBinderCreated();

        if (view is BoundTreeView.Original &&
            TryGetMacroReplacementSyntax(node, out var replacementNode) &&
            !ReferenceEquals(replacementNode, node))
        {
            var replacementBoundNode = GetBoundNode(replacementNode, view);
            CacheBoundNode(node, replacementBoundNode, GetBinder(node));
            return replacementBoundNode;
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
            if (TryGetContextualBindingRoot(node, out var contextualRoot) &&
                !ReferenceEquals(contextualRoot, node))
            {
                if (TryGetCachedBoundNode(contextualRoot) is not { } contextualBoundRoot)
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
                    ClearCachedBoundNodes(rebindRoot);
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

            if (TryGetCachedBoundNode(node) is { } cachedNode)
                return cachedNode;

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

    private static bool TryGetContextualBindingRoot(SyntaxNode node, out SyntaxNode root)
    {
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

        return !ReferenceEquals(root, node);
    }

    private void ClearCachedBoundNodes(SyntaxNode node)
    {
        RemoveCachedBoundNode(node);
        foreach (var child in node.DescendantNodes())
            RemoveCachedBoundNode(child);
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

    private static SyntaxNode GetFunctionExpressionRebindRoot(FunctionExpressionSyntax functionExpression)
    {
        ExpressionSyntax? enclosingExpression = null;

        for (var current = functionExpression.Parent; current is not null; current = current.Parent)
        {
            if (current is FunctionExpressionSyntax)
                continue;

            if (current is StatementSyntax statement)
                return statement;

            if (enclosingExpression is null && current is ExpressionSyntax expression)
                enclosingExpression = expression;
        }

        return (SyntaxNode?)enclosingExpression ?? functionExpression;
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
            var boundRoot = GetBoundNode(contextualRoot, BoundTreeView.Original);
            if (TryFindBoundNodeBySyntax(boundRoot, functionExpression, out var contextualBoundNode) &&
                contextualBoundNode is BoundFunctionExpression contextualFunction)
            {
                CacheBoundNode(functionExpression, contextualFunction, GetBinder(functionExpression));
                boundFunction = contextualFunction;
                return true;
            }
        }

        boundFunction = null!;
        return false;
    }

    public IParameterSymbol? GetFunctionExpressionParameterSymbol(ParameterSyntax parameterSyntax)
    {
        EnsureDeclarations();
        EnsureRootBinderCreated();
        EnsureDiagnosticsCollected();

        if (parameterSyntax.Ancestors().OfType<FunctionExpressionSyntax>().FirstOrDefault() is not { } functionExpression)
            return GetDeclaredSymbol(parameterSyntax) as IParameterSymbol;

        if (TryGetContextualBindingRoot(functionExpression, out var contextualRoot) &&
            !ReferenceEquals(contextualRoot, functionExpression))
        {
            ClearCachedBoundNodes(contextualRoot);
            var reboundRoot = GetBoundNode(contextualRoot, BoundTreeView.Original);
            if (TryFindBoundNodeBySyntax(reboundRoot, functionExpression, out var reboundFunctionNode) &&
                reboundFunctionNode is BoundFunctionExpression reboundLambda &&
                TryGetFunctionParameterBySyntax(functionExpression, parameterSyntax, reboundLambda.Parameters, out var reboundParameter))
            {
                CacheBoundNode(functionExpression, reboundLambda, GetBinder(functionExpression));
                return reboundParameter;
            }
        }

        if (TryGetContextualBoundFunctionExpression(functionExpression, out var contextualLambda) &&
            TryGetFunctionParameterBySyntax(functionExpression, parameterSyntax, contextualLambda.Parameters, out var contextualParameter))
        {
            return contextualParameter;
        }

        if (GetBoundNode(functionExpression) is BoundFunctionExpression boundLambda &&
            TryGetFunctionParameterBySyntax(functionExpression, parameterSyntax, boundLambda.Parameters, out var boundParameter))
        {
            return boundParameter;
        }

        var functionSymbolInfo = GetSymbolInfo(functionExpression);
        var functionSymbol = functionSymbolInfo.Symbol ?? functionSymbolInfo.CandidateSymbols.FirstOrDefault();
        if (functionSymbol is IMethodSymbol lambdaMethod &&
            TryGetFunctionParameterBySyntax(functionExpression, parameterSyntax, lambdaMethod.Parameters, out var lambdaParameter))
        {
            return lambdaParameter;
        }

        return GetDeclaredSymbol(parameterSyntax) as IParameterSymbol;
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
            if (!_binderCache.TryGetValue(node.Parent, out actualParentBinder) &&
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

    private void CacheBinder(SyntaxNode node, Binder binder)
    {
        _binderCache[node] = binder;
        if (CanUseStructuralBinderCache(node))
            _binderCacheByKey[GetSyntaxNodeMapKey(node)] = binder;
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
        if (_rootBinderCreated)
            return;

        var root = SyntaxTree.GetRoot();
        _ = GetBinder(root);
        _rootBinderCreated = true;
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
