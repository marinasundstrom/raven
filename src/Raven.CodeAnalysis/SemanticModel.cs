using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    private static readonly CompletionService s_completionService = new();
    private readonly Dictionary<SyntaxNode, Binder> _binderCache = new();
    private readonly Dictionary<SyntaxNode, SymbolInfo> _symbolMappings = new();
    private readonly Dictionary<SyntaxNode, BoundNode> _boundNodeCache = new();
    private readonly Dictionary<SyntaxNode, (Binder, BoundNode)> _boundNodeCache2 = new Dictionary<SyntaxNode, (Binder, BoundNode)>();
    private readonly Dictionary<SyntaxNode, BoundNode> _loweredBoundNodeCache = new();
    private readonly Dictionary<SyntaxNode, (Binder, BoundNode)> _loweredBoundNodeCache2 = new Dictionary<SyntaxNode, (Binder, BoundNode)>();

    private readonly Dictionary<BoundNode, SyntaxNode> _syntaxCache = new(ReferenceEqualityComparer.Instance);
    private readonly Dictionary<BoundNode, SyntaxNode> _loweredSyntaxCache = new(ReferenceEqualityComparer.Instance);
    private IImmutableList<Diagnostic>? _diagnostics;
    private readonly DiagnosticBag _declarationDiagnostics = new();
    private bool _declarationsComplete;
    private bool _rootBinderCreated;

    public bool IsDebuggingEnabled { get; set; } = true;

    public SemanticModel(Compilation compilation, SyntaxTree syntaxTree)
    {
        Compilation = compilation;
        SyntaxTree = syntaxTree;
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
        var root = SyntaxTree.GetRoot();
        var binder = GetBinder(root);

        Traverse(root, binder);

        DocumentationCommentValidator.Analyze(this, root, binder.Diagnostics);

        void Traverse(SyntaxNode node, Binder currentBinder)
        {
            foreach (var child in node.ChildNodes())
            {
                var childBinder = GetBinder(child, currentBinder);

                if (child is AttributeSyntax attributeSyntax)
                {
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
                    continue;
                }

                if (child is ExpressionSyntax || child is StatementSyntax)
                {
                    childBinder.GetOrBind(child);
                    continue;
                }

                Traverse(child, childBinder);
            }
        }
    }

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

        if (node is ExpressionSyntax expression)
        {
            EnsureDiagnosticsCollected();
            var boundExpression = GetBoundNode(expression);
            info = boundExpression.GetSymbolInfo();
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

        _symbolMappings[node] = info;
        return info;
    }

    /// <summary>
    /// Given a syntax node that declares a method, property, or member accessor, get the corresponding symbol.
    /// </summary>
    /// <param name="node"></param>
    /// <returns></returns>
    public ISymbol? GetDeclaredSymbol(SyntaxNode node)
    {
        var binder = GetBinder(node);

        if (node is UnionCaseClauseSyntax caseClause && _unionCaseSymbols.TryGetValue(caseClause, out var caseSymbol))
            return caseSymbol;

        if (Compilation.DeclarationTable.TryGetDeclKey(node, out var key))
        {
            return Compilation.SymbolFactory.GetOrCreate(key, () =>
            {
                return (Symbol)binder.BindDeclaredSymbol(node)!;
            });
        }

        return binder.BindDeclaredSymbol(node);
    }

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

        var binder = GetBinder(expr);

        var boundExpr = binder.BindExpression(expr);

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

    /// <summary>
    /// Gets type information about a type syntax.
    /// </summary>
    /// <param name="typeSyntax">The type syntax node.</param>
    public TypeInfo GetTypeInfo(TypeSyntax typeSyntax)
    {
        var binder = GetBinder(typeSyntax);
        try
        {
            var type = binder.BindTypeSyntaxDirect(typeSyntax);
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

        if (view is BoundTreeView.Original)
        {
            var binder = GetBinder(node);
            return binder.GetOrBind(node);
        }

        if (TryGetCachedLoweredBoundNode(node) is { } loweredCached)
            return loweredCached;

        var binderForLowering = GetBinder(node);
        var boundNode = binderForLowering.GetOrBind(node);
        var loweredNode = LowerBoundNode(binderForLowering, boundNode);
        CacheLoweredBoundNode(node, loweredNode, binderForLowering);
        return loweredNode;
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

    private BoundNode LowerBoundNode(Binder binder, BoundNode boundNode)
    {
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

        if (_binderCache.TryGetValue(node, out var existingBinder))
        {
            if (parentBinder is not null &&
                !ReferenceEquals(existingBinder.ParentBinder, parentBinder) &&
                (parentBinder is LambdaBinder || parentBinder.ContainingSymbol is ILambdaSymbol))
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
            _binderCache[cu] = binder;
            return binder;
        }

        // Ensure parent binder is constructed and cached first
        Binder? actualParentBinder = parentBinder;

        if (actualParentBinder == null)
        {
            if (!_binderCache.TryGetValue(node.Parent, out actualParentBinder))
            {
                // Recursively create and cache the parent binder first
                actualParentBinder = GetBinder(node.Parent);
            }
        }

        var newBinder = Compilation.BinderFactory.GetBinder(node, actualParentBinder);

        _binderCache[node] = newBinder;
        return newBinder;
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
