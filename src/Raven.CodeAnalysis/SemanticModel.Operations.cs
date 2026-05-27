using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Threading;

using Raven.CodeAnalysis.Operations;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    private readonly ConcurrentDictionary<SyntaxNode, IOperation> _operationCache = new();

    /// <summary>
    /// Gets the semantic operation corresponding to the specified syntax node.
    /// </summary>
    public IOperation? GetOperation(SyntaxNode node, CancellationToken cancellationToken = default)
    {
        if (node is null)
            throw new ArgumentNullException(nameof(node));

        using var semanticAccess = EnterSemanticAccess(cancellationToken);
        using var semanticQueryBinding = EnterSemanticQueryBinding();

        cancellationToken.ThrowIfCancellationRequested();

        if (_operationCache.TryGetValue(node, out var cached))
            return cached;

        EnsureBindingReadyForSemanticQuery();

        BoundNode? bound = TryGetCachedBoundNode(node);

        if (bound is null && node is PatternSyntax pattern)
        {
            BindPatternOwner(pattern);
            bound = TryGetCachedBoundNode(pattern);
        }

        if (bound is null)
        {
            bound = node switch
            {
                ExpressionSyntax or StatementSyntax => TryGetBoundNodeForSemanticQuery(node, out var queryBound)
                    ? queryBound
                    : null,
                _ => null
            };
        }

        if (bound is null && node is ArgumentSyntax argument)
            bound = TryGetBoundNodeForSemanticQuery(argument.Expression, out var argumentBound)
                ? argumentBound
                : null;

        if (bound is null && node is InterpolatedStringContentSyntax && node.Parent is InterpolatedStringExpressionSyntax parentExpression)
            bound = TryGetBoundNodeForSemanticQuery(parentExpression, out var interpolatedStringBound)
                ? interpolatedStringBound
                : null;

        if (bound is null)
            return null;

        var operation = OperationFactory.Create(this, node, bound);

        if (operation is null)
            return null;

        _operationCache[node] = operation;
        return operation;
    }
}
