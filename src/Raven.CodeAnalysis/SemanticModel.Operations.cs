using System;
using System.Collections.Generic;
using System.Threading;

using Raven.CodeAnalysis.Operations;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    private readonly Dictionary<SyntaxNode, IOperation> _operationCache = new();

    /// <summary>
    /// Gets the semantic operation corresponding to the specified syntax node.
    /// </summary>
    public IOperation? GetOperation(SyntaxNode node, CancellationToken cancellationToken = default)
    {
        if (node is null)
            throw new ArgumentNullException(nameof(node));

        cancellationToken.ThrowIfCancellationRequested();

        if (_operationCache.TryGetValue(node, out var cached))
            return cached;

        EnsureDiagnosticsCollected();

        BoundNode? bound = TryGetCachedBoundNode(node);

        if (bound is null)
        {
            bound = node switch
            {
                ExpressionSyntax or StatementSyntax => GetBoundNode(node),
                _ => null
            };
        }

        if (bound is null && node is InterpolatedStringContentSyntax && node.Parent is InterpolatedStringExpressionSyntax parentExpression)
            bound = GetBoundNode(parentExpression);

        if (bound is null)
            return null;

        var operation = OperationFactory.Create(this, node, bound);

        if (operation is null)
            return null;

        _operationCache[node] = operation;
        return operation;
    }
}
