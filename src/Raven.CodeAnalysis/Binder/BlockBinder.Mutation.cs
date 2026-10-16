using System.Collections.Generic;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

partial class BlockBinder
{
    // Capture locations before the read/modify/write sequence. Capture reference
    // receivers and indices, but recurse through value storage instead of copying it.
    private BoundExpression StabilizeMutation(BoundExpression result, BoundExpression target, SyntaxNode syntax)
    {
        if (result is BoundErrorExpression)
            return result;
        var statements = new List<BoundStatement>();
        var replacements = new Dictionary<BoundNode, BoundExpression>(ReferenceEqualityComparer.Instance);
        void Capture(BoundExpression value)
        {
            if (replacements.ContainsKey(value))
                return;
            var local = CreateTempLocal("mutationLocation", value.Type!, syntax);
            statements.Add(new BoundLocalDeclarationStatement([new BoundVariableDeclarator(local, value)]));
            replacements.Add(value, new BoundLocalAccess(local));
        }
        void Receiver(BoundExpression? receiver)
        {
            if (receiver is null or BoundTypeExpression)
                return;
            if (receiver.Type!.IsReferenceType)
                Capture(receiver);
            else
                Storage(receiver);
        }
        void Storage(BoundExpression storage)
        {
            switch (storage)
            {
                case BoundArrayAccessExpression array:
                    Capture(array.Receiver);
                    foreach (var index in array.Indices)
                        Capture(index);
                    break;
                case BoundIndexerAccessExpression indexer:
                    Receiver(indexer.Receiver);
                    foreach (var argument in indexer.Arguments)
                        Capture(argument);
                    break;
                case BoundFieldAccess field:
                    Receiver(field.Receiver);
                    break;
                case BoundMemberAccessExpression member:
                    Receiver(member.Receiver);
                    break;
            }
        }
        Storage(target);
        if (statements.Count == 0)
            return result;
        var rewritten = (BoundExpression)new MutationLocationRewriter(replacements).Visit(result)!;
        statements.Add(new BoundExpressionStatement(rewritten));
        return new BoundBlockExpression(statements, result.Type!);
    }

    private sealed class MutationLocationRewriter(Dictionary<BoundNode, BoundExpression> replacements) : BoundTreeRewriter
    {
        public override BoundNode? Visit(BoundNode? node)
            => node is not null && replacements.TryGetValue(node, out var replacement) ? replacement : base.Visit(node);
    }
}
