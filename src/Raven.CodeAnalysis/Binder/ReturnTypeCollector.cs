using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class ReturnTypeCollector
{
    public static ITypeSymbol? Infer(BoundNode node)
    {
        var collector = new Collector();
        collector.Visit(node);
        return collector.GetResult();
    }

    private sealed class Collector : BoundTreeWalker
    {
        private readonly HashSet<ITypeSymbol> _types = new(SymbolEqualityComparer.Default);

        public override void VisitExpression(BoundExpression node)
        {
            if (node is BoundIfExpression ifExpr)
            {
                VisitExpression(ifExpr.Condition);
                VisitExpression(ifExpr.ThenBranch);
                if (ifExpr.ElseBranch is not null)
                    VisitExpression(ifExpr.ElseBranch);
            }
            else
            {
                base.VisitExpression(node);
            }
        }

        public override void VisitReturnStatement(BoundReturnStatement node)
        {
            if (node.Expression?.Type is ITypeSymbol type)
                AddType(type);

            base.VisitReturnStatement(node);
        }

        public override void VisitBlockExpression(BoundBlockExpression node)
        {
            BoundStatement? last = null;
            foreach (var statement in node.Statements)
            {
                VisitStatement(statement);
                last = statement;
            }

            if (last is BoundExpressionStatement exprStmt && exprStmt.Expression.Type is ITypeSymbol type)
                AddType(type);
        }

        public override void VisitLambdaExpression(BoundLambdaExpression node)
        {
            // Don't traverse into nested lambdas
        }

        public override void VisitExpressionStatement(BoundExpressionStatement node)
        {
            VisitExpression(node.Expression);
        }

        private void AddType(ITypeSymbol type)
        {
            if (type is IUnionTypeSymbol union)
            {
                foreach (var t in union.Types)
                    _types.Add(t);
            }
            else
            {
                _types.Add(type);
            }
        }

        public ITypeSymbol? GetResult()
        {
            if (_types.Count == 0)
                return null;
            if (_types.Count == 1)
                return _types.First();
            return new UnionTypeSymbol(_types, null, null, null, []);
        }
    }
}
