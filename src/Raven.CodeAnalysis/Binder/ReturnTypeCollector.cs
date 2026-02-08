using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class ReturnTypeCollector
{
    public static ITypeSymbol? InferAsync(Compilation compilation, BoundNode node)
    {
        var inferred = Infer(node);
        return AsyncReturnTypeUtilities.InferAsyncReturnType(compilation, inferred);
    }

    public static ITypeSymbol? Infer(BoundNode node)
    {
        var collector = new Collector();
        collector.Visit(node);

        if (node is BoundBlockStatement block &&
            block.Statements.LastOrDefault() is BoundExpressionStatement exprStmt &&
            exprStmt.Expression.Type is ITypeSymbol tailType)
        {
            collector.AddInferredType(tailType);
        }

        var collected = collector.GetResult();

        if (collected is null && node is BoundExpression expression &&
            expression.Type is ITypeSymbol { TypeKind: not TypeKind.Error } type)
        {
            return TypeSymbolNormalization.NormalizeForInference(type);
        }

        return collected;
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
            // Collapse literal types to their underlying primitive types so return
            // type inference doesn't treat each distinct literal as its own type.
            if (type is LiteralTypeSymbol literal)
                type = literal.UnderlyingType;

            if (type is ITypeUnionSymbol union)
            {
                foreach (var t in union.Types)
                    AddType(t);
            }
            else
            {
                _types.Add(type);
            }
        }

        public void AddInferredType(ITypeSymbol type)
        {
            AddType(type);
        }

        public ITypeSymbol? GetResult()
        {
            if (_types.Count == 0)
                return null;

            if (_types.Count == 1)
                return TypeSymbolNormalization.NormalizeForInference(_types.First());

            return TypeSymbolNormalization.NormalizeUnion(_types);
        }
    }
}
