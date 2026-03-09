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

        if (node is BoundBlockStatement blockStatement)
        {
            CollectTailStatementTypes(blockStatement.Statements, collector);
        }
        else if (node is BoundBlockExpression blockExpression)
        {
            CollectTailStatementTypes(blockExpression.Statements, collector);
        }

        var collected = collector.GetResult();

        if (collected is null && node is BoundExpression expression &&
            expression.Type is ITypeSymbol { TypeKind: not TypeKind.Error } type)
        {
            return TypeSymbolNormalization.NormalizeForInference(type);
        }

        return collected;
    }

    private static bool IsValueReturningExpressionForInference(BoundExpressionStatement statement)
    {
        return statement.Expression is not (BoundAssignmentExpression or BoundErrorExpression);
    }

    private static void CollectTailStatementTypes(
        IEnumerable<BoundStatement> statements,
        Collector collector)
    {
        var statementList = statements as IReadOnlyList<BoundStatement> ?? statements.ToArray();
        if (statementList.Count == 0)
            return;

        if (statementList[^1] is BoundExpressionStatement exprStmt &&
            IsValueReturningExpressionForInference(exprStmt) &&
            exprStmt.Expression.Type is ITypeSymbol tailType)
        {
            collector.AddInferredType(tailType);
            return;
        }

        if (statementList[^1] is BoundIfStatement tailIf)
            CollectTailIfBranchTypes(tailIf, collector);
    }

    private static bool CollectTailIfBranchTypes(BoundIfStatement ifStatement, Collector collector)
    {
        if (ifStatement.ElseNode is null)
            return false;

        var thenCollected = TryCollectBranchType(ifStatement.ThenNode, collector);
        var elseCollected = TryCollectBranchType(ifStatement.ElseNode, collector);
        return thenCollected && elseCollected;
    }

    private static bool TryCollectBranchType(BoundStatement branch, Collector collector)
    {
        switch (branch)
        {
            case BoundExpressionStatement expressionStatement
                when IsValueReturningExpressionForInference(expressionStatement) &&
                     expressionStatement.Expression.Type is ITypeSymbol expressionType &&
                     expressionType.TypeKind is not TypeKind.Error &&
                     expressionType.SpecialType is not SpecialType.System_Void and not SpecialType.System_Unit:
                collector.AddInferredType(expressionType);
                return true;

            case BoundBlockStatement block when block.Statements.LastOrDefault() is { } lastStatement:
                return TryCollectBranchType(lastStatement, collector);

            case BoundIfStatement nestedIf:
                return CollectTailIfBranchTypes(nestedIf, collector);

            default:
                return false;
        }
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

        public override void VisitFunctionExpression(BoundFunctionExpression node)
        {
            // Don't traverse into nested lambdas
        }

        public override void VisitExpressionStatement(BoundExpressionStatement node)
        {
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
