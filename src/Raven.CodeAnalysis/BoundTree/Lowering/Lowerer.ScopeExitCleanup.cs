using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    // Run after propagation and structural control flow, but before optimization can
    // erase lexical scope boundaries. Function bodies have their own lowering pass.
    private BoundNode RewriteScopeExitCleanup(BoundNode body)
    {
        var rewriter = new ScopeExitCleanupRewriter(this);
        rewriter.Visit(body); // Index destination lifetimes, including forward labels.
        if (!rewriter.HasResources)
            return body;
        rewriter.Indexing = false;
        return rewriter.Visit(body)!;
    }

    private sealed class ScopeExitCleanupRewriter(Lowerer owner) : BoundTreeRewriter
    {
        public bool Indexing { get; set; } = true;
        public bool HasResources => _handled.Count > 0;
        private readonly List<BoundVariableDeclarator> _active = new();
        private readonly Dictionary<ILabelSymbol, ImmutableArray<BoundVariableDeclarator>> _labels = new(SymbolEqualityComparer.Default);
        private readonly HashSet<ILocalSymbol> _handled = new(SymbolEqualityComparer.Default);
        private readonly Stack<int> _loops = new();

        public override BoundNode? VisitFunctionExpression(BoundFunctionExpression node) => node;
        public override BoundNode? VisitFunctionStatement(BoundFunctionStatement node) => node;

        private ImmutableArray<ILocalSymbol> Remaining(ImmutableArray<ILocalSymbol> locals)
            => locals.Where(local => !_handled.Contains(local)).ToImmutableArray();

        private IEnumerable<BoundStatement> Cleanup(IEnumerable<BoundVariableDeclarator> resources)
            => owner.CreateDisposeStatements(resources.ToArray())
                .Select(statement => (BoundStatement)owner.VisitStatement(statement));

        public override BoundNode? VisitBlockStatement(BoundBlockStatement node)
        {
            var depth = _active.Count;
            var statements = node.Statements.Select(statement => (BoundStatement)VisitStatement(statement)).ToList();
            if (!Indexing)
                statements.AddRange(Cleanup(_active.Skip(depth)));
            _active.RemoveRange(depth, _active.Count - depth);
            return Indexing ? node : new BoundBlockStatement(statements, Remaining(node.LocalsToDispose));
        }

        public override BoundNode? VisitBlockExpression(BoundBlockExpression node)
        {
            var depth = _active.Count;
            var statements = node.Statements.Select(statement => (BoundStatement)VisitStatement(statement)).ToList();
            if (!Indexing && _active.Count > depth)
            {
                // A value block computes its result while resources are still alive.
                BoundExpression? result = null;
                if (statements.LastOrDefault() is BoundExpressionStatement tail &&
                    tail.Expression.Type.SpecialType != SpecialType.System_Void)
                {
                    var temporary = owner.CreateTempLocal("useResult", tail.Expression.Type, isMutable: false);
                    statements[^1] = new BoundLocalDeclarationStatement([new BoundVariableDeclarator(temporary, tail.Expression)]);
                    result = new BoundLocalAccess(temporary);
                }
                statements.AddRange(Cleanup(_active.Skip(depth)));
                if (result is not null)
                    statements.Add(new BoundExpressionStatement(result));
            }
            _active.RemoveRange(depth, _active.Count - depth);
            return Indexing ? node : new BoundBlockExpression(statements, node.UnitType, Remaining(node.LocalsToDispose), node.IntroduceILScope);
        }

        public override BoundNode? VisitLocalDeclarationStatement(BoundLocalDeclarationStatement node)
        {
            if (!node.IsUsing)
                return base.VisitLocalDeclarationStatement(node);

            var statements = new List<BoundStatement>();
            foreach (var declarator in node.Declarators)
            {
                // Initializer exits must not dispose this resource yet.
                var initialized = (BoundVariableDeclarator)VisitVariableDeclarator(declarator)!;
                statements.Add(new BoundLocalDeclarationStatement([initialized]));
                _active.Add(declarator);
                _handled.Add(declarator.Local);
            }
            return Indexing ? node : new BoundBlockStatement(statements);
        }

        public override BoundNode? VisitLabeledStatement(BoundLabeledStatement node)
        {
            if (Indexing)
                _labels[node.Label] = _active.ToImmutableArray();
            return base.VisitLabeledStatement(node);
        }

        private IEnumerable<BoundVariableDeclarator> Leaving(ILabelSymbol target)
        {
            var retained = _labels.TryGetValue(target, out var destination) ? destination : [];
            return _active.Where(resource => !retained.Any(other => SymbolEqualityComparer.Default.Equals(resource.Local, other.Local)));
        }

        private BoundStatement Transfer(BoundStatement transfer, IEnumerable<BoundVariableDeclarator> leaving)
        {
            if (Indexing)
                return transfer;
            var cleanup = Cleanup(leaving).ToList();
            if (cleanup.Count == 0)
                return transfer;
            cleanup.Add(transfer);
            return new BoundBlockStatement(cleanup);
        }

        public override BoundNode? VisitGotoStatement(BoundGotoStatement node)
            => Transfer(node, Leaving(node.Target));

        public override BoundNode? VisitConditionalGotoStatement(BoundConditionalGotoStatement node)
        {
            var condition = (BoundExpression)VisitExpression(node.Condition)!;
            if (Indexing)
                return node;
            var cleanup = Cleanup(Leaving(node.Target)).ToList();
            if (cleanup.Count == 0)
                return new BoundConditionalGotoStatement(node.Target, condition, node.JumpIfTrue);
            var skip = owner.CreateLabel("useSkipCleanup");
            cleanup.Insert(0, new BoundConditionalGotoStatement(skip, condition, !node.JumpIfTrue));
            cleanup.Add(new BoundGotoStatement(node.Target));
            cleanup.Add(CreateLabelStatement(skip));
            return new BoundBlockStatement(cleanup);
        }

        public override BoundNode? VisitReturnStatement(BoundReturnStatement node)
        {
            var expression = (BoundExpression?)VisitExpression(node.Expression);
            if (Indexing || _active.Count == 0)
                return node.Update(expression);
            var statements = new List<BoundStatement>();
            if (expression is not null)
            {
                if (expression.Type.SpecialType == SpecialType.System_Void)
                {
                    statements.Add(new BoundExpressionStatement(expression));
                    expression = null;
                }
                else
                {
                    var temporary = owner.CreateTempLocal("useReturn", expression.Type, isMutable: false);
                    statements.Add(new BoundLocalDeclarationStatement([new BoundVariableDeclarator(temporary, expression)]));
                    expression = new BoundLocalAccess(temporary);
                }
            }
            statements.AddRange(Cleanup(_active));
            statements.Add(new BoundReturnStatement(expression));
            return new BoundBlockStatement(statements);
        }

        public override BoundNode? VisitReturnExpression(BoundReturnExpression node)
        {
            var returned = (BoundStatement)VisitReturnStatement(new BoundReturnStatement(node.Expression))!;
            return returned is BoundReturnStatement simple
                ? node.Update(simple.Expression, node.Type)
                : new BoundBlockExpression([returned], node.Type);
        }

        // For-loops may remain structured until a backend selects its iteration plan.
        public override BoundNode? VisitForStatement(BoundForStatement node)
        {
            _loops.Push(_active.Count);
            try { return base.VisitForStatement(node); }
            finally { _loops.Pop(); }
        }

        private IEnumerable<BoundVariableDeclarator> LeavingLoop(ILabelSymbol? target)
            => target is not null ? Leaving(target) : _active.Skip(_loops.Peek());

        public override BoundNode? VisitBreakStatement(BoundBreakStatement node)
            => Transfer(node, LeavingLoop(node.TargetLabel));

        public override BoundNode? VisitContinueStatement(BoundContinueStatement node)
            => Transfer(node, LeavingLoop(node.TargetLabel));

        public override BoundNode? VisitBreakExpression(BoundBreakExpression node)
            => new BoundBlockExpression([Transfer(new BoundBreakStatement(node.TargetLabel), LeavingLoop(node.TargetLabel))], node.Type);

        public override BoundNode? VisitContinueExpression(BoundContinueExpression node)
            => new BoundBlockExpression([Transfer(new BoundContinueStatement(node.TargetLabel), LeavingLoop(node.TargetLabel))], node.Type);
    }
}
