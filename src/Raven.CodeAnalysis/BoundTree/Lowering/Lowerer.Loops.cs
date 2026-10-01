using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    // Ordinary vector iteration lowers once into existing language nodes. Both the
    // general .NET generator and target-neutral body planner consume this form.
    private bool CanLowerArrayFor(BoundForStatement node) =>
        _containingSymbol.ContainingAssembly is SourceAssemblySymbol &&
        node.Iteration is { Kind: ForIterationKind.Array, ArrayType: { Rank: 1 } array } &&
        node.Local is { } local && SymbolEqualityComparer.Default.Equals(local.Type, array.ElementType);

    public override BoundNode? VisitForStatement(BoundForStatement node)
    {
        if (!CanLowerArrayFor(node))
        {
            // A loop retained for general codegen owns its unlabeled transfers.
            // Do not redirect them into an enclosing lowered while/vector loop.
            _loopStack.Push((null, null));
            try { return base.VisitForStatement(node); }
            finally { _loopStack.Pop(); }
        }
        return LowerArrayForStatement(node, CreateLabel("for_break"), CreateLabel("for_continue"));
    }

    private BoundStatement LowerArrayForStatement(BoundForStatement node, ILabelSymbol breakLabel, ILabelSymbol continueLabel)
    {
        var compilation = ((SourceAssemblySymbol)_containingSymbol.ContainingAssembly!).Compilation;
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var unitType = compilation.GetSpecialType(SpecialType.System_Unit);
        var arrayType = node.Iteration.ArrayType!;
        var array = CreateTempLocal("forArray", arrayType, isMutable: false);
        var index = CreateTempLocal("forIndex", intType, isMutable: true);
        var begin = CreateLabel("for_begin");
        var collection = (BoundExpression)VisitExpression(node.Collection)!;
        BoundStatement body;
        _loopStack.Push((breakLabel, continueLabel));
        try { body = (BoundStatement)VisitStatement(node.Body)!; }
        finally { _loopStack.Pop(); }
        BoundExpression Number(int value) => new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, value, intType);
        BoundExpression Binary(BoundExpression left, SyntaxKind kind, BoundExpression right)
        {
            if (!BoundBinaryOperator.TryLookup(compilation, kind, left.Type, right.Type, out var op))
                throw new InvalidOperationException("Missing built-in vector-loop operator");
            return new BoundBinaryExpression(left, op, right);
        }
        var length = compilation.GetSpecialType(SpecialType.System_Array).GetMembers("Length").OfType<IPropertySymbol>().Single();
        return new BoundBlockStatement([
            new BoundLocalDeclarationStatement([new BoundVariableDeclarator(array, collection)]),
            new BoundLocalDeclarationStatement([new BoundVariableDeclarator(index, Number(0))]),
            CreateLabelStatement(begin),
            new BoundConditionalGotoStatement(breakLabel,
                Binary(new BoundLocalAccess(index), SyntaxKind.LessThanToken,
                    new BoundMemberAccessExpression(new BoundLocalAccess(array), length)), jumpIfTrue: false),
            new BoundLocalDeclarationStatement([new BoundVariableDeclarator(node.Local!,
                new BoundArrayAccessExpression(new BoundLocalAccess(array), [new BoundLocalAccess(index)], arrayType.ElementType))]),
            body,
            CreateLabelStatement(continueLabel),
            new BoundAssignmentStatement(new BoundLocalAssignmentExpression(index, new BoundLocalAccess(index),
                Binary(new BoundLocalAccess(index), SyntaxKind.PlusToken, Number(1)), unitType)),
            new BoundGotoStatement(begin, isBackward: true),
            CreateLabelStatement(breakLabel)
        ]);
    }

    public override BoundNode? VisitWhileStatement(BoundWhileStatement node)
    {
        var breakLabel = CreateLabel("while_break");
        var continueLabel = CreateLabel("while_continue");

        return LowerWhileStatement(node, breakLabel, continueLabel);
    }

    private BoundStatement LowerWhileStatement(
        BoundWhileStatement node,
        ILabelSymbol breakLabel,
        ILabelSymbol continueLabel)
    {
        var condition = (BoundExpression)VisitExpression(node.Condition)!;

        _loopStack.Push((breakLabel, continueLabel));
        var body = (BoundStatement)VisitStatement(node.Body);
        _loopStack.Pop();

        return new BoundBlockStatement([
            new BoundLabeledStatement(continueLabel, new BoundBlockStatement([
                new BoundConditionalGotoStatement(breakLabel, condition, jumpIfTrue: false),
            ])),
            body,
            new BoundGotoStatement(continueLabel, isBackward: true),
            CreateLabelStatement(breakLabel),
        ]);
    }

    public override BoundNode? VisitLoopStatement(BoundLoopStatement node)
    {
        var breakLabel = CreateLabel("loop_break");
        var continueLabel = CreateLabel("loop_continue");

        return LowerLoopStatement(node, breakLabel, continueLabel);
    }

    private BoundStatement LowerLoopStatement(
        BoundLoopStatement node,
        ILabelSymbol breakLabel,
        ILabelSymbol continueLabel)
    {
        _loopStack.Push((breakLabel, continueLabel));
        var body = (BoundStatement)VisitStatement(node.Body);
        _loopStack.Pop();

        return new BoundBlockStatement([
            CreateLabelStatement(continueLabel),
            body,
            new BoundGotoStatement(continueLabel, isBackward: true),
            CreateLabelStatement(breakLabel),
        ]);
    }

    public override BoundNode? VisitLabeledStatement(BoundLabeledStatement node)
    {
        var labels = new List<ILabelSymbol>();
        BoundStatement current = node;
        while (current is BoundLabeledStatement labeled)
        {
            labels.Add(labeled.Label);
            current = labeled.Statement;
        }

        BoundStatement? loweredLoop = current switch
        {
            BoundForStatement forStatement when CanLowerArrayFor(forStatement) => LowerLabeledLoop(labels, "for", forStatement, static (lowerer, statement, breakLabel, continueLabel) =>
                lowerer.LowerArrayForStatement(statement, breakLabel, continueLabel)),
            BoundWhileStatement whileStatement => LowerLabeledLoop(labels, "while", whileStatement, static (lowerer, statement, breakLabel, continueLabel) =>
                lowerer.LowerWhileStatement(statement, breakLabel, continueLabel)),
            BoundLoopStatement loopStatement => LowerLabeledLoop(labels, "loop", loopStatement, static (lowerer, statement, breakLabel, continueLabel) =>
                lowerer.LowerLoopStatement(statement, breakLabel, continueLabel)),
            _ => null,
        };

        if (loweredLoop is null)
            return base.VisitLabeledStatement(node);

        return WrapLabels(labels, loweredLoop);
    }

    private BoundStatement LowerLabeledLoop<TStatement>(
        List<ILabelSymbol> labels,
        string labelPrefix,
        TStatement statement,
        Func<Lowerer, TStatement, ILabelSymbol, ILabelSymbol, BoundStatement> lower)
        where TStatement : BoundStatement
    {
        var breakLabel = CreateLabel($"{labelPrefix}_break");
        var continueLabel = CreateLabel($"{labelPrefix}_continue");

        foreach (var label in labels)
            _labeledLoopTargets[label] = (breakLabel, continueLabel);

        try
        {
            return lower(this, statement, breakLabel, continueLabel);
        }
        finally
        {
            foreach (var label in labels)
                _labeledLoopTargets.Remove(label);
        }
    }

    private static BoundStatement WrapLabels(List<ILabelSymbol> labels, BoundStatement statement)
    {
        for (var i = labels.Count - 1; i >= 0; i--)
            statement = new BoundLabeledStatement(labels[i], statement);

        return statement;
    }

    public override BoundNode? VisitBreakStatement(BoundBreakStatement node)
    {
        if (node.TargetLabel is { } targetLabel)
        {
            if (_labeledLoopTargets.TryGetValue(targetLabel, out var target))
                return new BoundGotoStatement(target.BreakLabel);

            return base.VisitBreakStatement(node);
        }

        if (_loopStack.Count == 0)
            return base.VisitBreakStatement(node);

        var (breakLabel, _) = _loopStack.Peek();
        return breakLabel is null ? base.VisitBreakStatement(node) : new BoundGotoStatement(breakLabel);
    }

    public override BoundNode? VisitContinueStatement(BoundContinueStatement node)
    {
        if (node.TargetLabel is { } targetLabel)
        {
            if (_labeledLoopTargets.TryGetValue(targetLabel, out var target))
                return new BoundGotoStatement(target.ContinueLabel, isBackward: true);

            return base.VisitContinueStatement(node);
        }

        if (_loopStack.Count == 0)
            return base.VisitContinueStatement(node);

        var (_, continueLabel) = _loopStack.Peek();
        return continueLabel is null ? base.VisitContinueStatement(node) : new BoundGotoStatement(continueLabel, isBackward: true);
    }

    public override BoundNode? VisitBreakExpression(BoundBreakExpression node)
    {
        ILabelSymbol? breakLabel = null;
        if (node.TargetLabel is { } targetLabel)
        {
            if (_labeledLoopTargets.TryGetValue(targetLabel, out var target))
                breakLabel = target.BreakLabel;
        }
        else if (_loopStack.Count > 0)
        {
            breakLabel = _loopStack.Peek().BreakLabel;
        }

        return breakLabel is null
            ? base.VisitBreakExpression(node)
            : CreateControlFlowExpression(new BoundGotoStatement(breakLabel), node.Type);
    }

    public override BoundNode? VisitContinueExpression(BoundContinueExpression node)
    {
        ILabelSymbol? continueLabel = null;
        if (node.TargetLabel is { } targetLabel)
        {
            if (_labeledLoopTargets.TryGetValue(targetLabel, out var target))
                continueLabel = target.ContinueLabel;
        }
        else if (_loopStack.Count > 0)
        {
            continueLabel = _loopStack.Peek().ContinueLabel;
        }

        return continueLabel is null
            ? base.VisitContinueExpression(node)
            : CreateControlFlowExpression(new BoundGotoStatement(continueLabel, isBackward: true), node.Type);
    }

    private static BoundBlockExpression CreateControlFlowExpression(BoundStatement transfer, ITypeSymbol type)
        => new([transfer], type, []);
}
