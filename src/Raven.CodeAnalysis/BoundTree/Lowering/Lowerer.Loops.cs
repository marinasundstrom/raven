using System;
using System.Collections.Generic;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
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
        return new BoundGotoStatement(breakLabel);
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
        return new BoundGotoStatement(continueLabel, isBackward: true);
    }
}
