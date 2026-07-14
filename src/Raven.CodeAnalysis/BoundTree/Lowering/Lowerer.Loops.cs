namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    public override BoundNode? VisitWhileStatement(BoundWhileStatement node)
    {
        var breakLabel = CreateLabel("while_break");
        var continueLabel = CreateLabel("while_continue");

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

    public override BoundNode? VisitBreakStatement(BoundBreakStatement node)
    {
        if (_loopStack.Count == 0)
            return base.VisitBreakStatement(node);

        var (breakLabel, _) = _loopStack.Peek();
        return new BoundGotoStatement(breakLabel);
    }

    public override BoundNode? VisitContinueStatement(BoundContinueStatement node)
    {
        if (_loopStack.Count == 0)
            return base.VisitContinueStatement(node);

        var (_, continueLabel) = _loopStack.Peek();
        return new BoundGotoStatement(continueLabel, isBackward: true);
    }
}
