using System;
using System.Collections.Generic;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed class Lowerer : BoundTreeRewriter
{
    private readonly ISymbol _containingSymbol;
    private readonly Stack<(ILabelSymbol BreakLabel, ILabelSymbol ContinueLabel)> _loopStack = new();
    private int _labelCounter;

    private Lowerer(ISymbol containingSymbol)
    {
        _containingSymbol = containingSymbol;
    }

    public static BoundBlockStatement LowerBlock(ISymbol containingSymbol, BoundBlockStatement block)
    {
        var lowerer = new Lowerer(containingSymbol);
        return (BoundBlockStatement)lowerer.VisitStatement(block);
    }

    public static BoundStatement LowerStatement(ISymbol containingSymbol, BoundStatement statement)
    {
        var lowerer = new Lowerer(containingSymbol);
        return (BoundStatement)lowerer.VisitStatement(statement);
    }

    public override BoundNode? VisitBlockStatement(BoundBlockStatement node)
    {
        var statements = new List<BoundStatement>();

        foreach (var statement in node.Statements)
        {
            statements.Add((BoundStatement)VisitStatement(statement));
        }

        return new BoundBlockStatement(statements, node.LocalsToDispose);
    }

    public override BoundNode? VisitIfStatement(BoundIfStatement node)
    {
        var condition = (BoundExpression)VisitExpression(node.Condition)!;
        var thenStatement = (BoundStatement)VisitStatement(node.ThenNode);
        var elseStatement = node.ElseNode is null ? null : (BoundStatement)VisitStatement(node.ElseNode);

        if (elseStatement is null)
        {
            var endLabel = CreateLabel("if_end");
            return new BoundBlockStatement([
                new BoundConditionalGotoStatement(endLabel, condition, jumpIfTrue: false),
                thenStatement,
                CreateLabelStatement(endLabel),
            ]);
        }
        else
        {
            var elseLabel = CreateLabel("if_else");
            var endLabel = CreateLabel("if_end");
            return new BoundBlockStatement([
                new BoundConditionalGotoStatement(elseLabel, condition, jumpIfTrue: false),
                thenStatement,
                new BoundGotoStatement(endLabel),
                new BoundLabeledStatement(elseLabel, elseStatement),
                CreateLabelStatement(endLabel),
            ]);
        }
    }

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

    private ILabelSymbol CreateLabel(string prefix)
    {
        var name = $"{prefix}_{_labelCounter++}";
        var containingType = _containingSymbol.ContainingType as INamedTypeSymbol;
        var containingNamespace = _containingSymbol.ContainingNamespace;
        return new LabelSymbol(name, _containingSymbol, containingType, containingNamespace,
            [Location.None], Array.Empty<SyntaxReference>());
    }

    private static BoundStatement CreateLabelStatement(ILabelSymbol label)
    {
        return new BoundLabeledStatement(label, new BoundBlockStatement(Array.Empty<BoundStatement>()));
    }
}
