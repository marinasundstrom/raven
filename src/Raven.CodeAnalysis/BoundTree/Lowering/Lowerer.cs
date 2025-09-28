using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer : BoundTreeRewriter
{
    private readonly ISymbol _containingSymbol;
    private readonly Stack<(ILabelSymbol BreakLabel, ILabelSymbol ContinueLabel)> _loopStack = new();
    private int _labelCounter;
    private int _tempCounter;

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

    public override BoundNode? VisitInvocationExpression(BoundInvocationExpression node)
    {
        var receiver = (BoundExpression?)VisitExpression(node.Receiver);
        var arguments = node.Arguments.Select(a => (BoundExpression)VisitExpression(a)!).ToArray();

        BoundExpression? extensionReceiver = null;
        if (node.ExtensionReceiver is not null)
        {
            extensionReceiver = ReferenceEquals(node.ExtensionReceiver, node.Receiver)
                ? receiver
                : (BoundExpression?)VisitExpression(node.ExtensionReceiver);
        }

        if (node.Method.IsExtensionMethod && extensionReceiver is not null)
        {
            var loweredArguments = new BoundExpression[arguments.Length + 1];
            loweredArguments[0] = extensionReceiver;
            Array.Copy(arguments, 0, loweredArguments, 1, arguments.Length);
            return new BoundInvocationExpression(node.Method, loweredArguments, receiver: null);
        }

        return new BoundInvocationExpression(node.Method, arguments, receiver, extensionReceiver);
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

    private SourceLocalSymbol CreateTempLocal(string nameHint, ITypeSymbol type, bool isMutable)
    {
        var containingType = _containingSymbol.ContainingType as INamedTypeSymbol;
        var containingNamespace = _containingSymbol.ContainingNamespace;
        var name = $"<{nameHint}>__{_tempCounter++}";
        return new SourceLocalSymbol(name, type, isMutable, _containingSymbol, containingType, containingNamespace,
            [Location.None], Array.Empty<SyntaxReference>());
    }

    private static BoundStatement CreateLabelStatement(ILabelSymbol label)
    {
        return new BoundLabeledStatement(label, new BoundBlockStatement(Array.Empty<BoundStatement>()));
    }

    private Compilation GetCompilation()
    {
        if (_containingSymbol.ContainingAssembly is SourceAssemblySymbol sourceAssembly)
            return sourceAssembly.Compilation;

        throw new InvalidOperationException("Lowering requires a source assembly containing symbol.");
    }

    private static BoundExpression ApplyConversionIfNeeded(BoundExpression expression, ITypeSymbol targetType, Compilation compilation)
    {
        if (targetType is null)
            return expression;

        var sourceType = expression.Type ?? compilation.ErrorTypeSymbol;

        if (SymbolEqualityComparer.Default.Equals(sourceType, targetType))
            return expression;

        var conversion = compilation.ClassifyConversion(sourceType, targetType);
        if (!conversion.Exists || conversion.IsIdentity)
            return expression;

        return new BoundCastExpression(expression, targetType, conversion);
    }
}
