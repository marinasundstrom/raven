using System.Collections.Generic;
using System.Linq;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    public override BoundNode? VisitBlockStatement(BoundBlockStatement node)
    {
        if (_iteratorState is null && ContainsYield(node))
            return RewriteIteratorBlock(node);

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

    private BoundNode RewriteIteratorBlock(BoundBlockStatement node)
    {
        if (_containingSymbol is not IMethodSymbol method)
            return base.VisitBlockStatement(node);

        if (!TryGetIteratorElementType(method.ReturnType, out var elementType))
            return base.VisitBlockStatement(node);

        var compilation = GetCompilation();
        var listDefinition = compilation.GetTypeByMetadataName("System.Collections.Generic.List`1") as INamedTypeSymbol;
        var constructedList = listDefinition?.Construct(elementType);
        var constructedNamedList = constructedList as INamedTypeSymbol;

        ITypeSymbol builderType = constructedList ?? compilation.ErrorTypeSymbol;

        var builderLocal = CreateTempLocal("yieldResult", builderType, isMutable: true);

        BoundExpression initializer;
        if (constructedNamedList is not null)
        {
            var ctor = constructedNamedList.Constructors.FirstOrDefault(c => c.Parameters.Length == 0);
            initializer = ctor is not null
                ? new BoundObjectCreationExpression(ctor, Array.Empty<BoundExpression>())
                : new BoundErrorExpression(builderType, null, BoundExpressionReason.OtherError);
        }
        else
        {
            initializer = new BoundErrorExpression(builderType, null, BoundExpressionReason.OtherError);
        }

        var statements = new List<BoundStatement>
        {
            new BoundLocalDeclarationStatement([new BoundVariableDeclarator(builderLocal, initializer)])
        };

        var previousState = _iteratorState;
        var addMethod = constructedNamedList?
            .GetMembers("Add")
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m => m.Parameters.Length == 1);
        _iteratorState = new IteratorState(builderLocal, addMethod, elementType, method.ReturnType);

        foreach (var statement in node.Statements)
        {
            var lowered = (BoundStatement)VisitStatement(statement);
            statements.Add(lowered);
        }

        var compilationForReturn = GetCompilation();
        BoundExpression returnExpression = new BoundLocalAccess(builderLocal);
        returnExpression = ApplyConversionIfNeeded(returnExpression, method.ReturnType, compilationForReturn);
        statements.Add(new BoundReturnStatement(returnExpression));

        _iteratorState = previousState;

        return new BoundBlockStatement(statements, node.LocalsToDispose);
    }

    public override BoundNode? VisitYieldReturnStatement(BoundYieldReturnStatement node)
    {
        if (_iteratorState is null)
            return base.VisitYieldReturnStatement(node);

        var expression = (BoundExpression)VisitExpression(node.Expression)!;
        var state = _iteratorState.Value;
        var compilation = GetCompilation();
        expression = ApplyConversionIfNeeded(expression, state.ElementType, compilation);

        if (state.AddMethod is null)
            return new BoundExpressionStatement(expression);

        var receiver = new BoundLocalAccess(state.BuilderLocal);
        var invocation = new BoundInvocationExpression(state.AddMethod, new[] { expression }, receiver);
        return new BoundExpressionStatement(invocation);
    }

    public override BoundNode? VisitYieldBreakStatement(BoundYieldBreakStatement node)
    {
        if (_iteratorState is null)
            return base.VisitYieldBreakStatement(node);

        var state = _iteratorState.Value;
        BoundExpression returnExpression = new BoundLocalAccess(state.BuilderLocal);
        var compilation = GetCompilation();
        returnExpression = ApplyConversionIfNeeded(returnExpression, state.ReturnType, compilation);
        return new BoundReturnStatement(returnExpression);
    }
}

