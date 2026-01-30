using System;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    public override BoundExpression? VisitWithExpression(BoundWithExpression node)
    {
        var compilation = GetCompilation();
        var loweredReceiver = (BoundExpression)VisitExpression(node.Receiver)!;
        var receiverType = loweredReceiver.Type ?? compilation.ErrorTypeSymbol;

        var statements = new List<BoundStatement>();

        var receiverTemp = CreateTempLocal("withReceiver", receiverType, isMutable: true);
        var receiverAccess = new BoundLocalAccess(receiverTemp);

        statements.Add(new BoundLocalDeclarationStatement(
            ImmutableArray.Create(new BoundVariableDeclarator(receiverTemp, loweredReceiver))));

        switch (node.Strategy)
        {
            case BoundWithStrategyKind.UpdateMethod:
            case BoundWithStrategyKind.WithMethod:
                return LowerWithConventionCall(node, receiverAccess, statements, compilation);
            case BoundWithStrategyKind.WithMemberMethods:
                return LowerWithMemberMethods(node, receiverAccess, statements, compilation);
            case BoundWithStrategyKind.RecordClone:
            case BoundWithStrategyKind.Clone:
                return LowerWithClone(node, receiverAccess, statements, compilation);
            default:
                return node;
        }
    }

    private BoundExpression LowerWithConventionCall(
        BoundWithExpression node,
        BoundLocalAccess receiverAccess,
        List<BoundStatement> statements,
        Compilation compilation)
    {
        var valueTemps = new Dictionary<ISymbol, ILocalSymbol>(SymbolEqualityComparer.Default);

        foreach (var assignment in node.Assignments)
        {
            var loweredValue = (BoundExpression)VisitExpression(assignment.Value)!;
            var valueType = loweredValue.Type ?? compilation.ErrorTypeSymbol;
            var valueTemp = CreateTempLocal("withValue", valueType, isMutable: true);
            valueTemps[assignment.Member] = valueTemp;

            statements.Add(new BoundLocalDeclarationStatement(
                ImmutableArray.Create(new BoundVariableDeclarator(valueTemp, loweredValue))));
        }

        var method = node.Method ?? throw new InvalidOperationException("With-expression method strategy requires a method.");
        var arguments = ImmutableArray.CreateBuilder<BoundExpression>(method.Parameters.Length);

        for (var i = 0; i < method.Parameters.Length; i++)
        {
            var parameter = method.Parameters[i];
            var member = node.ParameterMembers[i];

            BoundExpression argument = valueTemps.TryGetValue(member, out var temp)
                ? new BoundLocalAccess(temp)
                : new BoundMemberAccessExpression(receiverAccess, member);

            argument = ApplyConversionIfNeeded(argument, parameter.Type, compilation);
            arguments.Add(argument);
        }

        var invocation = compilation.BoundNodeFactory.CreateInvocationExpression(method, arguments.ToImmutable(), receiverAccess);
        statements.Add(new BoundExpressionStatement(invocation));
        return compilation.BoundNodeFactory.CreateBlockExpression(statements);
    }

    private BoundExpression LowerWithMemberMethods(
        BoundWithExpression node,
        BoundLocalAccess receiverAccess,
        List<BoundStatement> statements,
        Compilation compilation)
    {
        BoundExpression current = receiverAccess;

        for (var i = 0; i < node.Assignments.Length; i++)
        {
            var assignment = node.Assignments[i];
            var method = node.MemberMethods[i];

            var loweredValue = (BoundExpression)VisitExpression(assignment.Value)!;
            loweredValue = ApplyConversionIfNeeded(loweredValue, method.Parameters[0].Type, compilation);

            current = compilation.BoundNodeFactory.CreateInvocationExpression(method, [loweredValue], current);
        }

        statements.Add(new BoundExpressionStatement(current));
        return compilation.BoundNodeFactory.CreateBlockExpression(statements);
    }

    private BoundExpression LowerWithClone(
        BoundWithExpression node,
        BoundLocalAccess receiverAccess,
        List<BoundStatement> statements,
        Compilation compilation)
    {
        BoundExpression cloneExpression;
        if (node.CopyConstructor is not null)
        {
            cloneExpression = new BoundObjectCreationExpression(
                node.CopyConstructor,
                ImmutableArray.Create<BoundExpression>(receiverAccess));
        }
        else if (node.CloneMethod is not null)
        {
            cloneExpression = compilation.BoundNodeFactory.CreateInvocationExpression(
                node.CloneMethod,
                ImmutableArray<BoundExpression>.Empty,
                receiverAccess);
        }
        else
        {
            return compilation.BoundNodeFactory.CreateBlockExpression(statements);
        }

        var cloneType = cloneExpression.Type ?? compilation.ErrorTypeSymbol;
        var cloneTemp = CreateTempLocal("withClone", cloneType, isMutable: true);
        var cloneAccess = new BoundLocalAccess(cloneTemp);

        statements.Add(new BoundLocalDeclarationStatement(
            ImmutableArray.Create(new BoundVariableDeclarator(cloneTemp, cloneExpression))));

        foreach (var assignment in node.Assignments)
        {
            var loweredValue = (BoundExpression)VisitExpression(assignment.Value)!;
            var memberType = GetMemberType(assignment.Member, compilation);
            loweredValue = ApplyConversionIfNeeded(loweredValue, memberType, compilation);

            BoundExpression assignmentExpression = assignment.Member switch
            {
                IPropertySymbol property => compilation.BoundNodeFactory.CreatePropertyAssignmentExpression(cloneAccess, property, loweredValue),
                IFieldSymbol field => compilation.BoundNodeFactory.CreateFieldAssignmentExpression(cloneAccess, field, loweredValue),
                _ => loweredValue
            };

            statements.Add(new BoundExpressionStatement(assignmentExpression));
        }

        statements.Add(new BoundExpressionStatement(cloneAccess));
        return compilation.BoundNodeFactory.CreateBlockExpression(statements);
    }

}
