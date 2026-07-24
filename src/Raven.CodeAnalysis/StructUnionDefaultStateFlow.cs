using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class StructUnionDefaultStateFlow
{
    // Narrow internal value-state analysis for struct-union match exhaustiveness.
    // This is intentionally not public AnalyzeDataFlow and not nullable-flow
    // analysis: it only tracks local struct-union carrier default state through
    // straight-line statements, if joins, loops, try/catch/finally, and nested
    // statement context.
    private enum DefaultState
    {
        Active,
        Default,
        MaybeDefault
    }

    public static bool ContentsMayBeDefault(
        BoundExpression expression,
        SyntaxNode matchSyntax,
        Func<SyntaxNode, BoundNode?> getBoundNode)
        => ExpressionMayBeDefault(expression, matchSyntax, getBoundNode);

    public static bool ExpressionMayBeDefault(
        BoundExpression expression,
        SyntaxNode expressionSyntax,
        Func<SyntaxNode, BoundNode?> getBoundNode)
        => GetExpressionState(expression, GetLocalStatesBefore(expressionSyntax, getBoundNode)) is DefaultState.Default or DefaultState.MaybeDefault;

    private static DefaultState GetExpressionState(
        BoundExpression expression,
        Dictionary<ILocalSymbol, DefaultState> localStates)
    {
        switch (expression)
        {
            case BoundDefaultValueExpression:
                return DefaultState.Default;
            case BoundParenthesizedExpression parenthesized:
                return GetExpressionState(parenthesized.Expression, localStates);
            case BoundConversionExpression conversion:
                return GetExpressionState(conversion.Expression, localStates);
            case BoundRequiredResultExpression required:
                return GetExpressionState(required.Operand, localStates);
            case BoundReturnExpression returnExpression:
                return returnExpression.Expression is not null
                    ? GetExpressionState(returnExpression.Expression, localStates)
                    : DefaultState.Active;
            case BoundBlockExpression blockExpression:
                return GetBlockExpressionState(blockExpression, localStates);
            case BoundUnionCaseExpression:
                return DefaultState.Active;
            case BoundMatchExpression matchExpression:
                return GetMatchExpressionState(matchExpression, localStates);
            case BoundLocalAccess localAccess:
                return localStates.TryGetValue(localAccess.Local, out var state) ? state : GetUnknownExpressionState(localAccess);
            case BoundParameterAccess:
            case BoundSelfExpression:
                return DefaultState.Active;
            case BoundFieldAccess:
            case BoundPropertyAccess:
                return GetMaybeDefaultExpressionState(expression);
            case BoundMemberAccessExpression { Member: IFieldSymbol or IPropertySymbol }:
                return GetMaybeDefaultExpressionState(expression);
            case BoundIfExpression ifExpression:
                {
                    var thenState = GetExpressionState(ifExpression.ThenBranch, localStates);
                    var elseState = ifExpression.ElseBranch is not null
                        ? GetExpressionState(ifExpression.ElseBranch, localStates)
                        : DefaultState.Active;

                    return Join(thenState, elseState);
                }
            default:
                return GetUnknownExpressionState(expression);
        }
    }

    private static DefaultState GetBlockExpressionState(
        BoundBlockExpression blockExpression,
        Dictionary<ILocalSymbol, DefaultState> localStates)
    {
        var blockStates = Clone(localStates);
        var statements = blockExpression.Statements.ToImmutableArray();

        for (var i = 0; i < statements.Length; i++)
        {
            var statement = statements[i];

            if (i == statements.Length - 1 && statement is BoundExpressionStatement expressionStatement)
                return GetExpressionState(expressionStatement.Expression, blockStates);

            ApplyStatement(statement, blockStates);
        }

        return GetUnknownExpressionState(blockExpression);
    }

    private static DefaultState GetMatchExpressionState(
        BoundMatchExpression matchExpression,
        Dictionary<ILocalSymbol, DefaultState> localStates)
    {
        if (matchExpression.Arms.IsDefaultOrEmpty)
            return GetUnknownExpressionState(matchExpression);

        DefaultState? result = null;

        foreach (var arm in matchExpression.Arms)
        {
            var armState = GetExpressionState(arm.Expression, localStates);
            result = result is { } current ? Join(current, armState) : armState;
        }

        return result ?? GetUnknownExpressionState(matchExpression);
    }

    private static Dictionary<ILocalSymbol, DefaultState> GetLocalStatesBefore(
        SyntaxNode matchSyntax,
        Func<SyntaxNode, BoundNode?> getBoundNode)
    {
        var states = new Dictionary<ILocalSymbol, DefaultState>(SymbolEqualityComparer.Default);

        foreach (var statement in GetStatementPath(matchSyntax))
        {
            foreach (var priorStatement in GetPriorStatements(statement))
            {
                if (getBoundNode(priorStatement) is BoundStatement boundStatement)
                    ApplyStatement(boundStatement, states);
            }
        }

        return states;
    }

    private static ImmutableArray<StatementSyntax> GetStatementPath(SyntaxNode matchSyntax)
    {
        var statements = matchSyntax.AncestorsAndSelf().OfType<StatementSyntax>().Reverse();
        return statements.ToImmutableArray();
    }

    private static ImmutableArray<StatementSyntax> GetPriorStatements(StatementSyntax statement)
    {
        var builder = ImmutableArray.CreateBuilder<StatementSyntax>();

        switch (statement.Parent)
        {
            case GlobalStatementSyntax globalStatement when globalStatement.Parent is CompilationUnitSyntax compilationUnit:
                foreach (var member in compilationUnit.Members)
                {
                    if (ReferenceEquals(member, globalStatement))
                        break;

                    if (member is GlobalStatementSyntax priorGlobal)
                        builder.Add(priorGlobal.Statement);
                }
                break;
            case BlockStatementSyntax block:
                foreach (var prior in block.Statements)
                {
                    if (ReferenceEquals(prior, statement))
                        break;

                    builder.Add(prior);
                }
                break;
            case BlockSyntax block:
                foreach (var prior in block.Statements)
                {
                    if (ReferenceEquals(prior, statement))
                        break;

                    builder.Add(prior);
                }
                break;
        }

        return builder.ToImmutable();
    }

    private static void ApplyStatement(
        BoundStatement statement,
        Dictionary<ILocalSymbol, DefaultState> states)
    {
        switch (statement)
        {
            case BoundLocalDeclarationStatement declaration:
                foreach (var declarator in declaration.Declarators)
                {
                    if (CanRepresentInactiveStructUnionState(declarator.Local.Type))
                    {
                        states[declarator.Local] = declarator.Initializer is not null
                            ? GetExpressionState(declarator.Initializer, states)
                            : DefaultState.Default;
                    }
                }
                break;
            case BoundAssignmentStatement { Expression: BoundLocalAssignmentExpression localAssignment }:
                ApplyLocalAssignment(localAssignment, states);
                break;
            case BoundExpressionStatement { Expression: BoundLocalAssignmentExpression localAssignment }:
                ApplyLocalAssignment(localAssignment, states);
                break;
            case BoundAssignmentStatement { Expression: BoundPatternAssignmentExpression patternAssignment }:
                ApplyPatternAssignment(patternAssignment, states);
                break;
            case BoundExpressionStatement { Expression: BoundPatternAssignmentExpression patternAssignment }:
                ApplyPatternAssignment(patternAssignment, states);
                break;
            case BoundBlockStatement block:
                foreach (var child in block.Statements)
                    ApplyStatement(child, states);
                break;
            case BoundIfStatement ifStatement:
                ApplyIfStatement(ifStatement, states);
                break;
            case BoundWhileStatement whileStatement:
                ApplyMayExecuteStatement(whileStatement.Body, states);
                break;
            case BoundForStatement forStatement:
                ApplyMayExecuteStatement(forStatement.Body, states);
                break;
            case BoundLockStatement lockStatement:
                ApplyStatement(lockStatement.Body, states);
                break;
            case BoundTryStatement tryStatement:
                ApplyTryStatement(tryStatement, states);
                break;
        }
    }

    private static void ApplyLocalAssignment(
        BoundLocalAssignmentExpression assignment,
        Dictionary<ILocalSymbol, DefaultState> states)
    {
        if (!CanRepresentInactiveStructUnionState(assignment.Local.Type))
            return;

        states[assignment.Local] = GetExpressionState(assignment.Right, states);
    }

    private static void ApplyPatternAssignment(
        BoundPatternAssignmentExpression assignment,
        Dictionary<ILocalSymbol, DefaultState> states)
    {
        var state = GetExpressionState(assignment.Right, states);

        foreach (var designator in assignment.Pattern.GetDesignators())
        {
            if (designator is BoundSingleVariableDesignator { Local: { } local } &&
                CanRepresentInactiveStructUnionState(local.Type))
            {
                states[local] = state;
            }
        }
    }

    private static void ApplyIfStatement(
        BoundIfStatement ifStatement,
        Dictionary<ILocalSymbol, DefaultState> states)
    {
        var before = Clone(states);

        var thenStates = Clone(before);
        ApplyStatement(ifStatement.ThenNode, thenStates);

        var elseStates = Clone(before);
        if (ifStatement.ElseNode is not null)
            ApplyStatement(ifStatement.ElseNode, elseStates);

        states.Clear();

        var locals = new HashSet<ILocalSymbol>(thenStates.Keys, SymbolEqualityComparer.Default);
        locals.UnionWith(elseStates.Keys);

        foreach (var local in locals)
        {
            var thenState = thenStates.TryGetValue(local, out var thenValue) ? thenValue : DefaultState.Active;
            var elseState = elseStates.TryGetValue(local, out var elseValue) ? elseValue : DefaultState.Active;
            states[local] = Join(thenState, elseState);
        }
    }

    private static void ApplyMayExecuteStatement(
        BoundStatement statement,
        Dictionary<ILocalSymbol, DefaultState> states)
    {
        var before = Clone(states);
        var after = Clone(before);
        ApplyStatement(statement, after);
        JoinInto(states, before, after);
    }

    private static void ApplyTryStatement(
        BoundTryStatement tryStatement,
        Dictionary<ILocalSymbol, DefaultState> states)
    {
        var before = Clone(states);

        var tryStates = Clone(before);
        ApplyStatement(tryStatement.TryBlock, tryStates);

        var joined = Clone(tryStates);

        foreach (var catchClause in tryStatement.CatchClauses)
        {
            var catchStates = Clone(before);
            ApplyStatement(catchClause.Block, catchStates);
            var currentJoined = Clone(joined);
            JoinInto(joined, currentJoined, catchStates);
        }

        states.Clear();
        foreach (var (local, state) in joined)
            states[local] = state;

        if (tryStatement.FinallyBlock is not null)
            ApplyStatement(tryStatement.FinallyBlock, states);
    }

    private static void JoinInto(
        Dictionary<ILocalSymbol, DefaultState> target,
        Dictionary<ILocalSymbol, DefaultState> left,
        Dictionary<ILocalSymbol, DefaultState> right)
    {
        target.Clear();

        var locals = new HashSet<ILocalSymbol>(left.Keys, SymbolEqualityComparer.Default);
        locals.UnionWith(right.Keys);

        foreach (var local in locals)
        {
            var leftState = left.TryGetValue(local, out var leftValue) ? leftValue : DefaultState.Active;
            var rightState = right.TryGetValue(local, out var rightValue) ? rightValue : DefaultState.Active;
            target[local] = Join(leftState, rightState);
        }
    }

    private static Dictionary<ILocalSymbol, DefaultState> Clone(Dictionary<ILocalSymbol, DefaultState> states)
        => new(states, SymbolEqualityComparer.Default);

    private static DefaultState Join(DefaultState left, DefaultState right)
    {
        if (left == right)
            return left;

        return DefaultState.MaybeDefault;
    }

    private static DefaultState GetUnknownExpressionState(BoundExpression expression)
        => DefaultState.Active;

    private static DefaultState GetMaybeDefaultExpressionState(BoundExpression expression)
        => CanRepresentInactiveStructUnionState(expression.Type) ? DefaultState.MaybeDefault : DefaultState.Active;

    private static bool CanRepresentInactiveStructUnionState(ITypeSymbol type)
        => type.TryGetUnion() is { TypeKind: TypeKind.Struct };
}
