using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class StructUnionDefaultStateFlow
{
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
        => GetExpressionState(expression, GetLocalStatesBefore(matchSyntax, getBoundNode)) is DefaultState.Default or DefaultState.MaybeDefault;

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
            case BoundLocalAccess localAccess:
                return localStates.TryGetValue(localAccess.Local, out var state) ? state : DefaultState.Active;
            case BoundIfExpression ifExpression:
                {
                    var thenState = GetExpressionState(ifExpression.ThenBranch, localStates);
                    var elseState = ifExpression.ElseBranch is not null
                        ? GetExpressionState(ifExpression.ElseBranch, localStates)
                        : DefaultState.Active;

                    return Join(thenState, elseState);
                }
            default:
                return DefaultState.Active;
        }
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
                            : DefaultState.Active;
                    }
                }
                break;
            case BoundAssignmentStatement { Expression: BoundLocalAssignmentExpression localAssignment }:
                ApplyLocalAssignment(localAssignment, states);
                break;
            case BoundExpressionStatement { Expression: BoundLocalAssignmentExpression localAssignment }:
                ApplyLocalAssignment(localAssignment, states);
                break;
            case BoundBlockStatement block:
                foreach (var child in block.Statements)
                    ApplyStatement(child, states);
                break;
            case BoundIfStatement ifStatement:
                ApplyIfStatement(ifStatement, states);
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

    private static Dictionary<ILocalSymbol, DefaultState> Clone(Dictionary<ILocalSymbol, DefaultState> states)
        => new(states, SymbolEqualityComparer.Default);

    private static DefaultState Join(DefaultState left, DefaultState right)
    {
        if (left == right)
            return left;

        return DefaultState.MaybeDefault;
    }

    private static bool CanRepresentInactiveStructUnionState(ITypeSymbol type)
        => type.TryGetUnion() is { TypeKind: TypeKind.Struct };
}
