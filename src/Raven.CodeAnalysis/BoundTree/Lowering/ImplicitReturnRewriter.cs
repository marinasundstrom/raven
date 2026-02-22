using System.Collections.Generic;
using System.Linq;

using Microsoft.CodeAnalysis;

namespace Raven.CodeAnalysis;

internal static class ImplicitReturnRewriter
{
    /// <summary>
    /// Returns true when the last statement of <paramref name="body"/> is a bare
    /// expression statement that should be treated as an implicit return for a
    /// method whose return type is <paramref name="returnType"/>.
    /// When true, <paramref name="candidate"/> is set to that statement.
    /// </summary>
    public static bool IsImplicitReturnCandidate(
        ITypeSymbol returnType,
        ITypeSymbol unitType,
        BoundBlockStatement body,
        out BoundExpressionStatement candidate)
    {
        candidate = null!;

        if (returnType.SpecialType == SpecialType.System_Void)
            return false;
        if (SymbolEqualityComparer.Default.Equals(returnType, unitType))
            return false;
        if (!body.Statements.Any())
            return false;

        var last = body.Statements.Last();
        if (last is not BoundExpressionStatement exprStmt)
            return false;

        var expr = exprStmt.Expression;
        if (expr?.Type is not { } exprType)
            return false;
        if (exprType.TypeKind == TypeKind.Error)
            return false;
        if (SymbolEqualityComparer.Default.Equals(exprType, unitType))
            return false;

        candidate = exprStmt;
        return true;
    }

    /// <summary>
    /// Rewrites the final statement in <paramref name="body"/> to use explicit return
    /// statements when it qualifies as an implicit return. Handles:
    /// <list type="bullet">
    ///   <item>A trailing <see cref="BoundExpressionStatement"/> → <see cref="BoundReturnStatement"/>.</item>
    ///   <item>A trailing <see cref="BoundIfStatement"/> with an else clause where every reachable
    ///     branch ends in an implicit-return-eligible expression.</item>
    /// </list>
    /// </summary>
    public static BoundBlockStatement RewriteIfNeeded(
        ITypeSymbol returnType,
        ITypeSymbol unitType,
        BoundBlockStatement body)
    {
        if (!body.Statements.Any())
            return body;

        if (returnType.SpecialType == SpecialType.System_Void)
            return body;
        if (SymbolEqualityComparer.Default.Equals(returnType, unitType))
            return body;

        var statements = body.Statements.ToList();
        var last = statements[statements.Count - 1];

        // Case 1: trailing expression statement → return statement.
        if (IsImplicitReturnCandidate(returnType, unitType, body, out var candidate))
        {
            statements[statements.Count - 1] = new BoundReturnStatement(candidate.Expression);
            return new BoundBlockStatement(statements, body.LocalsToDispose);
        }

        // Case 2: trailing if/else where every branch ends in an eligible expression
        // → rewrite each branch so it ends with an explicit return.
        if (last is BoundIfStatement ifStmt && ifStmt.ElseNode is not null)
        {
            var rewrittenIf = RewriteIfStatementBranches(returnType, unitType, ifStmt);
            if (rewrittenIf is not null)
            {
                statements[statements.Count - 1] = rewrittenIf;
                return new BoundBlockStatement(statements, body.LocalsToDispose);
            }
        }

        return body;
    }

    /// <summary>
    /// Recursively rewrites each branch of <paramref name="ifStmt"/> so that trailing
    /// implicit-return-eligible expressions become explicit <see cref="BoundReturnStatement"/>s.
    /// Returns <c>null</c> if any branch cannot be rewritten (e.g. no else clause, or
    /// an expression in a branch has an error type).
    /// </summary>
    private static BoundIfStatement? RewriteIfStatementBranches(
        ITypeSymbol returnType,
        ITypeSymbol unitType,
        BoundIfStatement ifStmt)
    {
        if (ifStmt.ElseNode is null)
            return null;

        var thenRewritten = RewriteBranch(returnType, unitType, ifStmt.ThenNode);
        if (thenRewritten is null)
            return null;

        var elseRewritten = RewriteBranch(returnType, unitType, ifStmt.ElseNode);
        if (elseRewritten is null)
            return null;

        return new BoundIfStatement(ifStmt.Condition, thenRewritten, elseRewritten);
    }

    /// <summary>
    /// Rewrites a single if-statement branch so that a trailing implicit-return-eligible
    /// expression becomes an explicit <see cref="BoundReturnStatement"/>. Returns
    /// <c>null</c> when the branch cannot be rewritten.
    /// </summary>
    private static BoundStatement? RewriteBranch(
        ITypeSymbol returnType,
        ITypeSymbol unitType,
        BoundStatement branch)
    {
        switch (branch)
        {
            case BoundBlockStatement block:
            {
                var rewritten = RewriteIfNeeded(returnType, unitType, block);
                // Check that the rewrite actually changed something (the last statement
                // was promoted to a return).
                return rewritten.Statements.Any() && rewritten.Statements.Last() is BoundReturnStatement
                    ? rewritten
                    : null;
            }

            case BoundExpressionStatement exprStmt:
            {
                // Bare expression branch (no braces): treat as a single-statement block.
                var expr = exprStmt.Expression;
                if (expr?.Type is not { } exprType) return null;
                if (exprType.TypeKind == TypeKind.Error) return null;
                if (SymbolEqualityComparer.Default.Equals(exprType, unitType)) return null;
                return new BoundReturnStatement(expr);
            }

            case BoundIfStatement nestedIf when nestedIf.ElseNode is not null:
            {
                // Nested if/else: recurse.
                return RewriteIfStatementBranches(returnType, unitType, nestedIf);
            }

            default:
                return null;
        }
    }
}
