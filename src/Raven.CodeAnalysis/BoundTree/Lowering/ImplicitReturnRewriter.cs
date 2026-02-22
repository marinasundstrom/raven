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
    /// Rewrites the final expression statement in <paramref name="body"/> to a
    /// <see cref="BoundReturnStatement"/> when it qualifies as an implicit return.
    /// </summary>
    public static BoundBlockStatement RewriteIfNeeded(
        ITypeSymbol returnType,
        ITypeSymbol unitType,
        BoundBlockStatement body)
    {
        if (!IsImplicitReturnCandidate(returnType, unitType, body, out var candidate))
            return body;

        var statements = body.Statements.ToList();
        statements[statements.Count - 1] = new BoundReturnStatement(candidate.Expression);
        return new BoundBlockStatement(statements, body.LocalsToDispose);
    }
}
