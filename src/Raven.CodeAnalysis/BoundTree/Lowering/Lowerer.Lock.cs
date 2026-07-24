using System;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    public override BoundNode? VisitLockStatement(BoundLockStatement node)
    {
        var compilation = GetCompilation();
        var monitorType = compilation.GetTypeByMetadataName("System.Threading.Monitor")
            ?? throw new InvalidOperationException("Failed to resolve System.Threading.Monitor.");
        var enterMethod = ResolveMonitorMethod(monitorType, "Enter");
        var exitMethod = ResolveMonitorMethod(monitorType, "Exit");
        var objectType = compilation.GetSpecialType(SpecialType.System_Object);
        var expression = (BoundExpression)VisitExpression(node.Expression)!;
        var body = (BoundStatement)VisitStatement(node.Body);
        var lockLocal = CreateTempLocal("lock", expression.Type, isMutable: false);
        var lockAccess = new BoundLocalAccess(lockLocal);
        BoundExpression objectAccess = SymbolEqualityComparer.Default.Equals(expression.Type, objectType)
            ? lockAccess
            : new BoundConversionExpression(
                lockAccess,
                objectType,
                compilation.ClassifyConversion(expression.Type, objectType));

        var declaration = new BoundLocalDeclarationStatement([
            new BoundVariableDeclarator(lockLocal, expression)
        ]);
        var enter = new BoundExpressionStatement(
            new BoundInvocationExpression(enterMethod, [objectAccess]));
        var exit = new BoundExpressionStatement(
            new BoundInvocationExpression(exitMethod, [objectAccess]));
        var tryBlock = body as BoundBlockStatement ?? new BoundBlockStatement([body]);
        var finallyBlock = new BoundBlockStatement([exit]);

        return new BoundBlockStatement(
            [
                declaration,
                enter,
                new BoundTryStatement(tryBlock, ImmutableArray<BoundCatchClause>.Empty, finallyBlock)
            ]);
    }

    private static IMethodSymbol ResolveMonitorMethod(INamedTypeSymbol monitorType, string name)
    {
        return monitorType.GetMembers(name)
            .OfType<IMethodSymbol>()
            .Single(method =>
                method.IsStatic &&
                method.Parameters.Length == 1 &&
                method.Parameters[0].Type.SpecialType == SpecialType.System_Object);
    }
}
