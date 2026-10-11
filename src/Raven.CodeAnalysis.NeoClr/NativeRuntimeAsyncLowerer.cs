using System.Collections.Immutable;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.NeoClr;

// Explicit native-emitter POC. The CLR AsyncHelpers contract remains independent.
internal static class NativeRuntimeAsyncLowerer
{
    internal static SourceCallablePlan Rewrite(Compilation compilation, SourceCallablePlan plan, string helperName)
    {
        UnsupportedInputException Unsupported(string reason) => new("runtime async POC: " + reason, plan.Syntax.GetLocation());
        if (plan.Symbol is not SourceMethodSymbol method || method.IsGenericMethod ||
            method.ContainingType?.Arity is > 0 || method.IsIterator ||
            method.ReturnType is not INamedTypeSymbol { SpecialType: SpecialType.System_Threading_Tasks_Task_T, TypeArguments.Length: 1 } task)
            throw Unsupported("requires a nongeneric Task<T> method on a nongeneric owner");
        if (!method.IsStatic && method.ContainingType?.IsReferenceType != true)
            throw Unsupported("value-type receivers are not supported");
        var helper = compilation.GetTypeByMetadataName(helperName)
            ?? throw Unsupported("helper type is unavailable: " + helperName);
        if (!SymbolEqualityComparer.Default.Equals(helper.ContainingAssembly, method.ContainingAssembly))
            throw Unsupported("the POC helper must be source-included in this assembly");
        IMethodSymbol Resolve(string name, int count)
        {
            var matches = helper.GetMembers(name).OfType<IMethodSymbol>()
                .Where(m => m.IsStatic && m.Arity == 1 && m.Parameters.Length == count).Take(2).ToArray();
            return matches.Length == 1 ? matches[0]
                : throw Unsupported("helper requires an unambiguous static " + name + "<T> contract");
        }
        var resultType = task.TypeArguments[0];
        var start = Resolve("Start", 1).Construct(resultType);
        var complete = Resolve("Complete", 1).Construct(resultType);
        var cancel = Resolve("Cancel", 0).Construct(resultType);
        var wait = Resolve("Wait", 1);
        var outcome = complete.ReturnType;
        var delegateType = start.Parameters[0].Type;
        if (!SymbolEqualityComparer.Default.Equals(start.ReturnType, task) ||
            !SymbolEqualityComparer.Default.Equals(complete.Parameters[0].Type, resultType) ||
            !SymbolEqualityComparer.Default.Equals(cancel.ReturnType, outcome) ||
            delegateType is not INamedTypeSymbol { TypeKind: TypeKind.Delegate } function ||
            function.GetDelegateInvokeMethod() is not { Parameters.Length: 0 } invoke ||
            !SymbolEqualityComparer.Default.Equals(invoke.ReturnType, outcome) ||
            wait.ReturnType.SpecialType != SpecialType.System_Boolean)
            throw Unsupported("helper Start/Complete/Cancel/Wait signatures do not match");
        var model = compilation.GetSemanticModel(plan.Syntax.SyntaxTree);
        if (plan.Body is null || model.GetBoundNode(plan.Body, BoundTreeView.Original) is not BoundBlockStatement body)
            throw Unsupported("requires a bound block body");
        var unit = compilation.GetSpecialType(SpecialType.System_Unit);
        var lambda = new SourceLambdaSymbol([], outcome, method, method.ContainingType, method.ContainingNamespace,
            [plan.Syntax.GetLocation()]);
        var captures = method.Parameters.Cast<ISymbol>().ToList();
        if (!method.IsStatic) captures.Insert(0, method.ContainingType!);
        lambda.SetDelegateType(delegateType);
        lambda.SetCapturedVariables(captures);
        body = ImplicitReturnRewriter.RewriteIfNeeded(resultType, unit, body);
        var rewriter = new Rewriter(lambda, unit, resultType, complete, cancel, wait, Unsupported);
        body = (BoundBlockStatement)rewriter.VisitStatement(body)!;
        if (resultType.SpecialType is SpecialType.System_Unit or SpecialType.System_Void)
            body = new BoundBlockStatement([.. body.Statements, rewriter.Completion(new BoundUnitExpression(unit))], body.LocalsToDispose);
        else
            body = new BoundBlockStatement([.. body.Statements, new BoundThrowStatement(new BoundUnitExpression(unit),
                compilerFailure: "Runtime async body reached an invalid fallthrough")], body.LocalsToDispose);
        body = Lowerer.LowerBlock(lambda, body);
        var expression = new BoundFunctionExpression([], outcome,
            new BoundBlockExpression(body.Statements, unit, body.LocalsToDispose), lambda, delegateType, captures, []);
        var wrapper = new BoundBlockStatement([new BoundReturnStatement(new BoundInvocationExpression(start, [expression]))]);
        return plan with { PreparedBody = wrapper };
    }

    private sealed class Rewriter(
        SourceLambdaSymbol owner, ITypeSymbol unit, ITypeSymbol resultType,
        IMethodSymbol complete, IMethodSymbol cancel, IMethodSymbol wait,
        Func<string, UnsupportedInputException> unsupported) : BoundTreeRewriter
    {
        private int ordinal;
        private SourceLocalSymbol Local(ITypeSymbol type) => new("<runtimeAsync>" + ordinal++, type, false,
            owner, owner.ContainingType, owner.ContainingNamespace, [Location.None], [], isImplicitlyDeclared: true);
        private static BoundLocalDeclarationStatement Declare(SourceLocalSymbol local, BoundExpression value) =>
            new([new BoundVariableDeclarator(local, value)]);
        internal BoundReturnStatement Completion(BoundExpression value) =>
            new(new BoundInvocationExpression(complete, [value]));
        public override BoundNode? VisitReturnStatement(BoundReturnStatement node)
        {
            var value = node.Expression is null ? new BoundUnitExpression(unit) : VisitExpression(node.Expression)!;
            var temporary = Local(resultType);
            return new BoundBlockStatement([Declare(temporary, value), Completion(new BoundLocalAccess(temporary))]);
        }
        public override BoundNode? VisitAwaitExpression(BoundAwaitExpression node)
        {
            if (node.Expression.Type is not INamedTypeSymbol { SpecialType: SpecialType.System_Threading_Tasks_Task_T, TypeArguments.Length: 1 } awaited)
                throw unsupported("only the managed Task<T> await contract is supported");
            var waitMethod = wait.Construct(awaited.TypeArguments[0]);
            if (!SymbolEqualityComparer.Default.Equals(waitMethod.Parameters[0].Type, awaited))
                throw unsupported("Wait<T> must accept the awaited Task<T>");
            var temporary = Local(awaited);
            var completed = new BoundInvocationExpression(waitMethod, [new BoundLocalAccess(temporary)]);
            var success = new BoundBlockStatement([]);
            var cancellation = new BoundBlockStatement([new BoundReturnStatement(new BoundInvocationExpression(cancel, []))]);
            return new BoundBlockExpression([
                Declare(temporary, VisitExpression(node.Expression)!),
                new BoundIfStatement(completed, success, cancellation),
                new BoundExpressionStatement(new BoundInvocationExpression(node.GetResultMethod, [], receiver: new BoundLocalAccess(temporary)))
            ], unit);
        }
        public override BoundNode? VisitReturnExpression(BoundReturnExpression node) =>
            throw unsupported("return expressions inside value expressions are not supported");
        public override BoundNode? VisitFunctionExpression(BoundFunctionExpression node)
        {
            if (node.Symbol is IMethodSymbol { IsAsync: true }) throw unsupported("nested async functions are not supported");
            return node;
        }
    }
}
