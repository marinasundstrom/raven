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
        var waits = helper.GetMembers("Wait").OfType<IMethodSymbol>()
            .Where(m => m.IsStatic && m.Arity <= 1 && m.Parameters.Length == 1).ToImmutableArray();
        var outcome = complete.ReturnType;
        var delegateType = start.Parameters[0].Type;
        if (!SymbolEqualityComparer.Default.Equals(start.ReturnType, task) ||
            !SymbolEqualityComparer.Default.Equals(complete.Parameters[0].Type, resultType) ||
            !SymbolEqualityComparer.Default.Equals(cancel.ReturnType, outcome) ||
            delegateType is not INamedTypeSymbol { TypeKind: TypeKind.Delegate } function ||
            function.GetDelegateInvokeMethod() is not { Parameters.Length: 0 } invoke ||
            !SymbolEqualityComparer.Default.Equals(invoke.ReturnType, outcome) || waits.IsEmpty)
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
        // Expose implicit awaits and loop transfers before replacing waits with
        // runtime suspension. Cleanup scopes then remain explicit on every exit.
        body = AwaitForLowerer.Rewrite(method, body);
        body = AsyncForLowerer.Rewrite(method, body, useExceptionHandling: false);
        body = (BoundBlockStatement)new CleanupScopes(compilation, lambda, Unsupported).VisitStatement(body)!;
        var rewriter = new Rewriter(lambda, unit, resultType, complete, cancel, waits, Unsupported);
        rewriter.IndexLabels(body);
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

    // Keep synchronous use and asynchronous iterator cleanup in one lexical stack.
    // Otherwise a later use-lowering pass could dispose an inner resource after
    // its outer iterator, or miss an exit introduced by cancellation rewriting.
    private sealed class CleanupScopes(Compilation compilation, ISymbol owner,
        Func<string, UnsupportedInputException> unsupported) : BoundTreeRewriter
    {
        public override BoundNode? VisitFunctionExpression(BoundFunctionExpression node) => node;
        public override BoundNode? VisitFunctionStatement(BoundFunctionStatement node) => node;
        private int ordinal;
        public override BoundNode? VisitBlockExpression(BoundBlockExpression node)
        {
            var statements = node.Statements.ToList();
            if (node.LocalsToDispose.IsEmpty && !statements.OfType<BoundLocalDeclarationStatement>().Any(d => d.IsUsing))
                return base.VisitBlockExpression(node);
            SourceLocalSymbol? result = null;
            if (statements.LastOrDefault() is BoundExpressionStatement tail && tail.Expression.Type.SpecialType != SpecialType.System_Void)
            {
                result = new SourceLocalSymbol("<runtimeUseResult>" + ordinal++, tail.Expression.Type, true,
                    owner, owner.ContainingType, owner.ContainingNamespace, [Location.None], [], isImplicitlyDeclared: true);
                statements[^1] = new BoundAssignmentStatement(new BoundLocalAssignmentExpression(result,
                    new BoundLocalAccess(result), tail.Expression, node.UnitType));
            }
            var rewritten = (BoundBlockStatement)VisitBlockStatement(new BoundBlockStatement(statements, node.LocalsToDispose))!;
            var output = rewritten.Statements.ToList();
            if (result is not null)
            {
                output.Insert(0, new BoundLocalDeclarationStatement([new BoundVariableDeclarator(result, new BoundDefaultValueExpression(result.Type))]));
                output.Add(new BoundExpressionStatement(new BoundLocalAccess(result)));
            }
            return new BoundBlockExpression(output, node.UnitType, introduceILScope: node.IntroduceILScope);
        }

        public override BoundNode? VisitBlockStatement(BoundBlockStatement node)
        {
            var statements = new List<BoundStatement>();
            var frames = new List<(List<BoundStatement> Prefix, BoundBlockStatement Cleanup)>();
            foreach (var statement in node.Statements)
            {
                if (statement is not BoundLocalDeclarationStatement declaration ||
                    (!declaration.IsUsing && !declaration.Declarators.Any(d => node.LocalsToDispose.Contains(d.Local))))
                {
                    statements.Add((BoundStatement)VisitStatement(statement)!);
                    continue;
                }
                foreach (var item in declaration.Declarators)
                {
                    statements.Add(new BoundLocalDeclarationStatement([new BoundVariableDeclarator(item.Local,
                        item.Initializer is null ? null : VisitExpression(item.Initializer),
                        item.FixedAddressInitializer, item.FixedPinnedLocal)]));
                    if (!UseDisposalUtilities.TryResolveUseDisposeMethod(compilation, item.Local.Type,
                        preferAsync: false, out var dispose, out var useAwait) || dispose is null || useAwait)
                        throw unsupported("requires a synchronous use-disposal contract");
                    BoundStatement cleanup = new BoundExpressionStatement(new BoundInvocationExpression(dispose, [],
                        receiver: new BoundLocalAccess(item.Local), requiresReceiverAddress: item.Local.Type.IsValueType));
                    if (item.Local.Type.IsReferenceType)
                    {
                        var receiver = new BoundLocalAccess(item.Local);
                        var nil = new BoundLiteralExpression(BoundLiteralExpressionKind.NullLiteral, null!, item.Local.Type);
                        if (!BoundBinaryOperator.TryLookup(compilation, SyntaxKind.NotEqualsToken, receiver.Type, nil.Type, out var op))
                            throw unsupported("missing resource null-check operator");
                        cleanup = new BoundIfStatement(new BoundBinaryExpression(receiver, op, nil), cleanup);
                    }
                    frames.Add((statements, new BoundBlockStatement([cleanup])));
                    statements = [];
                }
            }
            for (int index = frames.Count - 1; index >= 0; --index)
            {
                var frame = frames[index];
                frame.Prefix.Add(new BoundTryStatement(new BoundBlockStatement(statements), [], frame.Cleanup));
                statements = frame.Prefix;
            }
            return new BoundBlockStatement(statements);
        }
    }

    private sealed class Rewriter(
        SourceLambdaSymbol owner, ITypeSymbol unit, ITypeSymbol resultType,
        IMethodSymbol complete, IMethodSymbol cancel, ImmutableArray<IMethodSymbol> waits,
        Func<string, UnsupportedInputException> unsupported) : BoundTreeRewriter
    {
        private int ordinal;
        private readonly List<BoundBlockStatement> cleanups = [];
        private readonly Dictionary<ILabelSymbol, int> labelDepths = new(SymbolEqualityComparer.Default);
        internal void IndexLabels(BoundBlockStatement body) => new CleanupLabelIndex(labelDepths).VisitStatement(body);

        private sealed class CleanupLabelIndex(Dictionary<ILabelSymbol, int> labels) : BoundTreeWalker
        {
            private int depth;
            public override void VisitFunctionExpression(BoundFunctionExpression node) { }
            public override void VisitFunctionStatement(BoundFunctionStatement node) { }
            public override void VisitTryStatement(BoundTryStatement node)
            {
                if (node.FinallyBlock is not null) depth++;
                VisitStatement(node.TryBlock);
                if (node.FinallyBlock is not null) depth--;
                if (node.FinallyBlock is not null) VisitStatement(node.FinallyBlock);
            }
            public override void VisitLabeledStatement(BoundLabeledStatement node)
            {
                labels[node.Label] = depth;
                base.VisitLabeledStatement(node);
            }
        }

        private IEnumerable<BoundStatement> CleanupTo(int depth)
        {
            var saved = cleanups.ToArray();
            try
            {
                for (int index = saved.Length - 1; index >= depth; --index)
                {
                    cleanups.Clear();
                    cleanups.AddRange(saved.Take(index));
                    // If cleanup is cancelled, only outer scopes remain active.
                    yield return (BoundStatement)VisitStatement(saved[index])!;
                }
            }
            finally
            {
                cleanups.Clear();
                cleanups.AddRange(saved);
            }
        }

        public override BoundNode? VisitTryStatement(BoundTryStatement node)
        {
            if (!node.CatchClauses.IsEmpty)
                throw unsupported("recoverable exception handlers are not supported");
            if (node.FinallyBlock is null) return VisitStatement(node.TryBlock);
            cleanups.Add(node.FinallyBlock);
            var body = (BoundStatement)VisitStatement(node.TryBlock)!;
            cleanups.RemoveAt(cleanups.Count - 1);
            var cleanup = (BoundStatement)VisitStatement(node.FinallyBlock)!;
            return new BoundBlockStatement([body, cleanup]);
        }

        public override BoundNode? VisitGotoStatement(BoundGotoStatement node)
        {
            var depth = labelDepths.GetValueOrDefault(node.Target);
            return new BoundBlockStatement([.. CleanupTo(depth), node]);
        }

        public override BoundNode? VisitConditionalGotoStatement(BoundConditionalGotoStatement node)
        {
            var condition = VisitExpression(node.Condition)!;
            var jump = (BoundStatement)VisitGotoStatement(new BoundGotoStatement(node.Target))!;
            return node.JumpIfTrue ? new BoundIfStatement(condition, jump)
                : new BoundIfStatement(condition, new BoundBlockStatement([]), jump);
        }
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
            return new BoundBlockStatement([Declare(temporary, value), .. CleanupTo(0), Completion(new BoundLocalAccess(temporary))]);
        }
        public override BoundNode? VisitAwaitExpression(BoundAwaitExpression node)
        {
            if (node.GetAwaiterMethod.IsStatic || node.GetAwaiterMethod.Parameters.Length != 0 ||
                !node.AwaiterType.IsReferenceType)
                throw unsupported("requires a reference awaiter with an instance GetAwaiter method");
            var matches = waits.Select(m => m.Arity == 1 ? m.Construct(node.ResultType) : m)
                .Where(m => SymbolEqualityComparer.Default.Equals(m.Parameters[0].Type, node.AwaiterType))
                .Take(2).ToArray();
            if (matches.Length != 1 || matches[0].ReturnType.SpecialType != SpecialType.System_Boolean)
                throw unsupported("requires one exact Boolean Wait mapping for the awaited type");
            var waitMethod = matches[0];
            var temporary = Local(node.AwaiterType);
            var completed = new BoundInvocationExpression(waitMethod, [new BoundLocalAccess(temporary)]);
            var success = new BoundBlockStatement([]);
            var cancellation = new BoundBlockStatement([.. CleanupTo(0), new BoundReturnStatement(new BoundInvocationExpression(cancel, []))]);
            return new BoundBlockExpression([
                Declare(temporary, new BoundInvocationExpression(node.GetAwaiterMethod, [],
                    receiver: VisitExpression(node.Expression)!)),
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
