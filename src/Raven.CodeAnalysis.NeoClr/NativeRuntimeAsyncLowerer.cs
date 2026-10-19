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
        if (plan.Symbol is not SourceMethodSymbol method)
            throw Unsupported("requires a source method");
        var generator = AsyncReturnTypeUtilities.TryGetRuntimeIterator(compilation, method.ReturnType, out var iteratorKind, out var elementType);
        var task = method.ReturnType as INamedTypeSymbol;
        if (!generator && (method.IsIterator || task is not { SpecialType: SpecialType.System_Threading_Tasks_Task_T, TypeArguments.Length: 1 }))
            throw Unsupported("requires Task<T> or a selected async iterator return type");
        var unit = compilation.GetSpecialType(SpecialType.System_Unit);
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
        var resultType = generator ? unit : task!.TypeArguments[0];
        var start = Resolve("Start", 1).Construct(resultType);
        var complete = Resolve("Complete", 1).Construct(resultType);
        var cancel = Resolve("Cancel", 0).Construct(resultType);
        var waits = helper.GetMembers("Wait").OfType<IMethodSymbol>()
            .Where(m => m.IsStatic && m.Arity <= 1 && m.Parameters.Length == 1).ToImmutableArray();
        var outcome = complete.ReturnType;
        IMethodSymbol? yieldMethod = null;
        if (generator)
        {
            var producerHelper = compilation.GetTypeByMetadataName(helperName + "Generator")
                ?? throw Unsupported("generator helper is unavailable: " + helperName + "Generator");
            if (!SymbolEqualityComparer.Default.Equals(producerHelper.ContainingAssembly, method.ContainingAssembly))
                throw Unsupported("the generator helper must be source-included in this assembly");
            IMethodSymbol ProducerMethod(string name, int count)
            {
                var matches = producerHelper.GetMembers(name).OfType<IMethodSymbol>()
                    .Where(m => m.IsStatic && m.Arity == 1 && m.Parameters.Length == count).Take(2).ToArray();
                return matches.Length == 1 ? matches[0].Construct(elementType)
                    : throw Unsupported("generator helper requires one " + name + "<T> mapping");
            }
            start = ProducerMethod(iteratorKind == IteratorMethodKind.AsyncEnumerable ? "CreateIterable" : "CreateIterator", 1);
            yieldMethod = ProducerMethod("Yield", 2);
        }
        var delegateType = start.Parameters[0].Type;
        if (!SymbolEqualityComparer.Default.Equals(start.ReturnType, method.ReturnType) ||
            !SymbolEqualityComparer.Default.Equals(complete.Parameters[0].Type, resultType) ||
            !SymbolEqualityComparer.Default.Equals(cancel.ReturnType, outcome) ||
            delegateType is not INamedTypeSymbol { TypeKind: TypeKind.Delegate } function ||
            function.GetDelegateInvokeMethod() is not { } invoke ||
            invoke.Parameters.Length != (generator ? 1 : 0) ||
            !SymbolEqualityComparer.Default.Equals(invoke.ReturnType, outcome) || waits.IsEmpty)
            throw Unsupported("helper Start/Complete/Cancel/Wait signatures do not match");
        var model = compilation.GetSemanticModel(plan.Syntax.SyntaxTree);
        if (plan.Body is null || model.GetBoundNode(plan.Body, BoundTreeView.Original) is not BoundBlockStatement body)
            throw Unsupported("requires a bound block body");
        var lambda = new SourceLambdaSymbol([], outcome, method, method.ContainingType, method.ContainingNamespace,
            [plan.Syntax.GetLocation()]);
        IParameterSymbol? producer = null;
        if (generator)
        {
            producer = new SourceParameterSymbol("__ravenGenerator", invoke.Parameters[0].Type, lambda,
                method.ContainingType, method.ContainingNamespace, [Location.None], []);
            lambda.SetParameters([producer]);
            if (!SymbolEqualityComparer.Default.Equals(yieldMethod!.Parameters[0].Type, producer.Type) ||
                !SymbolEqualityComparer.Default.Equals(yieldMethod.Parameters[1].Type, elementType) ||
                !AwaitablePattern.TryFind(yieldMethod.ReturnType, null, out var yieldAwaitable, out _, out _) ||
                yieldAwaitable.GetResultMethod.ReturnType.SpecialType != SpecialType.System_Boolean)
                throw Unsupported("generator Yield must take its cursor and element and await to Boolean");
        }
        var captures = method.Parameters.Cast<ISymbol>().ToList();
        if (!method.IsStatic) captures.Insert(0, method.ContainingType!);
        lambda.SetDelegateType(delegateType);
        lambda.SetCapturedVariables(captures);
        body = ImplicitReturnRewriter.RewriteIfNeeded(resultType, unit, body);
        // Expose implicit awaits and loop transfers before replacing waits with
        // runtime suspension. Cleanup scopes then remain explicit on every exit.
        if (generator) body = YieldFromLowerer.Rewrite(method, compilation, body);
        body = AwaitForLowerer.Rewrite(method, body);
        body = AsyncForLowerer.Rewrite(method, body, useExceptionHandling: false);
        body = (BoundBlockStatement)new CleanupScopes(compilation, lambda, Unsupported).VisitStatement(body)!;
        var rewriter = new Rewriter(lambda, unit, resultType, complete, cancel, waits, Unsupported, yieldMethod, producer);
        rewriter.IndexLabels(body);
        body = (BoundBlockStatement)rewriter.VisitStatement(body)!;
        if (resultType.SpecialType is SpecialType.System_Unit or SpecialType.System_Void)
            body = new BoundBlockStatement([.. body.Statements, rewriter.Completion(new BoundUnitExpression(unit))], body.LocalsToDispose);
        else
            body = new BoundBlockStatement([.. body.Statements, new BoundThrowStatement(new BoundUnitExpression(unit),
                compilerFailure: "Runtime async body reached an invalid fallthrough")], body.LocalsToDispose);
        body = Lowerer.LowerBlock(lambda, body);
        var expression = new BoundFunctionExpression(lambda.Parameters, outcome,
            new BoundBlockExpression(body.Statements, unit, body.LocalsToDispose), lambda, delegateType, captures, []);
        var wrapper = new BoundBlockStatement([new BoundReturnStatement(new BoundInvocationExpression(start, [expression]))]);
        return plan with { PreparedBody = wrapper };
    }

    // Match arms are auxiliary nodes whose payloads the generated rewriter
    // preserves. Both cleanup discovery and suspension rewriting must visit them.
    private abstract class MatchRewriter : BoundTreeRewriter
    {
        private ImmutableArray<BoundMatchArm> RewriteArms(ImmutableArray<BoundMatchArm> arms) =>
            arms.Select(arm => new BoundMatchArm(
                (BoundPattern)VisitPattern(arm.Pattern)!, VisitExpression(arm.Guard),
                VisitExpression(arm.Expression)!)).ToImmutableArray();

        public override BoundNode? VisitMatchStatement(BoundMatchStatement node) =>
            node.Update(VisitExpression(node.Expression)!, RewriteArms(node.Arms));

        public override BoundNode? VisitMatchExpression(BoundMatchExpression node) =>
            node.Update(VisitExpression(node.Expression)!, RewriteArms(node.Arms), node.Type);
    }

    // Keep synchronous use and asynchronous iterator cleanup in one lexical stack.
    // Otherwise a later use-lowering pass could dispose an inner resource after
    // its outer iterator, or miss an exit introduced by cancellation rewriting.
    private sealed class CleanupScopes(Compilation compilation, ISymbol owner,
        Func<string, UnsupportedInputException> unsupported) : MatchRewriter
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
        Func<string, UnsupportedInputException> unsupported, IMethodSymbol? yieldMethod, IParameterSymbol? producer) : MatchRewriter
    {
        // Generated visitors otherwise defer argument-list traversal until after
        // VisitTryStatement has popped the lexical cleanup scope.
        public override IEnumerable<T> VisitList<T>(IEnumerable<T> nodes)
            => base.VisitList(nodes).ToArray();

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
        public override BoundNode? VisitPropagateExpression(BoundPropagateExpression node)
        {
            // Expand residual returns before wrapping exits in the runtime outcome.
            // Otherwise the later general lowering pass returns Result directly
            // from the TaskOutcome-producing closure and skips its cleanup stack.
            var lowered = Lowerer.LowerExpression(owner, node);
            if (lowered is BoundPropagateExpression)
                throw unsupported("propagation could not be lowered");
            return VisitExpression(lowered);
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
        public override BoundNode? VisitYieldStatement(BoundYieldStatement node)
        {
            if (yieldMethod is null || producer is null || node.Iteration is not null)
                throw unsupported("yield requires the selected generator protocol");
            var invocation = new BoundInvocationExpression(yieldMethod, [new BoundParameterAccess(producer), node.Expression]);
            if (!AwaitablePattern.TryFind(yieldMethod.ReturnType, null, out var pattern, out _, out _))
                throw unsupported("Yield result is not awaitable");
            var awaited = new BoundAwaitExpression(invocation, pattern.GetResultMethod.ReturnType, pattern.AwaiterType,
                pattern.GetAwaiterMethod, pattern.GetResultMethod, pattern.IsCompletedProperty);
            var resume = VisitExpression(awaited)!;
            var stop = new BoundBlockStatement([.. CleanupTo(0), Completion(new BoundUnitExpression(unit))]);
            return new BoundIfStatement(resume, new BoundBlockStatement([]), stop);
        }
        public override BoundNode? VisitYieldExpression(BoundYieldExpression node)
            => new BoundBlockExpression([(BoundStatement)VisitYieldStatement(new BoundYieldStatement(
                node.Expression, node.ElementType, node.IteratorKind, node.Iteration))!,
                new BoundExpressionStatement(new BoundUnitExpression(unit))], unit);
        public override BoundNode? VisitReturnExpression(BoundReturnExpression node) =>
            throw unsupported("return expressions inside value expressions are not supported");
        public override BoundNode? VisitFunctionExpression(BoundFunctionExpression node)
        {
            if (node.Symbol is IMethodSymbol { IsAsync: true }) throw unsupported("nested async functions are not supported");
            return node;
        }
    }
}
