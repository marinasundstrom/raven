using System;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class RuntimeAsyncLowerer
{
    public static BoundBlockStatement Rewrite(ISymbol symbol, BoundBlockStatement body)
    {
        if (symbol.ContainingAssembly is not SourceAssemblySymbol sourceAssembly)
            throw new InvalidOperationException("Runtime-async lowering requires a source assembly containing symbol.");

        var rewriter = new Rewriter(symbol, sourceAssembly.Compilation);
        return (BoundBlockStatement)rewriter.VisitStatement(body)!;
    }

    private sealed class Rewriter : BoundTreeRewriter
    {
        private readonly ISymbol _containingSymbol;
        private readonly Compilation _compilation;
        private readonly ITypeSymbol _unitType;
        private int _awaiterOrdinal;

        public Rewriter(ISymbol containingSymbol, Compilation compilation)
        {
            _containingSymbol = containingSymbol;
            _compilation = compilation;
            _unitType = compilation.GetSpecialType(SpecialType.System_Unit);
        }

        public override BoundNode? VisitAwaitExpression(BoundAwaitExpression node)
        {
            var expression = VisitExpression(node.Expression) ?? node.Expression;
            if (CanUseDirectAwaitHelper(expression.Type))
            {
                return ReferenceEquals(expression, node.Expression)
                    ? node
                    : new BoundAwaitExpression(
                        expression,
                        node.ResultType,
                        node.AwaiterType,
                        node.GetAwaiterMethod,
                        node.GetResultMethod,
                        node.IsCompletedProperty);
            }

            var awaiterLocal = CreateAwaiterLocal(node.AwaiterType);
            var getAwaiter = new BoundInvocationExpression(
                node.GetAwaiterMethod,
                Array.Empty<BoundExpression>(),
                receiver: expression);
            var awaiterDeclaration = new BoundLocalDeclarationStatement(
                new[] { new BoundVariableDeclarator(awaiterLocal, getAwaiter) });

            var isCompleted = new BoundMemberAccessExpression(
                new BoundLocalAccess(awaiterLocal),
                node.IsCompletedProperty);
            if (!BoundUnaryOperator.TryLookup(
                    _compilation,
                    SyntaxKind.ExclamationToken,
                    isCompleted.Type,
                    out var logicalNot))
            {
                throw new InvalidOperationException("Failed to bind the runtime-async IsCompleted guard.");
            }

            var suspensionHelper = ResolveSuspensionHelper(node.AwaiterType);
            var suspend = new BoundInvocationExpression(
                suspensionHelper,
                new BoundExpression[] { new BoundLocalAccess(awaiterLocal) });
            var suspendIfIncomplete = new BoundIfStatement(
                new BoundUnaryExpression(logicalNot, isCompleted),
                new BoundBlockStatement(new BoundStatement[] { new BoundExpressionStatement(suspend) }));

            var getResult = new BoundInvocationExpression(
                node.GetResultMethod,
                Array.Empty<BoundExpression>(),
                receiver: new BoundLocalAccess(awaiterLocal),
                requiresReceiverAddress: node.AwaiterType.IsValueType);

            return new BoundBlockExpression(
                new BoundStatement[]
                {
                    awaiterDeclaration,
                    suspendIfIncomplete,
                    new BoundExpressionStatement(getResult)
                },
                _unitType);
        }

        public override BoundNode? VisitFunctionExpression(BoundFunctionExpression node)
        {
            // Nested functions are lowered against their own symbol and async mode.
            return node;
        }

        private IMethodSymbol ResolveSuspensionHelper(ITypeSymbol awaiterType)
        {
            var implementsCriticalNotifyCompletion = awaiterType.AllInterfaces.Any(static @interface =>
                string.Equals(
                    @interface.ToFullyQualifiedMetadataName(),
                    "System.Runtime.CompilerServices.ICriticalNotifyCompletion",
                    StringComparison.Ordinal));
            var helperName = implementsCriticalNotifyCompletion ? "UnsafeAwaitAwaiter" : "AwaitAwaiter";
            var asyncHelpers = _compilation.GetTypeByMetadataName("System.Runtime.CompilerServices.AsyncHelpers")
                ?? throw new InvalidOperationException("The target runtime does not expose AsyncHelpers.");
            var helper = asyncHelpers.GetMembers(helperName)
                .OfType<IMethodSymbol>()
                .Single(method =>
                    method.IsStatic &&
                    method.Arity == 1 &&
                    method.Parameters.Length == 1);

            return helper.Construct(awaiterType);
        }

        private SourceLocalSymbol CreateAwaiterLocal(ITypeSymbol awaiterType)
        {
            return new SourceLocalSymbol(
                $"<runtimeAwaiter>__{_awaiterOrdinal++}",
                awaiterType,
                isMutable: true,
                _containingSymbol,
                _containingSymbol.ContainingType,
                _containingSymbol.ContainingNamespace,
                [Location.None],
                Array.Empty<SyntaxReference>(),
                isImplicitlyDeclared: true);
        }

        private static bool CanUseDirectAwaitHelper(ITypeSymbol? type)
        {
            if (type?.GetNonNullableType() is not INamedTypeSymbol namedType)
                return false;

            var definition = namedType.OriginalDefinition as INamedTypeSymbol ?? namedType;
            if (definition.SpecialType is SpecialType.System_Threading_Tasks_Task or SpecialType.System_Threading_Tasks_Task_T)
                return true;

            return definition.ToFullyQualifiedMetadataName() is
                "System.Threading.Tasks.ValueTask" or
                "System.Threading.Tasks.ValueTask`1" or
                "System.Runtime.CompilerServices.ConfiguredTaskAwaitable" or
                "System.Runtime.CompilerServices.ConfiguredTaskAwaitable`1" or
                "System.Runtime.CompilerServices.ConfiguredValueTaskAwaitable" or
                "System.Runtime.CompilerServices.ConfiguredValueTaskAwaitable`1";
        }
    }
}
