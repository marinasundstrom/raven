using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class AwaitForLowerer
{
    public static BoundBlockStatement Rewrite(ISymbol symbol, BoundBlockStatement body)
    {
        if (symbol is null)
            throw new ArgumentNullException(nameof(symbol));
        if (body is null)
            throw new ArgumentNullException(nameof(body));

        var compilation = GetCompilation(symbol);
        var rewriter = new Rewriter(symbol, compilation);
        return (BoundBlockStatement)rewriter.VisitStatement(body)!;
    }

    private static Compilation GetCompilation(ISymbol symbol)
    {
        if (symbol.ContainingAssembly is SourceAssemblySymbol sourceAssembly)
            return sourceAssembly.Compilation;

        throw new InvalidOperationException("Await-for lowering requires a source assembly containing symbol.");
    }

    private sealed class Rewriter : BoundTreeRewriter
    {
        private readonly ISymbol _containingSymbol;
        private readonly Compilation _compilation;
        private readonly ITypeSymbol _unitType;
        private int _tempCounter;
        private int _labelCounter;

        public Rewriter(ISymbol containingSymbol, Compilation compilation)
        {
            _containingSymbol = containingSymbol;
            _compilation = compilation;
            _unitType = compilation.GetSpecialType(SpecialType.System_Unit);
        }

        public override BoundNode? VisitForStatement(BoundForStatement node)
        {
            if (node.Iteration.Kind != ForIterationKind.Async ||
                node.Iteration.GetEnumeratorMethod is not IMethodSymbol getAsyncEnumeratorMethod ||
                node.Iteration.MoveNextAsyncMethod is not IMethodSymbol moveNextAsyncMethod ||
                node.Iteration.CurrentGetter is not IMethodSymbol currentGetter)
            {
                return base.VisitForStatement(node);
            }

            var collection = VisitExpression(node.Collection) ?? node.Collection;
            var body = VisitStatement(node.Body) ?? node.Body;

            var statements = new List<BoundStatement>();

            if (node.Local is not null)
            {
                statements.Add(new BoundLocalDeclarationStatement(
                    new[] { new BoundVariableDeclarator(node.Local, initializer: null) }));
            }

            var enumeratorType = getAsyncEnumeratorMethod.ReturnType;
            var enumeratorLocal = CreateTempLocal("asyncEnumerator", enumeratorType);
            var hasNextLocal = CreateTempLocal("asyncHasNext", _compilation.GetSpecialType(SpecialType.System_Boolean));
            var continueLabel = CreateLabel("awaitfor_continue");
            var breakLabel = CreateLabel("awaitfor_break");

            BoundExpression getEnumeratorInvocation;
            var optionalArguments = getAsyncEnumeratorMethod.Parameters
                .Select(static parameter => (BoundExpression)new BoundDefaultValueExpression(parameter.Type))
                .ToArray();

            if (getAsyncEnumeratorMethod.IsExtensionMethod)
            {
                var extensionArguments = new List<BoundExpression>(optionalArguments.Length + 1) { collection };
                extensionArguments.AddRange(optionalArguments);

                getEnumeratorInvocation = new BoundInvocationExpression(
                    getAsyncEnumeratorMethod,
                    extensionArguments,
                    receiver: null,
                    extensionReceiver: collection);
            }
            else
            {
                getEnumeratorInvocation = new BoundInvocationExpression(
                    getAsyncEnumeratorMethod,
                    optionalArguments,
                    receiver: collection);
            }

            statements.Add(new BoundLocalDeclarationStatement(
                new[] { new BoundVariableDeclarator(enumeratorLocal, getEnumeratorInvocation) }));
            statements.Add(new BoundLocalDeclarationStatement(
                new[] { new BoundVariableDeclarator(hasNextLocal, initializer: null) }));

            var moveNextInvocation = new BoundInvocationExpression(
                moveNextAsyncMethod,
                Array.Empty<BoundExpression>(),
                receiver: new BoundLocalAccess(enumeratorLocal));
            var moveNextAwait = ConvertToBoolIfNeeded(CreateAwaitExpression(moveNextInvocation));
            var assignHasNext = new BoundAssignmentStatement(
                new BoundLocalAssignmentExpression(
                    hasNextLocal,
                    new BoundLocalAccess(hasNextLocal),
                    moveNextAwait,
                    _unitType));

            var loopBodyStatements = new List<BoundStatement>
            {
                assignHasNext,
                new BoundConditionalGotoStatement(breakLabel, new BoundLocalAccess(hasNextLocal), jumpIfTrue: false),
            };

            if (node.Local is not null)
            {
                var currentInvocation = new BoundInvocationExpression(
                    currentGetter,
                    Array.Empty<BoundExpression>(),
                    receiver: new BoundLocalAccess(enumeratorLocal));

                BoundExpression currentValue = currentInvocation;
                if (!SymbolEqualityComparer.Default.Equals(currentValue.Type, node.Local.Type))
                    currentValue = ConvertIfNeeded(currentValue, node.Local.Type);

                var assignment = new BoundLocalAssignmentExpression(
                    node.Local,
                    new BoundLocalAccess(node.Local),
                    currentValue,
                    _unitType);
                loopBodyStatements.Add(new BoundAssignmentStatement(assignment));
            }

            loopBodyStatements.Add(body);
            loopBodyStatements.Add(new BoundGotoStatement(continueLabel, isBackward: true));

            statements.Add(new BoundLabeledStatement(continueLabel, new BoundBlockStatement(loopBodyStatements)));
            statements.Add(new BoundLabeledStatement(breakLabel, new BoundBlockStatement(Array.Empty<BoundStatement>())));
            return new BoundBlockStatement(statements);
        }

        private SourceLocalSymbol CreateTempLocal(string nameHint, ITypeSymbol type)
        {
            var containingType = _containingSymbol.ContainingType as INamedTypeSymbol;
            var containingNamespace = _containingSymbol.ContainingNamespace;
            var name = $"<{nameHint}>__awaitfor_{_tempCounter++}";
            return new SourceLocalSymbol(
                name,
                type,
                isMutable: true,
                _containingSymbol,
                containingType,
                containingNamespace,
                [Location.None],
                Array.Empty<SyntaxReference>());
        }

        private ILabelSymbol CreateLabel(string nameHint)
        {
            var containingType = _containingSymbol.ContainingType as INamedTypeSymbol;
            var containingNamespace = _containingSymbol.ContainingNamespace;
            var name = $"<{nameHint}>__awaitfor_{_labelCounter++}";
            return new LabelSymbol(
                name,
                _containingSymbol,
                containingType,
                containingNamespace,
                [Location.None],
                Array.Empty<SyntaxReference>());
        }

        private BoundExpression CreateAwaitExpression(BoundExpression operand)
        {
            var operandType = operand.Type ?? _compilation.ErrorTypeSymbol;
            if (!AwaitablePattern.TryFind(operandType, isAccessible: null, out var awaitable, out _, out _))
                return operand;

            var resultType = awaitable.GetResultMethod.ReturnType;
            if (resultType.SpecialType == SpecialType.System_Void)
                resultType = _unitType;

            return new BoundAwaitExpression(
                operand,
                resultType,
                awaitable.AwaiterType,
                awaitable.GetAwaiterMethod,
                awaitable.GetResultMethod,
                awaitable.IsCompletedProperty);
        }

        private BoundExpression ConvertToBoolIfNeeded(BoundExpression expression)
        {
            var boolType = _compilation.GetSpecialType(SpecialType.System_Boolean);
            if (SymbolEqualityComparer.Default.Equals(expression.Type, boolType))
                return expression;

            return ConvertIfNeeded(expression, boolType);
        }

        private BoundExpression ConvertIfNeeded(BoundExpression expression, ITypeSymbol targetType)
        {
            if (SymbolEqualityComparer.Default.Equals(expression.Type, targetType))
                return expression;

            var conversion = _compilation.ClassifyConversion(expression.Type, targetType);
            if (!conversion.Exists)
                return expression;

            return new BoundConversionExpression(expression, targetType, conversion);
        }
    }
}
