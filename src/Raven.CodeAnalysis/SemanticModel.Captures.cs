using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    public ImmutableArray<ISymbol> GetCapturedVariables(ISymbol symbol)
    {
        if (symbol is null)
            return ImmutableArray<ISymbol>.Empty;

        return symbol switch
        {
            SourceLambdaSymbol lambda => lambda.CapturedVariables,
            SourceMethodSymbol method => GetOrComputeFunctionCapturedVariables(method),
            _ => ImmutableArray<ISymbol>.Empty
        };
    }

    public ImmutableArray<ISymbol> GetCapturedVariables(SyntaxNode node)
    {
        if (node is null)
            return ImmutableArray<ISymbol>.Empty;

        EnsureDiagnosticsCollected();

        if (node is FunctionStatementSyntax function &&
            GetDeclaredSymbol(function) is ISymbol functionSymbol)
        {
            return GetCapturedVariables(functionSymbol);
        }

        if (node is FunctionExpressionSyntax lambdaSyntax &&
            GetBoundNode(lambdaSyntax) is BoundFunctionExpression boundLambda)
        {
            return boundLambda.CapturedVariables
                .Where(static symbol => symbol is not null)
                .Distinct(SymbolEqualityComparer.Default)
                .ToImmutableArray();
        }

        var symbol = node switch
        {
            VariableDeclaratorSyntax variableDeclarator => GetDeclaredSymbol(variableDeclarator),
            ParameterSyntax parameter => GetDeclaredSymbol(parameter),
            _ => GetSymbolInfo(node).Symbol
        };

        return symbol is null
            ? ImmutableArray<ISymbol>.Empty
            : IsCapturedVariable(symbol) ? ImmutableArray.Create(symbol) : ImmutableArray<ISymbol>.Empty;
    }

    public bool IsCapturedVariable(ISymbol symbol)
    {
        if (symbol is not ILocalSymbol and not IParameterSymbol and not ITypeSymbol)
            return false;

        EnsureDiagnosticsCollected();

        var root = SyntaxTree.GetRoot();
        foreach (var function in root.DescendantNodes().OfType<FunctionStatementSyntax>())
        {
            if (GetDeclaredSymbol(function) is not ISymbol functionSymbol)
                continue;

            var captures = GetCapturedVariables(functionSymbol);
            if (!captures.IsDefaultOrEmpty &&
                captures.Contains(symbol, SymbolEqualityComparer.Default))
            {
                return true;
            }
        }

        foreach (var lambda in root.DescendantNodes().OfType<FunctionExpressionSyntax>())
        {
            var captures = GetCapturedVariables(lambda);
            if (!captures.IsDefaultOrEmpty &&
                captures.Contains(symbol, SymbolEqualityComparer.Default))
            {
                return true;
            }
        }

        return false;
    }

    private ImmutableArray<ISymbol> GetOrComputeFunctionCapturedVariables(SourceMethodSymbol method)
    {
        if (!method.CapturedVariables.IsDefaultOrEmpty)
            return method.CapturedVariables;

        if (method.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax() is not FunctionStatementSyntax function)
            return ImmutableArray<ISymbol>.Empty;

        BoundBlockStatement? functionBody = function.Body is not null
            ? GetBoundNode(function.Body, BoundTreeView.Original) as BoundBlockStatement
                ?? GetBoundNode(function.Body, BoundTreeView.Lowered) as BoundBlockStatement
            : GetBoundNode(function.ExpressionBody!, BoundTreeView.Original) as BoundBlockStatement
                ?? GetBoundNode(function.ExpressionBody!, BoundTreeView.Lowered) as BoundBlockStatement;

        if (functionBody is null)
            return ImmutableArray<ISymbol>.Empty;

        var captures = AnalyzeFunctionCapturedVariables(functionBody, method);
        if (!captures.IsDefaultOrEmpty)
            method.SetCapturedVariables(captures);

        return captures;
    }

    private static ImmutableArray<ISymbol> AnalyzeFunctionCapturedVariables(BoundBlockStatement body, IMethodSymbol functionSymbol)
    {
        var walker = new FunctionCapturedVariableWalker(functionSymbol);
        walker.VisitStatement(body);
        return walker.GetCapturedVariables();
    }

    private sealed class FunctionCapturedVariableWalker : BoundTreeWalker
    {
        private readonly IMethodSymbol _functionSymbol;
        private readonly HashSet<ISymbol> _captured = new(SymbolEqualityComparer.Default);

        public FunctionCapturedVariableWalker(IMethodSymbol functionSymbol)
        {
            _functionSymbol = functionSymbol;
        }

        public ImmutableArray<ISymbol> GetCapturedVariables()
        {
            if (_captured.Count == 0)
                return ImmutableArray<ISymbol>.Empty;

            return _captured.ToImmutableArray();
        }

        public override void VisitLocalAccess(BoundLocalAccess node)
        {
            AddIfCaptured(node.Symbol);
            base.VisitLocalAccess(node);
        }

        public override void VisitParameterAccess(BoundParameterAccess node)
        {
            AddIfCaptured(node.Symbol);
            base.VisitParameterAccess(node);
        }

        public override void VisitVariableExpression(BoundVariableExpression node)
        {
            AddIfCaptured(node.Symbol);
            base.VisitVariableExpression(node);
        }

        public override void VisitSelfExpression(BoundSelfExpression node)
        {
            AddIfCaptured(node.Symbol ?? node.Type);
            base.VisitSelfExpression(node);
        }

        private void AddIfCaptured(ISymbol? symbol)
        {
            if (symbol is null)
                return;

            if (symbol is ILocalSymbol or IParameterSymbol)
            {
                if (SymbolEqualityComparer.Default.Equals(symbol.ContainingSymbol, _functionSymbol))
                    return;

                // Skip variables declared in nested lambdas/functions — they are not
                // captures from the outer scope of _functionSymbol.
                if (IsNestedWithin(symbol.ContainingSymbol, _functionSymbol))
                    return;

                _captured.Add(symbol);
                return;
            }

            if (symbol is ITypeSymbol typeSymbol &&
                _functionSymbol.ContainingType is { } containingType &&
                SymbolEqualityComparer.Default.Equals(typeSymbol, containingType))
            {
                _captured.Add(typeSymbol);
            }
        }

        private static bool IsNestedWithin(ISymbol? scope, ISymbol parent)
        {
            var current = scope;
            while (current is not null)
            {
                if (SymbolEqualityComparer.Default.Equals(current, parent))
                    return true;
                current = current.ContainingSymbol;
            }
            return false;
        }
    }
}
