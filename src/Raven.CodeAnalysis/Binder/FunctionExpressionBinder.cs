using System;
using System.Collections.Generic;
using System.Linq;

namespace Raven.CodeAnalysis;

class FunctionExpressionBinder : BlockBinder
{
    private readonly Dictionary<string, IParameterSymbol> _parameters = new();
    private readonly Dictionary<string, IMethodSymbol> _functions = new();

    public FunctionExpressionBinder(ISymbol containingSymbol, Binder parent) : base(containingSymbol, parent) { }

    public void DeclareParameter(IParameterSymbol param)
    {
        _parameters[param.Name] = param;
    }

    public IEnumerable<IParameterSymbol> GetParameters() => _parameters.Values;

    public void DeclareFunction(string name, IMethodSymbol method)
    {
        _functions[name] = method;
    }

    public override ISymbol? LookupSymbol(string name)
    {
        if (_parameters.TryGetValue(name, out var param))
            return param;

        if (_functions.TryGetValue(name, out var function))
            return function;

        return base.LookupSymbol(name);
    }

    public override ITypeSymbol? LookupType(string name)
    {
        if (ContainingSymbol is IMethodSymbol method)
        {
            var methodTypeParameter = method.TypeParameters.FirstOrDefault(tp => tp.Name == name);
            if (methodTypeParameter is not null)
                return methodTypeParameter;
        }

        return base.LookupType(name);
    }

    public override BoundExpression BindExpression(ExpressionSyntax expression)
    {
        // Lambda bodies are rebound during target-typed replay; evict any cached nodes
        // (including descendants) so bindings reflect the current lambda parameter scope.
        ClearCachedBoundNodes(expression);
        return base.BindExpression(expression);
    }

    public override BoundStatement BindStatement(StatementSyntax statement)
    {
        // Avoid reusing cached statements (and descendants) bound under a different scope.
        ClearCachedBoundNodes(statement);
        return base.BindStatement(statement);
    }

    private void ClearCachedBoundNodes(SyntaxNode node)
    {
        RemoveCachedBoundNode(node);
        RemoveCachedBinder(node);
        foreach (var child in node.DescendantNodes())
        {
            RemoveCachedBoundNode(child);
            RemoveCachedBinder(child);
        }
    }

    public IReadOnlyList<ISymbol> AnalyzeCapturedVariables()
    {
        if (_lambdaBody is null)
            return [];

        return FunctionExpressionCapturedVariableWalker.Analyze(_lambdaBody, this);
    }

    public bool IsDeclaredInLambda(ISymbol symbol)
    {
        if (_parameters.Values.Contains(symbol))
            return true;

        foreach (var local in _locals.Values)
        {
            if (SymbolEqualityComparer.Default.Equals(local.Symbol, symbol))
                return true;
        }

        if (symbol is ILocalSymbol or IParameterSymbol)
        {
            if (symbol.ContainingSymbol is { } containing &&
                SymbolEqualityComparer.Default.Equals(containing, ContainingSymbol))
            {
                return true;
            }
        }

        return false;
    }

    private BoundExpression? _lambdaBody;
    public void SetLambdaBody(BoundExpression body) => _lambdaBody = body;
}

sealed class FunctionExpressionCapturedVariableWalker : BoundTreeWalker
{
    private readonly FunctionExpressionBinder _binder;
    private readonly HashSet<ISymbol> _accessedSymbols = new();

    public FunctionExpressionCapturedVariableWalker(FunctionExpressionBinder binder)
    {
        _binder = binder;
    }

    public static IReadOnlyList<ISymbol> Analyze(BoundExpression body, FunctionExpressionBinder binder)
    {
        var walker = new FunctionExpressionCapturedVariableWalker(binder);
        walker.VisitExpression(body);
        return walker._accessedSymbols
            .Where(symbol => !binder.IsDeclaredInLambda(symbol))
            .Distinct()
            .ToList();
    }

    public override void VisitLocalAccess(BoundLocalAccess node)
    {
        AddSymbol(node.Symbol);
        base.VisitLocalAccess(node);
    }

    public override void VisitParameterAccess(BoundParameterAccess node)
    {
        AddSymbol(node.Symbol);
        base.VisitParameterAccess(node);
    }

    public override void VisitVariableExpression(BoundVariableExpression node)
    {
        AddSymbol(node.Symbol);
        base.VisitVariableExpression(node);
    }

    public override void VisitSelfExpression(BoundSelfExpression node)
    {
        AddSymbol(node.Symbol);
        base.VisitSelfExpression(node);
    }

    private void AddSymbol(ISymbol? symbol)
    {
        if (symbol is not null)
            _accessedSymbols.Add(symbol);
    }
}

sealed class FunctionExpressionSelfCaptureCollector : BoundTreeWalker
{
    private readonly ISymbol _containingSymbol;
    private readonly HashSet<ISymbol> _captures = new(SymbolEqualityComparer.Default);

    private FunctionExpressionSelfCaptureCollector(ISymbol containingSymbol)
    {
        _containingSymbol = containingSymbol;
    }

    public static IEnumerable<ISymbol> Collect(BoundExpression body, ISymbol containingSymbol)
    {
        if (body is null)
            return Array.Empty<ISymbol>();

        var collector = new FunctionExpressionSelfCaptureCollector(containingSymbol);
        collector.VisitExpression(body);
        return collector._captures;
    }

    public override void VisitFunctionExpression(BoundFunctionExpression node)
    {
        foreach (var captured in node.CapturedVariables)
        {
            if (captured is not null && ShouldPropagateCapture(captured))
                _captures.Add(captured);
        }

        if (node.Unbound is { LambdaSymbol: Symbols.SourceLambdaSymbol sourceLambda } && sourceLambda.HasCaptures)
        {
            foreach (var captured in sourceLambda.CapturedVariables)
            {
                if (captured is not null && ShouldPropagateCapture(captured))
                    _captures.Add(captured);
            }
        }

        base.VisitFunctionExpression(node);
    }

    private bool ShouldPropagateCapture(ISymbol captured)
    {
        if (SymbolEqualityComparer.Default.Equals(captured.ContainingSymbol, _containingSymbol))
            return true;

        if (captured is ITypeSymbol typeSymbol && _containingSymbol.ContainingType is { } containingType)
            return SymbolEqualityComparer.Default.Equals(typeSymbol, containingType);

        return false;
    }
}
