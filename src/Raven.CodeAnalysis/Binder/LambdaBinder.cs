using System.Collections.Generic;

namespace Raven.CodeAnalysis;

class LambdaBinder : BlockBinder
{
    private readonly Dictionary<string, IParameterSymbol> _parameters = new();

    public LambdaBinder(ISymbol containingSymbol, Binder parent) : base(containingSymbol, parent) { }

    public void DeclareParameter(IParameterSymbol param)
    {
        _parameters[param.Name] = param;
    }

    public IEnumerable<IParameterSymbol> GetParameters() => _parameters.Values;

    public override ISymbol? LookupSymbol(string name)
    {
        if (_parameters.TryGetValue(name, out var param))
            return param;

        return base.LookupSymbol(name);
    }

    public IReadOnlyList<ISymbol> AnalyzeCapturedVariables()
    {
        if (_lambdaBody is null)
            return [];

        return CapturedVariableWalker.Analyze(_lambdaBody, this);
    }

    public bool IsDeclaredInLambda(ISymbol symbol)
    {
        if (_parameters.Values.Contains(symbol))
            return true;

        foreach (var local in _locals.Values)
        {
            if (ReferenceEquals(local.Symbol, symbol))
                return true;
        }

        return false;
    }

    private BoundExpression? _lambdaBody;
    public void SetLambdaBody(BoundExpression body) => _lambdaBody = body;
}

sealed class CapturedVariableWalker : BoundTreeWalker
{
    private readonly LambdaBinder _binder;
    private readonly HashSet<ISymbol> _accessedSymbols = new();

    public CapturedVariableWalker(LambdaBinder binder)
    {
        _binder = binder;
    }

    public static IReadOnlyList<ISymbol> Analyze(BoundExpression body, LambdaBinder binder)
    {
        var walker = new CapturedVariableWalker(binder);
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
