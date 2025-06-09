

namespace Raven.CodeAnalysis;

class LambdaBinder : BlockBinder
{
    private readonly Dictionary<string, IParameterSymbol> _parameters = new();

    public LambdaBinder(ISymbol containingSymbol, Binder parent) : base(containingSymbol, parent) { }

    public void DeclareParameter(IParameterSymbol param)
    {
        _parameters[param.Name] = param;
    }

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
        // Compare by reference if possible
        return _parameters.Values.Contains(symbol);
        //|| _locals.Values.Contains(symbol);
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
        if (node.Symbol is ISymbol symbol)
        {
            _accessedSymbols.Add(symbol);
        }
        base.VisitLocalAccess(node);
    }
}
