using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed class LocalFunctionCaptureAnalysis : BoundTreeWalker
{
    private readonly MethodSymbol _functionSymbol;
    private readonly HashSet<Symbol> _captured = new(SymbolEqualityComparer.Default);
    private HashSet<SourceMethodSymbol>? _dependencies;

    private LocalFunctionCaptureAnalysis(MethodSymbol functionSymbol)
    {
        _functionSymbol = functionSymbol;
    }

    internal static void Update(SourceMethodSymbol function, BoundBlockStatement body)
    {
        var analysis = new LocalFunctionCaptureAnalysis(function);
        analysis.VisitStatement(body);
        function.SetLocalFunctionCaptures([.. analysis._captured], analysis._dependencies is null ? [] : [.. analysis._dependencies]);
    }

    public override void VisitInvocationExpression(BoundInvocationExpression node)
    {
        AddDependency(node.Method);
        base.VisitInvocationExpression(node);
    }

    public override void VisitMethodGroupExpression(BoundMethodGroupExpression node)
    {
        AddDependency(node.SelectedMethod);
        base.VisitMethodGroupExpression(node);
    }

    private void AddDependency(MethodSymbol? method)
    {
        if (method?.OriginalDefinition is SourceMethodSymbol source && !ReferenceEquals(source, _functionSymbol) &&
            source.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax() is FunctionStatementSyntax)
            (_dependencies ??= new(ReferenceEqualityComparer.Instance)).Add(source);
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

    private void AddIfCaptured(Symbol? symbol)
    {
        if (symbol is null)
            return;

        if (symbol is LocalSymbol or ParameterSymbol)
        {
            if (IsDeclaredWithin(symbol, _functionSymbol))
                return;

            _captured.Add(symbol);
            return;
        }

        if (symbol is TypeSymbol typeSymbol &&
            _functionSymbol.ContainingType is { } containingType &&
            SymbolEqualityComparer.Default.Equals(typeSymbol, containingType))
        {
            _captured.Add(typeSymbol);
        }
    }

    internal static bool IsDeclaredWithin(Symbol symbol, MethodSymbol function)
    {
        var current = symbol.ContainingSymbol;
        while (current is not null)
        {
            if (SymbolEqualityComparer.Default.Equals(current, function))
                return true;
            current = current.ContainingSymbol;
        }
        return false;
    }
}
