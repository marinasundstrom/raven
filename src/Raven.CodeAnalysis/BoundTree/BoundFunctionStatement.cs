namespace Raven.CodeAnalysis;

internal partial class BoundFunctionStatement : BoundStatement
{
    public BoundFunctionStatement(IMethodSymbol method)
    {
        Method = method;
    }

    public IMethodSymbol Method { get; }
}