namespace Raven.CodeAnalysis;

internal partial class BoundLocalFunctionStatement : BoundStatement
{
    public BoundLocalFunctionStatement(IMethodSymbol method)
    {
        Method = method;
    }

    public IMethodSymbol Method { get; }
}