namespace Raven.CodeAnalysis;

internal partial class BoundFunctionStatement : BoundStatement
{
    public BoundFunctionStatement(MethodSymbol method)
    {
        Method = method;
    }

    public MethodSymbol Method { get; }
}
