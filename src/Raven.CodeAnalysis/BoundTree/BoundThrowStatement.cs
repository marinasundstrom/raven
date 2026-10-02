namespace Raven.CodeAnalysis;

internal partial class BoundThrowStatement : BoundStatement
{
    public BoundThrowStatement(BoundExpression expression, string? compilerFailure = null)
    {
        Expression = expression;
        CompilerFailure = compilerFailure;
    }

    // Only compiler-generated invariant guards may opt into target terminal failure.
    public string? CompilerFailure { get; }

    public BoundExpression Expression { get; }
}
