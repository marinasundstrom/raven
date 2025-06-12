
namespace Raven.CodeAnalysis;

internal partial class BoundObjectCreationExpression : BoundExpression
{
    public IMethodSymbol Constructor { get; }
    public IEnumerable<BoundExpression> Arguments { get; }
    public BoundExpression? Receiver { get; }

    public BoundObjectCreationExpression(IMethodSymbol constructor, IEnumerable<BoundExpression> arguments, BoundExpression? receiver = null)
           : base(constructor.ContainingType!, constructor, BoundExpressionReason.None)
    {
        Constructor = constructor;
        Arguments = arguments;
        Receiver = receiver;
    }

    public override string ToString() =>
        $"new {Constructor.ContainingType.Name}({string.Join(", ", Arguments.Select(a => a.ToString()))})";
}