
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

internal readonly struct BoundArgument
{
    public BoundArgument(BoundExpression expression, RefKind refKind, string? name) : this()
    {
        Name = name;
        RefKind = refKind;
        Expression = expression;
    }

    public string? Name { get; }
    public RefKind RefKind { get; }
    public BoundExpression Expression { get; }
    public ITypeSymbol Type => Expression.Type;
}