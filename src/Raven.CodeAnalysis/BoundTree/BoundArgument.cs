
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

// NOTE: For potential future use
internal readonly struct BoundArgument
{
    public BoundArgument(BoundExpression expression, RefKind refKind, string? name, SyntaxNode? syntax = null, bool isSpread = false) : this()
    {
        Name = name;
        RefKind = refKind;
        Expression = expression;
        Syntax = syntax;
        IsSpread = isSpread;
    }

    public string? Name { get; }

    // Currently handled by the expression
    public RefKind RefKind { get; }
    public BoundExpression Expression { get; }
    public SyntaxNode? Syntax { get; }
    public bool IsSpread { get; }
    public ITypeSymbol Type => Expression.Type;
}
