namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Represents an ordinary Raven expression whose bound result is compiler-verified
/// to be implicitly convertible to <typeparamref name="T"/>.
/// </summary>
/// <remarks>
/// This macro-only facade does not add a syntax-node kind. <see cref="Syntax"/>
/// remains the immutable, source-backed expression authored by the caller.
/// </remarks>
public sealed class ExpressionSyntax<T>
{
    internal ExpressionSyntax(Raven.CodeAnalysis.Syntax.ExpressionSyntax syntax, ITypeSymbol type)
    {
        Syntax = syntax;
        Type = type;
    }

    /// <summary>Gets the underlying ordinary Raven expression syntax.</summary>
    public Raven.CodeAnalysis.Syntax.ExpressionSyntax Syntax { get; }

    /// <summary>Gets the compiler-verified bound type of <see cref="Syntax"/>.</summary>
    public ITypeSymbol Type { get; }
}
