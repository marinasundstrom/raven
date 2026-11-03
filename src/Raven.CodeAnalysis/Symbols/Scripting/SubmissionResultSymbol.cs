namespace Raven.CodeAnalysis.Symbols;

/// <summary>
/// Represents the compiler-owned destination for a submission's trailing value.
/// </summary>
internal sealed class SubmissionResultSymbol : SymbolBase, LocalSymbol
{
    internal SubmissionResultSymbol(TypeSymbol type, MethodSymbol containingMethod)
        : base(
            SymbolKind.Local,
            "<submission-result>",
            containingMethod,
            containingMethod.ContainingType,
            containingMethod.ContainingNamespace,
            [],
            [],
            Accessibility.NotApplicable,
            addAsMember: false)
    {
        Type = type;
    }

    public TypeSymbol Type { get; }

    public ScopedKind ScopedKind => ScopedKind.None;

    public bool IsMutable => false;

    public bool IsConst => false;

    public object? ConstantValue => null;

    public override void Accept(SymbolVisitor visitor)
        => visitor.VisitLocal(this);

    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor)
        => visitor.VisitLocal(this);
}
