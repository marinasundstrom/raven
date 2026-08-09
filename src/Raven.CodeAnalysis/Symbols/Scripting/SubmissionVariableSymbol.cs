namespace Raven.CodeAnalysis.Symbols;

/// <summary>
/// Represents a variable imported from a script submission's persistent state.
/// </summary>
internal sealed class SubmissionVariableSymbol : Symbol, ILocalSymbol
{
    internal SubmissionVariableSymbol(ILocalSymbol variable, int slot)
        : base(
            SymbolKind.Local,
            variable.Name,
            variable.ContainingSymbol,
            variable.ContainingType,
            variable.ContainingNamespace,
            [.. variable.Locations],
            [.. variable.DeclaringSyntaxReferences],
            Accessibility.NotApplicable,
            addAsMember: false)
    {
        OriginalVariable = variable;
        Slot = slot;
    }

    internal ILocalSymbol OriginalVariable { get; }

    internal int Slot { get; }

    public ITypeSymbol Type => OriginalVariable.Type;

    public ScopedKind ScopedKind => OriginalVariable.ScopedKind;

    public bool IsMutable => OriginalVariable.IsMutable;

    public bool IsConst => OriginalVariable.IsConst;

    public object? ConstantValue => OriginalVariable.ConstantValue;

    public override IAssemblySymbol? ContainingAssembly => OriginalVariable.ContainingAssembly;

    public override IModuleSymbol? ContainingModule => OriginalVariable.ContainingModule;

    public override ISymbol UnderlyingSymbol => OriginalVariable;

    public override void Accept(SymbolVisitor visitor)
        => visitor.VisitLocal(this);

    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor)
        => visitor.VisitLocal(this);
}
