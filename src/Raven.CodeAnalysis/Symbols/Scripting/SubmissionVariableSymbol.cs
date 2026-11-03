namespace Raven.CodeAnalysis.Symbols;

/// <summary>
/// Represents a variable imported from a script submission's persistent state.
/// </summary>
internal sealed class SubmissionVariableSymbol : SymbolBase, LocalSymbol
{
    internal SubmissionVariableSymbol(LocalSymbol variable, int slot)
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

    internal LocalSymbol OriginalVariable { get; }

    internal int Slot { get; }

    public TypeSymbol Type => OriginalVariable.Type;

    public ScopedKind ScopedKind => OriginalVariable.ScopedKind;

    public bool IsMutable => OriginalVariable.IsMutable;

    public bool IsConst => OriginalVariable.IsConst;

    public object? ConstantValue => OriginalVariable.ConstantValue;

    public override AssemblySymbol? ContainingAssembly => OriginalVariable.ContainingAssembly;

    public override ModuleSymbol? ContainingModule => OriginalVariable.ContainingModule;

    public override Symbol UnderlyingSymbol => OriginalVariable;

    public override void Accept(SymbolVisitor visitor)
        => visitor.VisitLocal(this);

    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor)
        => visitor.VisitLocal(this);
}
