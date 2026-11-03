namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceLocalSymbol : SourceSymbol, LocalSymbol
{
    public SourceLocalSymbol(string name, TypeSymbol type, bool isMutable, Symbol containingSymbol, NamedTypeSymbol? containingType, NamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences, bool isConst = false, object? constantValue = null, ScopedKind scopedKind = ScopedKind.None, bool isImplicitlyDeclared = false)
        : base(SymbolKind.Local, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        Type = type;
        IsMutable = isMutable;
        IsConst = isConst;
        ConstantValue = constantValue;
        ScopedKind = scopedKind;
        IsImplicitlyDeclared = isImplicitlyDeclared;
    }

    public TypeSymbol Type { get; }

    public bool IsMutable { get; }

    public ScopedKind ScopedKind { get; }

    public bool IsConst { get; }

    public object? ConstantValue { get; }

    public override bool IsImplicitlyDeclared { get; }
}
