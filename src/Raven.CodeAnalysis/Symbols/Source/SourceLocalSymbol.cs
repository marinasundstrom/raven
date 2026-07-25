namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceLocalSymbol : SourceSymbol, ILocalSymbol
{
    public SourceLocalSymbol(string name, ITypeSymbol type, bool isMutable, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences, bool isConst = false, object? constantValue = null, ScopedKind scopedKind = ScopedKind.None)
        : base(SymbolKind.Local, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        Type = type;
        IsMutable = isMutable;
        IsConst = isConst;
        ConstantValue = constantValue;
        ScopedKind = scopedKind;
    }

    public ITypeSymbol Type { get; }

    public bool IsMutable { get; }

    public ScopedKind ScopedKind { get; }

    public bool IsConst { get; }

    public object? ConstantValue { get; }
}
