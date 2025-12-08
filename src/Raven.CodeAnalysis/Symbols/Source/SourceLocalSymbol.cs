namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceLocalSymbol : SourceSymbol, ILocalSymbol
{
    public SourceLocalSymbol(string name, ITypeSymbol type, bool isMutable, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences, bool isConst = false, bool isReference = false, object? constantValue = null)
        : base(SymbolKind.Local, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        Type = type;
        IsMutable = isMutable;
        IsConst = isConst;
        IsReference = isReference;
        ConstantValue = constantValue;
    }

    public ITypeSymbol Type { get; }

    public bool IsMutable { get; }

    public bool IsConst { get; }

    public bool IsReference { get; }

    public object? ConstantValue { get; }
}
