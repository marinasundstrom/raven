namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceFieldSymbol : SourceSymbol, IFieldSymbol
{
    private readonly object _constantValue;
    private readonly bool _isStatic;

    public SourceFieldSymbol(string name, ITypeSymbol fieldType, bool isStatic, bool isLiteral, object constantValue, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Field, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        Type = fieldType;
        _isStatic = isStatic;
        IsLiteral = isLiteral;
        _constantValue = constantValue;
    }

    public ITypeSymbol Type { get; }

    public bool IsLiteral { get; }

    public object? GetConstantValue()
    {
        return _constantValue;
    }

    public override bool IsStatic => _isStatic;
}