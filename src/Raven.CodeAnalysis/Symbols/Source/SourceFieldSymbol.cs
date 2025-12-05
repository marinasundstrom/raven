using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceFieldSymbol : SourceSymbol, IFieldSymbol
{
    private readonly object _constantValue;
    private readonly bool _isMutable;
    private readonly bool _isStatic;

    public SourceFieldSymbol(string name, ITypeSymbol fieldType, bool isStatic, bool isMutable, bool isConst, object constantValue, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences, BoundExpression? initializer = null, Accessibility declaredAccessibility = Accessibility.NotApplicable)
        : base(SymbolKind.Field, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences, declaredAccessibility)
    {
        Type = fieldType;
        _isMutable = isMutable;
        _isStatic = isStatic;
        IsConst = isConst;
        _constantValue = constantValue;
        Initializer = initializer;
    }

    public ITypeSymbol Type { get; }

    public bool IsConst { get; }

    public bool IsMutable => _isMutable;

    public BoundExpression? Initializer { get; }

    public object? GetConstantValue()
    {
        return _constantValue;
    }

    public override bool IsStatic => _isStatic;
}