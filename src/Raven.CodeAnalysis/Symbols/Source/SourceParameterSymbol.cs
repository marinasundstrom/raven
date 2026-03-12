namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceParameterSymbol : SourceSymbol, IParameterSymbol
{
    public SourceParameterSymbol(
        string name,
        ITypeSymbol parameterType,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        RefKind refKind = RefKind.None,
        bool hasExplicitDefaultValue = false,
        object? explicitDefaultValue = null,
        bool isMutable = false,
        bool isVarParams = false)
        : base(SymbolKind.Parameter, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        Type = parameterType;
        RefKind = refKind;
        HasExplicitDefaultValue = hasExplicitDefaultValue;
        ExplicitDefaultValue = explicitDefaultValue;
        IsMutable = isMutable;
        IsVarParams = isVarParams;
    }

    public ITypeSymbol Type { get; }

    public bool IsVarParams { get; }

    public RefKind RefKind { get; }

    public bool HasExplicitDefaultValue { get; }

    public object? ExplicitDefaultValue { get; }

    public bool IsOptional => HasExplicitDefaultValue;

    public bool IsMutable { get; }
}
