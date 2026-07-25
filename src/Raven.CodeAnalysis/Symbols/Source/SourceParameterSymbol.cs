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
        bool isVarParams = false,
        ScopedKind scopedKind = ScopedKind.None)
        : base(SymbolKind.Parameter, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        Type = parameterType;
        RefKind = refKind;
        HasExplicitDefaultValue = hasExplicitDefaultValue;
        ExplicitDefaultValue = explicitDefaultValue;
        IsMutable = isMutable;
        IsVarParams = isVarParams;
        ScopedKind = scopedKind != ScopedKind.None
            ? scopedKind
            : refKind switch
            {
                RefKind.Out => ScopedKind.ScopedRef,
                RefKind.Ref when SemanticFacts.MayBeRefLike(parameterType) => ScopedKind.ScopedRef,
                _ => ScopedKind.None,
            };
    }

    public ITypeSymbol Type { get; }

    public bool IsVarParams { get; }

    public RefKind RefKind { get; }

    public ScopedKind ScopedKind { get; }

    public bool HasExplicitDefaultValue { get; }

    public object? ExplicitDefaultValue { get; }

    public bool IsOptional => HasExplicitDefaultValue;

    public bool IsMutable { get; }
}
