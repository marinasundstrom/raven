using System.Collections.Immutable;

using Raven.CodeAnalysis.Macros;

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
        ScopedKind scopedKind = ScopedKind.None,
        MacroParameterRole macroRole = MacroParameterRole.None,
        bool hasImplicitName = false)
        : base(SymbolKind.Parameter, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        Type = parameterType;
        RefKind = refKind;
        HasExplicitDefaultValue = hasExplicitDefaultValue;
        ExplicitDefaultValue = explicitDefaultValue;
        IsMutable = isMutable;
        IsVarParams = isVarParams;
        MacroRole = macroRole;
        HasImplicitName = hasImplicitName;
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

    public bool HasImplicitName { get; }

    public bool IsVarParams { get; }

    public RefKind RefKind { get; }

    public ScopedKind ScopedKind { get; }

    public MacroParameterRole MacroRole { get; }

    public bool HasExplicitDefaultValue { get; }

    public object? ExplicitDefaultValue { get; }

    public bool IsOptional => HasExplicitDefaultValue;

    public bool IsMutable { get; }

    public override ImmutableArray<AttributeData> GetAttributes()
    {
        var attributes = base.GetAttributes();
        if (!HasImplicitName)
            return attributes;

        var compilerGenerated = CreateCompilerGeneratedAttribute();
        return compilerGenerated is null ? attributes : attributes.Add(compilerGenerated);
    }
}
