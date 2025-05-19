using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class MetadataParameterSymbol : MetadataSymbol, IParameterSymbol
{
    private readonly ParameterInfo _parameterInfo;
    private ITypeSymbol _type;

    public MetadataParameterSymbol(ParameterInfo parameterInfo, ITypeSymbol returnType, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(containingSymbol, containingType, containingNamespace, locations)
    {
        _parameterInfo = parameterInfo;
    }

    public override SymbolKind Kind => SymbolKind.Parameter;
    public override string Name => _parameterInfo.Name;
    public ITypeSymbol Type => _type ??= Compilation.GetType(_parameterInfo.ParameterType);
}