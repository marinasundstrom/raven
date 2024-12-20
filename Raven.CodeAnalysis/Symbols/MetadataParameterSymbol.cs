using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal class MetadataParameterSymbol : MetadataSymbol, IParameterSymbol
{
    private readonly ParameterInfo _parameterInfo;

    public MetadataParameterSymbol(ParameterInfo parameterInfo,  ITypeSymbol returnType, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(containingSymbol, containingType, containingNamespace, locations)
    {
        _parameterInfo = parameterInfo;
    }

    public override SymbolKind Kind => SymbolKind.Parameter;
    public override string Name => _parameterInfo.Name;
    public ITypeSymbol Type { get; }
}