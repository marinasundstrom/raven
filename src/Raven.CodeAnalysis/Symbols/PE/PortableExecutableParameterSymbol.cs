using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PortableExecutableParameterSymbol : PortableExecutableSymbol, IParameterSymbol
{
    private readonly ParameterInfo _parameterInfo;
    private ITypeSymbol _type;

    public PortableExecutableParameterSymbol(ParameterInfo parameterInfo, ITypeSymbol returnType, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(containingSymbol, containingType, containingNamespace, locations)
    {
        _parameterInfo = parameterInfo;
    }

    public override SymbolKind Kind => SymbolKind.Parameter;
    public override string Name => _parameterInfo.Name;
    public ITypeSymbol Type => _type ??= PEContainingModule.GetType(_parameterInfo.ParameterType);
}