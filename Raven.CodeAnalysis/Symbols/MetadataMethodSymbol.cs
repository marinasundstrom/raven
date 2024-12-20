using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal class MetadataMethodSymbol : MetadataSymbol, IMethodSymbol
{
    private readonly MethodInfo _methodInfo;

    public MetadataMethodSymbol(MethodInfo methodInfo,  ITypeSymbol returnType, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(containingSymbol, containingType, containingNamespace, locations)
    {
        _methodInfo = methodInfo;
    }

    public override SymbolKind Kind => SymbolKind.Method;
    public override string Name => _methodInfo.Name;
    public ITypeSymbol ReturnType { get; }
    public ImmutableArray<IParameterSymbol> Parameters { get; set; } = [];
}