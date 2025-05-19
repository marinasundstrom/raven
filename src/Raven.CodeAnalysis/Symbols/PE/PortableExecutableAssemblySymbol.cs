using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PortableExecutableAssemblySymbol : PortableExecutableSymbol, IAssemblySymbol
{
    private readonly Assembly _assembly;

    public PortableExecutableAssemblySymbol(Assembly assembly, Location[] locations)
        : base(null!, null, null, locations)
    {
        _assembly = assembly;
    }

    public override SymbolKind Kind => SymbolKind.Assembly;

    public override string Name => _assembly.GetName().Name!;

    public string FullName => _assembly.GetName().FullName;

    public INamespaceSymbol GlobalNamespace => throw new NotImplementedException();

    public IEnumerable<IModuleSymbol> Modules => throw new NotImplementedException();

    public INamedTypeSymbol? GetTypeByMetadataName(string fullyQualifiedPortableExecutableName)
    {
        throw new NotImplementedException();
    }
}
