using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Symbols;

internal partial class ArrayTypeSymbol : Symbol, IArrayTypeSymbol
{
    private ImmutableArray<INamedTypeSymbol> _interfaces;
    private ImmutableArray<INamedTypeSymbol> _allInterfaces;
    private ImmutableArray<INamedTypeSymbol> _arraySpecificInterfaces;

    public ArrayTypeSymbol(
        INamedTypeSymbol? baseType,
        ITypeSymbol elementType,
        ISymbol? containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        int rank = 1,
        int? fixedLength = null)
        : base(containingSymbol, containingType, containingNamespace, locations, [], addAsMember: false)
    {
        BaseType = baseType;
        ElementType = elementType;
        Rank = rank;
        FixedLength = rank == 1 ? fixedLength : null;

        TypeKind = TypeKind.Array;
    }

    public override string Name
    {
        get
        {
            var suffix = Rank == 1
                ? FixedLength is int fixedLength ? $"[{fixedLength}]" : "[]"
                : "[" + new string(',', Rank - 1) + "]";

            return $"{ElementType}{suffix}";
        }
    }

    public override IAssemblySymbol? ContainingAssembly => ContainingNamespace?.ContainingAssembly;

    public override IModuleSymbol? ContainingModule => ContainingNamespace?.ContainingModule;

    public override SymbolKind Kind => SymbolKind.Type;

    public ITypeSymbol ElementType { get; }

    public SpecialType SpecialType => SpecialType.System_Array;

    public bool IsNamespace => false;

    public bool IsType => true;

    public int Rank { get; }

    public bool IsFixedArray => FixedLength.HasValue;

    public int? FixedLength { get; }

    public INamedTypeSymbol? BaseType { get; }

    public TypeKind TypeKind { get; }

    public ITypeSymbol? OriginalDefinition { get; }

    private bool AreInterfacesComplete =>
        BaseType is not IArrayTypeProvider provider || provider.AreInterfacesComplete;

    public ImmutableArray<INamedTypeSymbol> Interfaces =>
        !AreInterfacesComplete ? ComputeInterfaces() :
        !_interfaces.IsDefault ? _interfaces : _interfaces = ComputeInterfaces();

    public ImmutableArray<INamedTypeSymbol> AllInterfaces =>
        !AreInterfacesComplete ? ComputeAllInterfaces() :
        !_allInterfaces.IsDefault ? _allInterfaces : _allInterfaces = ComputeAllInterfaces();

    public ImmutableArray<ISymbol> GetMembers()
        => BaseType is IArrayTypeProvider provider
            ? provider.GetMembers(this)
            : BaseType?.GetMembers() ?? ImmutableArray<ISymbol>.Empty;

    public ImmutableArray<ISymbol> GetMembers(string name) => GetMembers().Where(m => m.Name == name).ToImmutableArray();

    public ITypeSymbol? LookupType(string name) => BaseType?.LookupType(name);

    public override string ToString() => Name;

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        symbol = GetMembers(name).FirstOrDefault();
        return symbol is not null;
    }

    private ImmutableArray<INamedTypeSymbol> ComputeInterfaces()
    {
        var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();

        if (BaseType is INamedTypeSymbol baseType)
            builder.AddRange(baseType.Interfaces);

        foreach (var arrayInterface in GetArraySpecificInterfaces())
            AddUnique(builder, arrayInterface);

        return builder.ToImmutable();
    }

    private ImmutableArray<INamedTypeSymbol> ComputeAllInterfaces()
    {
        var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();

        if (BaseType is INamedTypeSymbol baseType)
            builder.AddRange(baseType.AllInterfaces);

        foreach (var arrayInterface in GetArraySpecificInterfaces())
        {
            AddUnique(builder, arrayInterface);

            foreach (var inherited in arrayInterface.AllInterfaces)
                AddUnique(builder, inherited);
        }

        return builder.ToImmutable();
    }

    private ImmutableArray<INamedTypeSymbol> GetArraySpecificInterfaces()
    {
        if (!_arraySpecificInterfaces.IsDefault)
            return _arraySpecificInterfaces;

        var interfaces = BaseType is IArrayTypeProvider provider
            ? provider.GetAdditionalInterfaces(this)
            : ImmutableArray<INamedTypeSymbol>.Empty;
        if (AreInterfacesComplete)
            _arraySpecificInterfaces = interfaces;
        return interfaces;
    }

    private static void AddUnique(ImmutableArray<INamedTypeSymbol>.Builder builder, INamedTypeSymbol symbol)
    {
        foreach (var existing in builder)
        {
            if (SymbolEqualityComparer.Default.Equals(existing, symbol))
                return;
        }

        builder.Add(symbol);
    }
}
