using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Symbols;

internal partial class ArrayTypeSymbolImplementation : SymbolBase, ArrayTypeSymbol
{
    private ImmutableArray<NamedTypeSymbol> _interfaces;
    private ImmutableArray<NamedTypeSymbol> _allInterfaces;
    private ImmutableArray<NamedTypeSymbol> _arraySpecificInterfaces;

    public ArrayTypeSymbolImplementation(
        NamedTypeSymbol? baseType,
        TypeSymbol elementType,
        Symbol? containingSymbol,
        NamedTypeSymbol? containingType,
        NamespaceSymbol? containingNamespace,
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

    public override AssemblySymbol? ContainingAssembly => ContainingNamespace?.ContainingAssembly;

    public override ModuleSymbol? ContainingModule => ContainingNamespace?.ContainingModule;

    public override SymbolKind Kind => SymbolKind.Type;

    public TypeSymbol ElementType { get; }

    public SpecialType SpecialType => SpecialType.System_Array;

    public bool IsNamespace => false;

    public bool IsType => true;

    public int Rank { get; }

    public bool IsFixedArray => FixedLength.HasValue;

    public int? FixedLength { get; }

    public NamedTypeSymbol? BaseType { get; }

    public TypeKind TypeKind { get; }

    public TypeSymbol? OriginalDefinition { get; }

    private bool AreInterfacesComplete =>
        BaseType is not IArrayTypeProvider provider || provider.AreInterfacesComplete;

    public ImmutableArray<NamedTypeSymbol> Interfaces =>
        !AreInterfacesComplete ? ComputeInterfaces() :
        !_interfaces.IsDefault ? _interfaces : _interfaces = ComputeInterfaces();

    public ImmutableArray<NamedTypeSymbol> AllInterfaces =>
        !AreInterfacesComplete ? ComputeAllInterfaces() :
        !_allInterfaces.IsDefault ? _allInterfaces : _allInterfaces = ComputeAllInterfaces();

    public ImmutableArray<Symbol> GetMembers()
        => BaseType is IArrayTypeProvider provider
            ? provider.GetMembers(this)
            : BaseType?.GetMembers() ?? ImmutableArray<Symbol>.Empty;

    public ImmutableArray<Symbol> GetMembers(string name) => GetMembers().Where(m => m.Name == name).ToImmutableArray();

    public TypeSymbol? LookupType(string name) => BaseType?.LookupType(name);

    public override string ToString() => Name;

    public bool IsMemberDefined(string name, out Symbol? symbol)
    {
        symbol = GetMembers(name).FirstOrDefault();
        return symbol is not null;
    }

    private ImmutableArray<NamedTypeSymbol> ComputeInterfaces()
    {
        var builder = ImmutableArray.CreateBuilder<NamedTypeSymbol>();

        if (BaseType is NamedTypeSymbol baseType)
            builder.AddRange(baseType.Interfaces);

        foreach (var arrayInterface in GetArraySpecificInterfaces())
            AddUnique(builder, arrayInterface);

        return builder.ToImmutable();
    }

    private ImmutableArray<NamedTypeSymbol> ComputeAllInterfaces()
    {
        var builder = ImmutableArray.CreateBuilder<NamedTypeSymbol>();

        if (BaseType is NamedTypeSymbol baseType)
            builder.AddRange(baseType.AllInterfaces);

        foreach (var arrayInterface in GetArraySpecificInterfaces())
        {
            AddUnique(builder, arrayInterface);

            foreach (var inherited in arrayInterface.AllInterfaces)
                AddUnique(builder, inherited);
        }

        return builder.ToImmutable();
    }

    private ImmutableArray<NamedTypeSymbol> GetArraySpecificInterfaces()
    {
        if (!_arraySpecificInterfaces.IsDefault)
            return _arraySpecificInterfaces;

        var interfaces = BaseType is IArrayTypeProvider provider
            ? provider.GetAdditionalInterfaces(this)
            : ImmutableArray<NamedTypeSymbol>.Empty;
        if (AreInterfacesComplete)
            _arraySpecificInterfaces = interfaces;
        return interfaces;
    }

    private static void AddUnique(ImmutableArray<NamedTypeSymbol>.Builder builder, NamedTypeSymbol symbol)
    {
        foreach (var existing in builder)
        {
            if (SymbolEqualityComparer.Default.Equals(existing, symbol))
                return;
        }

        builder.Add(symbol);
    }
}
