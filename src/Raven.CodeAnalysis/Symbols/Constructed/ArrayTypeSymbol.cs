using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal partial class ArrayTypeSymbol : PESymbol, IArrayTypeSymbol
{
    private ImmutableArray<INamedTypeSymbol> _interfaces;
    private ImmutableArray<INamedTypeSymbol> _allInterfaces;
    private ImmutableArray<INamedTypeSymbol> _arraySpecificInterfaces;

    public ArrayTypeSymbol(
        INamedTypeSymbol baseType,
        ITypeSymbol elementType,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        int rank = 1)
        : base(containingSymbol, containingType, containingNamespace, locations, addAsMember: false)
    {
        BaseType = baseType;
        ElementType = elementType;
        Rank = rank;

        TypeKind = TypeKind.Array;
    }

    public override string Name
    {
        get
        {
            var suffix = Rank == 1
                ? "[]"
                : "[" + new string(',', Rank - 1) + "]";

            return $"{ElementType}{suffix}";
        }
    }

    public override SymbolKind Kind => SymbolKind.Type;

    public ITypeSymbol ElementType { get; }

    public SpecialType SpecialType => SpecialType.System_Array;

    public bool IsNamespace => false;

    public bool IsType => true;

    public int Rank { get; }

    public INamedTypeSymbol? BaseType { get; }

    public TypeKind TypeKind { get; }

    public ITypeSymbol? OriginalDefinition { get; }

    public ImmutableArray<INamedTypeSymbol> Interfaces =>
        !_interfaces.IsDefault ? _interfaces : _interfaces = ComputeInterfaces();

    public ImmutableArray<INamedTypeSymbol> AllInterfaces =>
        !_allInterfaces.IsDefault ? _allInterfaces : _allInterfaces = ComputeAllInterfaces();

    public ImmutableArray<ISymbol> GetMembers() => BaseType!.GetMembers();

    public ImmutableArray<ISymbol> GetMembers(string name) => BaseType!.GetMembers(name);

    public ITypeSymbol? LookupType(string name)
    {
        throw new NotImplementedException();
    }

    public override string ToString() => Name;

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        throw new NotSupportedException();
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

        if (Rank != 1)
        {
            _arraySpecificInterfaces = ImmutableArray<INamedTypeSymbol>.Empty;
            return _arraySpecificInterfaces;
        }

        var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();

        AddConstructedInterface(builder, "System.Collections.Generic.IEnumerable`1");
        AddConstructedInterface(builder, "System.Collections.Generic.ICollection`1");
        AddConstructedInterface(builder, "System.Collections.Generic.IList`1");
        AddConstructedInterface(builder, "System.Collections.Generic.IReadOnlyCollection`1");
        AddConstructedInterface(builder, "System.Collections.Generic.IReadOnlyList`1");

        _arraySpecificInterfaces = builder.ToImmutable();
        return _arraySpecificInterfaces;
    }

    private void AddConstructedInterface(ImmutableArray<INamedTypeSymbol>.Builder builder, string metadataName)
    {
        if (TryResolveInterface(metadataName) is not INamedTypeSymbol definition)
            return;

        if (!definition.IsGenericType || definition.Arity != 1)
            return;

        if (definition.Construct(ElementType) is not INamedTypeSymbol constructed)
            return;

        AddUnique(builder, constructed);
    }

    private INamedTypeSymbol? TryResolveInterface(string metadataName)
    {
        if (BaseType?.ContainingAssembly?.GetTypeByMetadataName(metadataName) is INamedTypeSymbol resolved)
            return resolved;

        return ContainingAssembly?.GetTypeByMetadataName(metadataName);
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
