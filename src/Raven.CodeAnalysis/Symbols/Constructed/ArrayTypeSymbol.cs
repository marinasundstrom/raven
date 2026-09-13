using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis.Symbols;

internal partial class ArrayTypeSymbol : PESymbol, IArrayTypeSymbol
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
        : base(containingSymbol, containingType, containingNamespace, locations, addAsMember: false)
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

    public ImmutableArray<INamedTypeSymbol> Interfaces =>
        !_interfaces.IsDefault ? _interfaces : _interfaces = ComputeInterfaces();

    public ImmutableArray<INamedTypeSymbol> AllInterfaces =>
        !_allInterfaces.IsDefault ? _allInterfaces : _allInterfaces = ComputeAllInterfaces();

    public ImmutableArray<ISymbol> GetMembers()
    {
        var members = BaseType!.GetMembers();
        if (Rank != 1 || BaseType is not PENamedTypeSymbol metadataBase ||
            metadataBase.Compilation.Options.RuntimeIterationContract?.ArrayShapeTypeName is null)
            return members;
        // Interface members keep their interface owner so calls use normal dispatch.
        return members.AddRange(GetArraySpecificInterfaces().SelectMany(i => i.GetMembers())
            .Where(m => !m.IsStatic && !members.Any(existing => existing.Name == m.Name)));
    }

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

        if (Rank != 1)
        {
            _arraySpecificInterfaces = ImmutableArray<INamedTypeSymbol>.Empty;
            return _arraySpecificInterfaces;
        }

        var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();

        // A target can describe its vector interfaces on a regular generic class.
        // Explicit but invalid metadata must not invent host-runtime interfaces.
        if (BaseType is PENamedTypeSymbol shapeBase &&
            shapeBase.Compilation.Options.RuntimeIterationContract is { ArrayShapeTypeName: not null } shapeContract)
        {
            var shape = shapeBase.Compilation.GetTypeByMetadataName(shapeContract.ArrayShapeTypeName);
            if (shape is { TypeKind: TypeKind.Class, Arity: 1 } &&
                shape.ContainingAssembly?.Name == shapeContract.AssemblyName &&
                shape.Construct(ElementType) is INamedTypeSymbol constructedShape)
            {
                foreach (var implemented in constructedShape.AllInterfaces)
                    AddUnique(builder, implemented);
            }
            _arraySpecificInterfaces = builder.ToImmutable();
            return _arraySpecificInterfaces;
        }

        if (BaseType is PENamedTypeSymbol metadataBase &&
            metadataBase.Compilation.Options.RuntimeIterationContract is { ArraysImplementIterable: true } contract)
        {
            var definition = metadataBase.Compilation.GetTypeByMetadataName(contract.IterableTypeName);
            if (definition is { TypeKind: TypeKind.Interface, Arity: 1 } &&
                definition.ContainingAssembly?.Name == contract.AssemblyName &&
                definition.Construct(ElementType) is INamedTypeSymbol constructed)
                AddUnique(builder, constructed);
            _arraySpecificInterfaces = builder.ToImmutable();
            return _arraySpecificInterfaces;
        }

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
