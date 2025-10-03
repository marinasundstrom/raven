using System;
using System.Collections.Immutable;
using System.Linq;
using System.Net.NetworkInformation;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PENamedTypeSymbol : PESymbol, INamedTypeSymbol
{
    private readonly TypeResolver _typeResolver;
    protected readonly System.Reflection.TypeInfo _typeInfo;
    private readonly List<ISymbol> _members = new List<ISymbol>();
    private INamedTypeSymbol? _baseType;
    private bool _membersLoaded;
    private ImmutableArray<ITypeParameterSymbol>? _typeParameters;
    private string _name;
    private ImmutableArray<INamedTypeSymbol>? _interfaces;
    private ImmutableArray<INamedTypeSymbol>? _allInterfaces;
    private readonly ITypeSymbol? _constructedFrom;
    private readonly ITypeSymbol? _originalDefinition;

    public PENamedTypeSymbol(TypeResolver typeResolver, System.Reflection.TypeInfo typeInfo, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(containingSymbol, containingType, containingNamespace, locations)
    {
        _typeResolver = typeResolver;
        _typeInfo = typeInfo;

        if (typeInfo?.Name == "Object" || typeInfo.BaseType?.Name == "Object")
        {
            TypeKind = TypeKind.Class;
            (_constructedFrom, _originalDefinition) = ResolveGenericOrigins();
            return;
        }

        if (typeInfo.IsEnum)
            TypeKind = TypeKind.Enum;
        else if (IsDelegateType(typeInfo))
            TypeKind = TypeKind.Delegate;
        else if (typeInfo.IsValueType)
            TypeKind = TypeKind.Struct;
        else if (typeInfo.IsInterface)
            TypeKind = TypeKind.Interface;
        else if (typeInfo.IsPointer)
            TypeKind = TypeKind.Pointer;
        else if (typeInfo.IsArray)
            TypeKind = TypeKind.Array;
        else
            TypeKind = TypeKind.Class;

        (_constructedFrom, _originalDefinition) = ResolveGenericOrigins();
    }

    public override SymbolKind Kind => SymbolKind.Type;
    public override string Name => _name ??= _typeInfo.IsGenericType ? StripArity(_typeInfo.Name) : _typeInfo.Name;

    string StripArity(string name)
    {
        var index = name.IndexOf('`');
        return index >= 0 ? name.Substring(0, index) : name;
    }

    public override string MetadataName => _typeInfo.Name;

    public bool IsNamespace { get; } = false;
    public bool IsType { get; } = true;
    public bool IsValueType => _typeInfo.IsValueType;

    //public bool IsInterface => _typeInfo.IsInterface;

    public ImmutableArray<IMethodSymbol> Constructors => GetMembers(".ctor").OfType<IMethodSymbol>().ToImmutableArray();
    public IMethodSymbol? StaticConstructor { get; }
    public ImmutableArray<ITypeSymbol> TypeArguments { get; }
    public ImmutableArray<ITypeParameterSymbol> TypeParameters =>
        _typeParameters ??=
            _typeInfo.GenericTypeParameters
                .Select(t => (ITypeParameterSymbol)_typeResolver.ResolveType(t)!)
                .ToImmutableArray();
    public ITypeSymbol? ConstructedFrom => _constructedFrom;

    private (ITypeSymbol constructedFrom, ITypeSymbol originalDefinition) ResolveGenericOrigins()
    {
        var self = (ITypeSymbol)this;
        var constructedFrom = self;
        var originalDefinition = self;

        if (!_typeInfo.IsGenericType)
            return (constructedFrom, originalDefinition);

        if (_typeInfo.IsGenericTypeDefinition)
            return (constructedFrom, originalDefinition);

        var type = _typeInfo.AsType();
        var definitionType = type.GetGenericTypeDefinition();

        if (ReferenceEquals(definitionType, type))
            return (constructedFrom, originalDefinition);

        var resolved = _typeResolver.ResolveType(definitionType);
        if (resolved is INamedTypeSymbol definitionSymbol)
        {
            constructedFrom = definitionSymbol;

            originalDefinition = definitionSymbol.OriginalDefinition ?? definitionSymbol;
        }

        return (constructedFrom, originalDefinition);
    }
    public bool IsAbstract => _typeInfo.IsAbstract;
    public bool IsSealed => _typeInfo.IsSealed;
    public bool IsGenericType => _typeInfo.IsGenericType;
    public bool IsUnboundGenericType => _typeInfo.IsGenericTypeDefinition;
    public ImmutableArray<INamedTypeSymbol> Interfaces
    {
        get
        {
            if (_interfaces is null)
            {
                var all = AllInterfaces;
                _interfaces = all
                    .Where(i => (BaseType is null || !BaseType.AllInterfaces.Contains(i, SymbolEqualityComparer.Default)) &&
                                !all.Any(other => !SymbolEqualityComparer.Default.Equals(i, other) &&
                                                 other.AllInterfaces.Contains(i, SymbolEqualityComparer.Default)))
                    .ToImmutableArray();
            }

            return _interfaces.Value;
        }
    }

    public ImmutableArray<INamedTypeSymbol> AllInterfaces =>
        _allInterfaces ??= _typeInfo.GetInterfaces()
            .Select(i => (INamedTypeSymbol)_typeResolver.ResolveType(i)!)
            .ToImmutableArray();

    private static bool IsDelegateType(System.Reflection.TypeInfo typeInfo)
    {
        if (typeInfo.FullName == typeof(MulticastDelegate).FullName ||
            typeInfo.FullName == typeof(Delegate).FullName)
        {
            return true;
        }

        if (typeof(MulticastDelegate).IsAssignableFrom(typeInfo))
            return true;

        var baseType = typeInfo.BaseType;
        while (baseType is not null)
        {
            if (baseType.FullName == typeof(MulticastDelegate).FullName)
                return true;

            baseType = baseType.BaseType;
        }

        return false;
    }

    public SpecialType SpecialType
    {
        get
        {
            var type = _typeInfo.AsType();

            if (type.FullName == "System.Object")
                return SpecialType.System_Object;
            if (type.FullName == "System.String")
                return SpecialType.System_String;
            if (type.FullName == "System.Boolean")
                return SpecialType.System_Boolean;
            if (type.FullName == "System.Char")
                return SpecialType.System_Char;
            if (type.FullName == "System.SByte")
                return SpecialType.System_SByte;
            if (type.FullName == "System.Byte")
                return SpecialType.System_Byte;
            if (type.FullName == "System.Int16")
                return SpecialType.System_Int16;
            if (type.FullName == "System.UInt16")
                return SpecialType.System_UInt16;
            if (type.FullName == "System.Int32")
                return SpecialType.System_Int32;
            if (type.FullName == "System.UInt32")
                return SpecialType.System_UInt32;
            if (type.FullName == "System.Int64")
                return SpecialType.System_Int64;
            if (type.FullName == "System.UInt64")
                return SpecialType.System_UInt64;
            if (type.FullName == "System.Single")
                return SpecialType.System_Single;
            if (type.FullName == "System.Double")
                return SpecialType.System_Double;
            if (type.FullName == "System.Decimal")
                return SpecialType.System_Decimal;
            if (type.FullName == "System.Void")
                return SpecialType.System_Void;
            if (type.FullName == "System.DateTime")
                return SpecialType.System_DateTime;
            if (type.FullName == "System.IntPtr")
                return SpecialType.System_IntPtr;
            if (type.FullName == "System.UIntPtr")
                return SpecialType.System_UIntPtr;
            if (type.FullName == "System.Type")
                return SpecialType.System_Type;

            if (type.FullName == "System.Nullable`1")
                return SpecialType.System_Nullable_T;

            if (type.Namespace == "System" && type.Name.StartsWith("ValueTuple`"))
            {
                return type.GetGenericArguments().Length switch
                {
                    1 => SpecialType.System_ValueTuple_T1,
                    2 => SpecialType.System_ValueTuple_T2,
                    3 => SpecialType.System_ValueTuple_T3,
                    4 => SpecialType.System_ValueTuple_T4,
                    5 => SpecialType.System_ValueTuple_T5,
                    6 => SpecialType.System_ValueTuple_T6,
                    7 => SpecialType.System_ValueTuple_T7,
                    _ => SpecialType.None
                };
            }

            if (type.FullName == "System.Array" || type.IsArray)
                return SpecialType.System_Array;

            return SpecialType.None;
        }
    }

    public INamedTypeSymbol? BaseType => _baseType ??= (_typeInfo.BaseType is not null ? (INamedTypeSymbol?)PEContainingModule.GetType(_typeInfo.BaseType) : null);

    public TypeKind TypeKind { get; }

    public ITypeSymbol? OriginalDefinition => _originalDefinition;

    public int Arity => _typeInfo.GenericTypeParameters.Length;

    private ImmutableArray<IFieldSymbol>? _tupleElements;

    public INamedTypeSymbol UnderlyingTupleType => this;

    public ImmutableArray<IFieldSymbol> TupleElements
    {
        get
        {
            if (!IsValueTupleSpecialType())
                return ImmutableArray<IFieldSymbol>.Empty;

            if (_tupleElements is not null)
                return _tupleElements.Value;

            EnsureMembersLoaded();

            _tupleElements = _members
                .OfType<IFieldSymbol>()
                .Where(@field => @field.Name.StartsWith("Item", StringComparison.Ordinal))
                .OrderBy(@field => @field.Name, StringComparer.Ordinal)
                .ToImmutableArray();

            return _tupleElements.Value;
        }
    }

    public ImmutableArray<ISymbol> GetMembers()
    {
        EnsureMembersLoaded();
        return _members.ToImmutableArray();
    }

    public ImmutableArray<ISymbol> GetMembers(string name)
    {
        EnsureMembersLoaded();
        return _members.Where(x => x.Name == name).ToImmutableArray();
    }

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        symbol = _members.FirstOrDefault(m => m.Name == name);
        return symbol is not null;
    }

    public ITypeSymbol? LookupType(string name)
    {
        EnsureMembersLoaded();
        return _members
            .OfType<INamedTypeSymbol>()
            .FirstOrDefault(type => type.Name == name);
    }

    private bool IsValueTupleSpecialType()
    {
        return SpecialType is SpecialType.System_ValueTuple_T1
            or SpecialType.System_ValueTuple_T2
            or SpecialType.System_ValueTuple_T3
            or SpecialType.System_ValueTuple_T4
            or SpecialType.System_ValueTuple_T5
            or SpecialType.System_ValueTuple_T6
            or SpecialType.System_ValueTuple_T7;
    }

    internal void AddMember(ISymbol member)
    {
        _members.Add(member);
    }

    private void EnsureMembersLoaded()
    {
        if (_membersLoaded)
            return;

        _membersLoaded = true;

        foreach (var methodInfo in _typeInfo.DeclaredMethods)
        {
            if (methodInfo.IsSpecialName)
                continue;

            new PEMethodSymbol(
                _typeResolver,
                methodInfo,
                this,
                [new MetadataLocation(ContainingModule!)]);
        }

        foreach (var propertyInfo in _typeInfo.DeclaredProperties)
        {
            var property = new PEPropertySymbol(
                _typeResolver,
                propertyInfo,
                this,
                [new MetadataLocation(ContainingModule!)]);

            if (propertyInfo.GetMethod is not null)
            {
                property.GetMethod = new PEMethodSymbol(
                    _typeResolver,
                    propertyInfo.GetMethod,
                    property,
                    this,
                    [new MetadataLocation(ContainingModule!)]);
            }

            if (propertyInfo.SetMethod is not null)
            {
                property.SetMethod = new PEMethodSymbol(
                    _typeResolver,
                    propertyInfo.SetMethod,
                    property,
                    this,
                    [new MetadataLocation(ContainingModule!)]);
            }
        }

        foreach (var fieldInfo in _typeInfo.DeclaredFields)
        {
            if (fieldInfo.IsSpecialName)
                continue;

            new PEFieldSymbol(
                _typeResolver,
                fieldInfo,
                this,
                [new MetadataLocation(ContainingModule!)]);
        }

        foreach (var constructorInfo in _typeInfo.DeclaredConstructors)
        {
            new PEMethodSymbol(
                _typeResolver,
                constructorInfo,
                this,
                [new MetadataLocation(ContainingModule!)]);
        }

        foreach (var nestedTypeInfo in _typeInfo.DeclaredNestedTypes)
        {
            new PENamedTypeSymbol(
                _typeResolver,
                nestedTypeInfo,
                this,
                this,
                this.ContainingNamespace,
                [new MetadataLocation(ContainingModule!)]);
        }
    }

    public System.Reflection.TypeInfo GetTypeInfo() => _typeInfo;

    public ITypeSymbol Construct(params ITypeSymbol[] typeArguments)
    {
        if (typeArguments.Length != Arity)
            throw new ArgumentException($"Type '{Name}' expects {Arity} type arguments, but got {typeArguments.Length}.");

        return new ConstructedNamedTypeSymbol(this, typeArguments.ToImmutableArray());
    }
}

