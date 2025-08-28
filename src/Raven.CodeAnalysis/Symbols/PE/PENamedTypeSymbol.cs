using System.Collections.Immutable;
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

    public PENamedTypeSymbol(TypeResolver typeResolver, System.Reflection.TypeInfo typeInfo, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(containingSymbol, containingType, containingNamespace, locations)
    {
        _typeResolver = typeResolver;
        _typeInfo = typeInfo;

        if (typeInfo?.Name == "Object" || typeInfo.BaseType?.Name == "Object")
        {
            TypeKind = TypeKind.Class;
            return;
        }

        if (typeInfo.IsEnum)
            TypeKind = TypeKind.Enum;
        else if (typeof(MulticastDelegate).IsAssignableFrom(typeInfo))
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
    public ImmutableArray<ITypeParameterSymbol> TypeParameters => _typeParameters ??= _typeInfo.GenericTypeParameters.Select(x => (ITypeParameterSymbol)new PETypeParameterSymbol(x, this, this, this.ContainingNamespace, [])).ToImmutableArray();
    public ITypeSymbol? ConstructedFrom { get; }
    public bool IsAbstract => _typeInfo.IsAbstract;
    public bool IsSealed => _typeInfo.IsSealed;
    public bool IsGenericType => _typeInfo.IsGenericType;
    public bool IsUnboundGenericType => _typeInfo.IsGenericTypeDefinition;

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
            if (type == typeof(sbyte))
                return SpecialType.System_SByte;
            if (type == typeof(byte))
                return SpecialType.System_Byte;
            if (type == typeof(short))
                return SpecialType.System_Int16;
            if (type == typeof(ushort))
                return SpecialType.System_UInt16;
            if (type.FullName == "System.Int32")
                return SpecialType.System_Int32;
            if (type == typeof(uint))
                return SpecialType.System_UInt32;
            if (type == typeof(long))
                return SpecialType.System_Int64;
            if (type == typeof(ulong))
                return SpecialType.System_UInt64;
            if (type == typeof(float))
                return SpecialType.System_Single;
            if (type == typeof(double))
                return SpecialType.System_Double;
            if (type == typeof(decimal))
                return SpecialType.System_Decimal;
            if (type.FullName == "System.Void")
                return SpecialType.System_Void;
            if (type == typeof(DateTime))
                return SpecialType.System_DateTime;
            if (type == typeof(IntPtr))
                return SpecialType.System_IntPtr;
            if (type == typeof(UIntPtr))
                return SpecialType.System_UIntPtr;

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

    public ITypeSymbol? OriginalDefinition { get; }

    public int Arity => _typeInfo.GenericTypeParameters.Length;

    public INamedTypeSymbol UnderlyingTupleType => throw new NotImplementedException();

    public ImmutableArray<IFieldSymbol> TupleElements => throw new NotImplementedException();

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
        return null;
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
