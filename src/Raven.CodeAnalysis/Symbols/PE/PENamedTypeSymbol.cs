using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PENamedTypeSymbol : PESymbol, INamedTypeSymbol
{
    private readonly System.Reflection.TypeInfo _typeInfo;
    private readonly List<ISymbol> _members = new List<ISymbol>();
    private INamedTypeSymbol? _baseType;
    private bool _membersLoaded;

    public PENamedTypeSymbol(System.Reflection.TypeInfo typeInfo, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(containingSymbol, containingType, containingNamespace, locations)
    {
        _typeInfo = typeInfo;
    }

    public override SymbolKind Kind => SymbolKind.Type;
    public override string Name => _typeInfo.Name;

    public bool IsNamespace { get; } = false;
    public bool IsType { get; } = true;

    public ImmutableArray<IMethodSymbol> Constructors => GetMembers(".ctor").OfType<IMethodSymbol>().ToImmutableArray();
    public IMethodSymbol? StaticConstructor { get; }
    public ImmutableArray<ITypeSymbol> TypeArguments { get; }
    public ImmutableArray<ITypeParameterSymbol> TypeParameters { get; }

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

            if (type.IsArray)
                return SpecialType.System_Array;

            return SpecialType.None;
        }
    }

    public bool IsValueType => _typeInfo.IsValueType;

    public INamedTypeSymbol? BaseType => _baseType ??= (_typeInfo.BaseType is not null ? (INamedTypeSymbol?)PEContainingModule.GetType(_typeInfo.BaseType) : null);

    public bool IsArray => false;

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
        throw new NotImplementedException();
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

        foreach (var mi in _typeInfo.DeclaredMethods)
        {
            if (mi.IsSpecialName)
                continue;

            var method = new PEMethodSymbol(
                mi,
                this,
                [new MetadataLocation(ContainingModule!)]);
        }

        foreach (var pi in _typeInfo.DeclaredProperties)
        {
            var property = new PEPropertySymbol(
                pi,
                this,
                [new MetadataLocation(ContainingModule!)]);

            if (pi.GetMethod is not null)
            {
                property.GetMethod = new PEMethodSymbol(
                    pi.GetMethod,
                    property,
                    this,
                    [new MetadataLocation(ContainingModule!)]);
            }

            if (pi.SetMethod is not null)
            {
                property.SetMethod = new PEMethodSymbol(
                    pi.SetMethod,
                    property,
                    this,
                    [new MetadataLocation(ContainingModule!)]);
            }
        }

        foreach (var fi in _typeInfo.DeclaredFields)
        {
            if (fi.IsSpecialName)
                continue;

            var field = new PEFieldSymbol(
                fi,
                this,
                [new MetadataLocation(ContainingModule!)]);
        }

        foreach (var ci in _typeInfo.DeclaredConstructors)
        {
            var ctor = new PEMethodSymbol(
                ci,
                this,
                [new MetadataLocation(ContainingModule!)]);
        }
    }
}
