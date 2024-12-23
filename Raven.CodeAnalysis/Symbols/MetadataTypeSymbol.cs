using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal class MetadataTypeSymbol : MetadataSymbol, ITypeSymbol, INamedTypeSymbol
{
    private readonly TypeInfo _typeInfo;
    private List<ISymbol> _members = new List<ISymbol>();

    public MetadataTypeSymbol(Compilation compilation, TypeInfo typeInfo, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(compilation, containingSymbol, containingType, containingNamespace, locations)
    {
        _typeInfo = typeInfo;
    }

    public override SymbolKind Kind => SymbolKind.Type;
    public override string Name => _typeInfo.Name;

    public bool IsNamespace { get; } = false;
    public bool IsType { get; } = true;

    public ImmutableArray<IMethodSymbol> Constructors { get; }
    public IMethodSymbol? StaticConstructor { get; }
    public ImmutableArray<ITypeSymbol> TypeArguments { get; }
    public ImmutableArray<ITypeParameterSymbol> TypeParameters { get; }

    public SpecialType SpecialType
    {
        get
        {
            var type = _typeInfo.AsType();

            if (type == typeof(object))
                return SpecialType.System_Object;
            if (type == typeof(string))
                return SpecialType.System_String;
            if (type == typeof(bool))
                return SpecialType.System_Boolean;
            if (type == typeof(char))
                return SpecialType.System_Char;
            if (type == typeof(sbyte))
                return SpecialType.System_SByte;
            if (type == typeof(byte))
                return SpecialType.System_Byte;
            if (type == typeof(short))
                return SpecialType.System_Int16;
            if (type == typeof(ushort))
                return SpecialType.System_UInt16;
            if (type == typeof(int))
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
            if (type == typeof(void))
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

    public ImmutableArray<ISymbol> GetMembers()
    {
        return _members.ToImmutableArray();
    }

    public ImmutableArray<ISymbol> GetMembers(string name)
    {
        return _members.Where(x => x.Name == name).ToImmutableArray();
    }

    internal void AddMember(ISymbol member)
    {
        _members.Add(member);
    }
}