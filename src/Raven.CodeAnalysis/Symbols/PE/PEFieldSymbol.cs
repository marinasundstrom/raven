using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PEFieldSymbol : PESymbol, IFieldSymbol
{
    private readonly TypeResolver _typeResolver;
    private readonly FieldInfo _fieldInfo;
    private ITypeSymbol? _type;
    private Accessibility? _accessibility;
    private FieldInfo? _runtimeFieldInfo;

    public PEFieldSymbol(TypeResolver typeResolver, FieldInfo fieldInfo, INamedTypeSymbol? containingType, Location[] locations)
        : base(containingType, containingType, containingType.ContainingNamespace, locations)
    {
        _typeResolver = typeResolver;
        _fieldInfo = fieldInfo;
    }

    public override SymbolKind Kind => SymbolKind.Field;
    public override string Name => _fieldInfo.Name;

    public virtual ITypeSymbol Type
    {
        get
        {
            return _type ??= _typeResolver.ResolveType(_fieldInfo);
        }
    }

    public override Accessibility DeclaredAccessibility => _accessibility ??= MapAccessibility(_fieldInfo);

    public override bool IsStatic => _fieldInfo.IsStatic;
    public virtual bool IsConst => _fieldInfo.IsLiteral;

    public virtual bool IsMutable => !_fieldInfo.IsInitOnly && !_fieldInfo.IsLiteral;

    public object? GetConstantValue() => _fieldInfo.GetRawConstantValue();

    public virtual FieldInfo GetFieldInfo()
    {
        if (_runtimeFieldInfo is not null)
            return _runtimeFieldInfo;

        var runtimeField = _typeResolver.ResolveRuntimeField(_fieldInfo);
        if (runtimeField is not null)
        {
            _runtimeFieldInfo = runtimeField;
            return runtimeField;
        }

        return _fieldInfo;
    }
}