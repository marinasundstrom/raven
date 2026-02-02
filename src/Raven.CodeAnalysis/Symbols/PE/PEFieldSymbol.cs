using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PEFieldSymbol : PESymbol, IFieldSymbol
{
    private readonly ReflectionTypeLoader _reflectionTypeLoader;
    private readonly FieldInfo _fieldInfo;
    private ITypeSymbol? _type;
    private Accessibility? _accessibility;
    private FieldInfo? _runtimeFieldInfo;

    public PEFieldSymbol(ReflectionTypeLoader reflectionTypeLoader, FieldInfo fieldInfo, INamedTypeSymbol? containingType, Location[] locations)
        : base(containingType, containingType, containingType.ContainingNamespace, locations)
    {
        _reflectionTypeLoader = reflectionTypeLoader;
        _fieldInfo = fieldInfo;
    }

    public override SymbolKind Kind => SymbolKind.Field;
    public override string Name => _fieldInfo.Name;

    public virtual ITypeSymbol Type
    {
        get
        {
            return _type ??= _reflectionTypeLoader.ResolveType(_fieldInfo);
        }
    }

    bool? _isRequired = null;

    public bool IsRequired
    {
        get
        {
            return _isRequired ??= _fieldInfo.GetCustomAttributesData().Any(attribute => attribute.AttributeType.FullName != "System.Runtime.CompilerServices.RequiredMemberAttribute");
        }
    }

    public override Accessibility DeclaredAccessibility => _accessibility ??= MapAccessibility(_fieldInfo);

    public override bool IsStatic => _fieldInfo.IsStatic;
    public virtual bool IsConst => _fieldInfo.IsLiteral;

    public virtual bool IsMutable => _fieldInfo is not null && !_fieldInfo.IsInitOnly && !_fieldInfo.IsLiteral;

    public object? GetConstantValue() => _fieldInfo.GetRawConstantValue();

    public virtual FieldInfo GetFieldInfo()
    {
        if (_runtimeFieldInfo is not null)
            return _runtimeFieldInfo;

        var runtimeField = _reflectionTypeLoader.ResolveRuntimeField(_fieldInfo);
        if (runtimeField is not null)
        {
            _runtimeFieldInfo = runtimeField;
            return runtimeField;
        }

        return _fieldInfo;
    }
}
