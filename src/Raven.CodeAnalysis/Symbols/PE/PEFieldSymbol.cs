using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PEFieldSymbol : PESymbol, IFieldSymbol
{
    private readonly FieldInfo _fieldInfo;
    private ITypeSymbol? _type;
    private Accessibility? _accessibility;

    public PEFieldSymbol(FieldInfo fieldInfo, INamedTypeSymbol? containingType, Location[] locations)
        : base(containingType, containingType, containingType.ContainingNamespace, locations)
    {
        _fieldInfo = fieldInfo;
    }

    public override SymbolKind Kind => SymbolKind.Field;
    public override string Name => _fieldInfo.Name;

    public virtual ITypeSymbol Type
    {
        get
        {
            if (_fieldInfo.FieldType.IsGenericParameter)
            {
                return _type ??= new PETypeParameterSymbol(_fieldInfo.FieldType, this, ContainingType, ContainingNamespace, []);
            }

            _type ??= PEContainingModule.GetType(_fieldInfo.FieldType);

            var unionAttribute = _fieldInfo.GetCustomAttributesData().FirstOrDefault(x => x.AttributeType.Name == "TypeUnionAttribute");
            if (unionAttribute is not null)
            {
                var types = ((IEnumerable<CustomAttributeTypedArgument>)unionAttribute.ConstructorArguments.First().Value).Select(x => (Type)x.Value);
                _type = new UnionTypeSymbol(types.Select(x => PEContainingModule.GetType(x)!).ToArray(), null, null, null, []);
            }

            return _type;
        }
    }

    public override Accessibility DeclaredAccessibility => _accessibility ??= MapAccessibility(_fieldInfo);

    public override bool IsStatic => _fieldInfo.IsStatic;
    public virtual bool IsLiteral => _fieldInfo.IsLiteral;

    public object? GetConstantValue() => _fieldInfo.GetRawConstantValue();

    public virtual FieldInfo GetFieldInfo() => _fieldInfo;
}