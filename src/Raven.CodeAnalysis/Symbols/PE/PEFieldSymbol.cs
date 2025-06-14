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

            return _type ??= PEContainingModule.GetType(_fieldInfo.FieldType);
        }
    }

    public override Accessibility DeclaredAccessibility => _accessibility ??= MapAccessibility(_fieldInfo);

    public override bool IsStatic => _fieldInfo.IsStatic;
    public virtual bool IsLiteral => _fieldInfo.IsLiteral;
    
    public object? GetConstantValue() => _fieldInfo.GetRawConstantValue();

    public virtual FieldInfo GetFieldInfo() => _fieldInfo;
}