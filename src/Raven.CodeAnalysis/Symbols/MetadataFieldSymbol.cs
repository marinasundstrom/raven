using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class MetadataFieldSymbol : MetadataSymbol, IFieldSymbol
{
    private readonly FieldInfo _fieldInfo;
    private ITypeSymbol? _type;

    public MetadataFieldSymbol(FieldInfo fieldInfo, ITypeSymbol fieldType, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(containingSymbol, containingType, containingNamespace, locations)
    {
        _fieldInfo = fieldInfo;
    }

    public override SymbolKind Kind => SymbolKind.Field;
    public override string Name => _fieldInfo.Name;
    public ITypeSymbol Type => _type ??= Compilation.GetType(_fieldInfo.FieldType);
    public override bool IsStatic => _fieldInfo.IsStatic;
    public bool IsLiteral => _fieldInfo.IsLiteral;
    public object? GetConstantValue() => _fieldInfo.GetRawConstantValue();

    public FieldInfo GetFieldInfo() => _fieldInfo;
}