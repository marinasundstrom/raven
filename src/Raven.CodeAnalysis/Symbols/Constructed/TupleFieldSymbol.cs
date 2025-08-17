using System.Reflection;

using Raven.CodeAnalysis.CodeGen;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class TupleFieldSymbol : PEFieldSymbol, IFieldSymbol
{
    private readonly SubstitutedFieldSymbol _underlyingField;

    public TupleFieldSymbol(string name, SubstitutedFieldSymbol underlyingField, INamedTypeSymbol? containingType,
        Location[] locations)
        : base(null, null!, containingType, locations)
    {
        Name = name;
        _underlyingField = underlyingField;
    }

    public override string Name { get; }

    public override bool IsStatic => _underlyingField.IsStatic;
    public override bool IsLiteral => _underlyingField.IsLiteral;

    public override ITypeSymbol Type => _underlyingField.Type;

    public IFieldSymbol UnderlyingField => _underlyingField;

    /*
    public override FieldInfo GetFieldInfo()
    {
        return ((SubstitutedFieldSymbol)_underlyingField).GetFieldInfo();
    }
    */
}

public static class FieldSymbolExtensions
{

    internal static FieldInfo GetFieldInfo(this IFieldSymbol fieldSymbol, CodeGenerator codeGen)
    {
        return fieldSymbol switch
        {
            TupleFieldSymbol tupleFieldSymbol => tupleFieldSymbol.UnderlyingField.GetFieldInfo(codeGen),
            SourceFieldSymbol sourceField => (FieldInfo)codeGen.GetMemberBuilder(sourceField),
            PEFieldSymbol field => field.GetFieldInfo(),
            SubstitutedFieldSymbol field => field.GetFieldInfo(codeGen),
            _ => throw new NotSupportedException($"Unsupported field type: {fieldSymbol.GetType().Name}")
        };
    }
}