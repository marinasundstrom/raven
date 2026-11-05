using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class LiteralTypeSymbol : SourceSymbol, TypeSymbol, ILiteralTypeInfo
{
    public TypeSymbol UnderlyingType { get; }
    public object ConstantValue { get; }

    private static string GetDisplayName(object? value)
        => value switch
        {
            string s => "\"" + s + "\"",
            char c => "'" + c + "'",
            bool b => b ? "true" : "false",
            _ => value?.ToString() ?? string.Empty
        };

    public LiteralTypeSymbol(TypeSymbol underlyingType, object constantValue, Compilation compilation)
        : base(SymbolKind.Type, GetDisplayName(constantValue), compilation.Assembly, null, null, [], [])
    {
        UnderlyingType = underlyingType;
        ConstantValue = constantValue;
        TypeKind = underlyingType.TypeKind;
    }

    public override string Name => GetDisplayName(ConstantValue);

    public NamedTypeSymbol? BaseType => UnderlyingType.BaseType;

    public SpecialType SpecialType => SpecialType.None;

    public bool IsNamespace => false;

    public bool IsType => true;

    public TypeKind TypeKind { get; }

    public TypeSymbol? OriginalDefinition => null;

    public bool IsValueType => UnderlyingType.IsValueType;

    public bool IsReferenceType => UnderlyingType.IsReferenceType;

    public ImmutableArray<NamedTypeSymbol> Interfaces => UnderlyingType.Interfaces;

    public ImmutableArray<NamedTypeSymbol> AllInterfaces => UnderlyingType.AllInterfaces;

    public ImmutableArray<Symbol> GetMembers() => UnderlyingType.GetMembers();

    public ImmutableArray<Symbol> GetMembers(string name) => UnderlyingType.GetMembers(name);

    public TypeSymbol? LookupType(string name) => UnderlyingType.LookupType(name);

    public bool IsMemberDefined(string name, out Symbol? symbol) => UnderlyingType.IsMemberDefined(name, out symbol);

    public override void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);

    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
}
