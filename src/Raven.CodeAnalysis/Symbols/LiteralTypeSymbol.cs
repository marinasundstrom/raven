using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class LiteralTypeSymbol : SourceSymbol, ITypeSymbol
{
    public ITypeSymbol UnderlyingType { get; }
    public object ConstantValue { get; }

    private static string GetDisplayName(object? value)
        => value switch
        {
            string s => "\"" + s + "\"",
            char c => "'" + c + "'",
            bool b => b ? "true" : "false",
            _ => value?.ToString() ?? string.Empty
        };

    public LiteralTypeSymbol(ITypeSymbol underlyingType, object constantValue, Compilation compilation)
        : base(SymbolKind.Type, GetDisplayName(constantValue), compilation.Assembly, null, null, [], [])
    {
        UnderlyingType = underlyingType;
        ConstantValue = constantValue;
        TypeKind = underlyingType.TypeKind;
    }

    public override string Name => GetDisplayName(ConstantValue);

    public INamedTypeSymbol? BaseType => UnderlyingType.BaseType;

    public SpecialType SpecialType => SpecialType.None;

    public bool IsNamespace => false;

    public bool IsType => true;

    public TypeKind TypeKind { get; }

    public ITypeSymbol? OriginalDefinition => null;

    public bool IsValueType => UnderlyingType.IsValueType;

    public bool IsReferenceType => UnderlyingType.IsReferenceType;

    public ImmutableArray<INamedTypeSymbol> Interfaces => UnderlyingType.Interfaces;

    public ImmutableArray<INamedTypeSymbol> AllInterfaces => UnderlyingType.AllInterfaces;

    public ImmutableArray<ISymbol> GetMembers() => UnderlyingType.GetMembers();

    public ImmutableArray<ISymbol> GetMembers(string name) => UnderlyingType.GetMembers(name);

    public ITypeSymbol? LookupType(string name) => UnderlyingType.LookupType(name);

    public bool IsMemberDefined(string name, out ISymbol? symbol) => UnderlyingType.IsMemberDefined(name, out symbol);

    public override void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);

    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
}
