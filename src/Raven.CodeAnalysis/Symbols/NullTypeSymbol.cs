using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class NullTypeSymbol : SourceSymbol, TypeSymbol
{
    private readonly Compilation _compilation;

    public NullTypeSymbol(Compilation compilation)
        : base(SymbolKind.Type, "null", compilation.Assembly, null, compilation.Assembly.GlobalNamespace, [], [])
    {
        _compilation = compilation;
        TypeKind = TypeKind.Null;
    }

    public override string Name => "null";

    public NamedTypeSymbol? BaseType => _compilation.GetSpecialType(SpecialType.System_Object);

    public SpecialType SpecialType => SpecialType.None;

    public bool IsNamespace => false;

    public bool IsType => true;

    public TypeKind TypeKind { get; }

    public TypeSymbol? OriginalDefinition => null;

    public ImmutableArray<NamedTypeSymbol> Interfaces => BaseType?.Interfaces ?? ImmutableArray<NamedTypeSymbol>.Empty;

    public ImmutableArray<NamedTypeSymbol> AllInterfaces => BaseType?.AllInterfaces ?? ImmutableArray<NamedTypeSymbol>.Empty;

    public ImmutableArray<Symbol> GetMembers() => [];

    public ImmutableArray<Symbol> GetMembers(string name) => [];

    public TypeSymbol? LookupType(string name) => null;

    public bool IsMemberDefined(string name, out Symbol? symbol)
    {
        symbol = null;
        return false;
    }

    public override void Accept(SymbolVisitor visitor)
    {
        visitor.DefaultVisit(this);
    }

    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor)
    {
        return visitor.DefaultVisit(this);
    }
}
