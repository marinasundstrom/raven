using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class UnitTypeSymbol : SourceSymbol, ITypeSymbol
{
    private readonly Compilation _compilation;

    public UnitTypeSymbol(Compilation compilation)
        : base(SymbolKind.Type, "unit", compilation.Assembly, null, compilation.Assembly.GlobalNamespace, [], [])
    {
        _compilation = compilation;
        TypeKind = TypeKind.Struct;
    }

    public override string Name => "unit";

    public INamedTypeSymbol? BaseType => _compilation.GetSpecialType(SpecialType.System_ValueType);

    public SpecialType SpecialType => SpecialType.None;

    public bool IsNamespace => false;

    public bool IsType => true;

    public TypeKind TypeKind { get; }

    public ITypeSymbol? OriginalDefinition => null;

    public ImmutableArray<ISymbol> GetMembers() => [];

    public ImmutableArray<ISymbol> GetMembers(string name) => [];

    public ITypeSymbol? LookupType(string name) => null;

    public bool IsMemberDefined(string name, out ISymbol? symbol)
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
