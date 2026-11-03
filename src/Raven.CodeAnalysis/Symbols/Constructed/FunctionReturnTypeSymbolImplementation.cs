using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

/// <summary>A result projection retained until its function operand is substituted.</summary>
internal sealed class FunctionReturnTypeSymbolImplementation : SymbolBase, FunctionReturnTypeSymbol
{
    private readonly TypeSymbol unitType;

    internal FunctionReturnTypeSymbolImplementation(TypeSymbol functionType, TypeSymbol unitType)
        : base(SymbolKind.Type, "ReturnType", null, null, null, [], [], Accessibility.Public, addAsMember: false)
    {
        FunctionType = functionType;
        this.unitType = unitType;
    }

    public TypeSymbol FunctionType { get; }
    internal TypeSymbol Substitute(Func<TypeSymbol, TypeSymbol> substitute)
    {
        var operand = substitute(FunctionType);
        return operand is StructuralFunctionTypeSymbol function
            ? function.ReturnType.SpecialType == SpecialType.System_Void ? unitType : function.ReturnType :
            ReferenceEquals(operand, FunctionType) ? this : new FunctionReturnTypeSymbolImplementation(operand, unitType);
    }
    public bool IsNamespace => false;
    public bool IsType => true;
    public bool IsValueType => false;
    public bool IsReferenceType => false;
    public NamedTypeSymbol? BaseType => null;
    public TypeSymbol OriginalDefinition => this;
    public SpecialType SpecialType => SpecialType.None;
    public TypeKind TypeKind => TypeKind.FunctionReturn;
    public ImmutableArray<NamedTypeSymbol> Interfaces => [];
    public ImmutableArray<NamedTypeSymbol> AllInterfaces => [];
    public ImmutableArray<Symbol> GetMembers() => [];
    public ImmutableArray<Symbol> GetMembers(string name) => [];
    public TypeSymbol? LookupType(string name) => null;
    public bool IsMemberDefined(string name, out Symbol? symbol) { symbol = null; return false; }
    public override void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
}
