using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

/// <summary>A result projection retained until its function operand is substituted.</summary>
internal sealed class FunctionReturnTypeSymbol : Symbol, IFunctionReturnTypeSymbol
{
    private readonly ITypeSymbol unitType;

    internal FunctionReturnTypeSymbol(ITypeSymbol functionType, ITypeSymbol unitType)
        : base(SymbolKind.Type, "ReturnType", null, null, null, [], [], Accessibility.Public, addAsMember: false)
    {
        FunctionType = functionType;
        this.unitType = unitType;
    }

    public ITypeSymbol FunctionType { get; }
    internal ITypeSymbol Substitute(Func<ITypeSymbol, ITypeSymbol> substitute)
    {
        var operand = substitute(FunctionType);
        return operand is StructuralFunctionTypeSymbol function
            ? function.ReturnType.SpecialType == SpecialType.System_Void ? unitType : function.ReturnType :
            ReferenceEquals(operand, FunctionType) ? this : new FunctionReturnTypeSymbol(operand, unitType);
    }
    public bool IsNamespace => false;
    public bool IsType => true;
    public bool IsValueType => false;
    public bool IsReferenceType => false;
    public INamedTypeSymbol? BaseType => null;
    public ITypeSymbol OriginalDefinition => this;
    public SpecialType SpecialType => SpecialType.None;
    public TypeKind TypeKind => TypeKind.FunctionReturn;
    public ImmutableArray<INamedTypeSymbol> Interfaces => [];
    public ImmutableArray<INamedTypeSymbol> AllInterfaces => [];
    public ImmutableArray<ISymbol> GetMembers() => [];
    public ImmutableArray<ISymbol> GetMembers(string name) => [];
    public ITypeSymbol? LookupType(string name) => null;
    public bool IsMemberDefined(string name, out ISymbol? symbol) { symbol = null; return false; }
    public override void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
}
