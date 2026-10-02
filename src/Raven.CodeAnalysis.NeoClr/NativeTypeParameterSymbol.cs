using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NativeTypeParameterSymbol : Symbol, ITypeParameterSymbol
{
    internal NativeTypeParameterSymbol(string name, int ordinal, ISymbol owner)
        : base(SymbolKind.TypeParameter, name, owner, owner as INamedTypeSymbol ?? owner.ContainingType, owner.ContainingNamespace, [], []) => Ordinal = ordinal;

    public int Ordinal { get; }
    public TypeParameterOwnerKind OwnerKind => ContainingSymbol is IMethodSymbol ? TypeParameterOwnerKind.Method : TypeParameterOwnerKind.Type;
    public INamedTypeSymbol? DeclaringTypeParameterOwner => ContainingSymbol as INamedTypeSymbol;
    public IMethodSymbol? DeclaringMethodParameterOwner => ContainingSymbol as IMethodSymbol;
    public TypeParameterConstraintKind ConstraintKind => TypeParameterConstraintKind.None;
    public ImmutableArray<ITypeSymbol> ConstraintTypes => [];
    public VarianceKind Variance => VarianceKind.None;
    public SpecialType SpecialType => SpecialType.None;
    public TypeKind TypeKind => TypeKind.TypeParameter;
    public bool IsNamespace => false;
    public bool IsType => true;
    public bool IsReferenceType => false;
    public bool IsValueType => false;
    public INamedTypeSymbol? BaseType => null;
    public ITypeSymbol? OriginalDefinition => this;
    public ImmutableArray<INamedTypeSymbol> Interfaces => [];
    public ImmutableArray<INamedTypeSymbol> AllInterfaces => [];
    public ImmutableArray<ISymbol> GetMembers() => [];
    public ImmutableArray<ISymbol> GetMembers(string name) => [];
    public ITypeSymbol? LookupType(string name) => null;
    public bool IsMemberDefined(string name, out ISymbol? symbol) { symbol = null; return false; }
    public override IModuleSymbol ContainingModule => ContainingSymbol!.ContainingModule!;
    public override IAssemblySymbol ContainingAssembly => ContainingSymbol!.ContainingAssembly!;
    public override void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
}
