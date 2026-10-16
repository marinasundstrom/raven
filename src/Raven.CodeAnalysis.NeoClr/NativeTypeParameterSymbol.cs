using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NativeTypeParameterSymbol : Symbol, ITypeParameterSymbol
{
    private readonly Lazy<ImmutableArray<ITypeSymbol>> constraintTypes;

    internal NativeTypeParameterSymbol(string name, int ordinal, ISymbol owner, Func<ImmutableArray<ITypeSymbol>>? getConstraints = null, VarianceKind variance = VarianceKind.None, TypeParameterConstraintKind specialConstraints = TypeParameterConstraintKind.None)
        : base(SymbolKind.TypeParameter, name, owner, owner as INamedTypeSymbol ?? owner.ContainingType, owner.ContainingNamespace, [], [])
    {
        Ordinal = ordinal;
        Variance = variance;
        this.specialConstraints = specialConstraints;
        constraintTypes = new(getConstraints ?? (() => []));
    }

    private readonly TypeParameterConstraintKind specialConstraints;
    public int Ordinal { get; }
    public TypeParameterOwnerKind OwnerKind => ContainingSymbol is IMethodSymbol ? TypeParameterOwnerKind.Method : TypeParameterOwnerKind.Type;
    public INamedTypeSymbol? DeclaringTypeParameterOwner => ContainingSymbol as INamedTypeSymbol;
    public IMethodSymbol? DeclaringMethodParameterOwner => ContainingSymbol as IMethodSymbol;
    public TypeParameterConstraintKind ConstraintKind => specialConstraints | (ConstraintTypes.IsEmpty ? TypeParameterConstraintKind.None : TypeParameterConstraintKind.TypeConstraint);
    public ImmutableArray<ITypeSymbol> ConstraintTypes => constraintTypes.Value;
    public VarianceKind Variance { get; }
    public SpecialType SpecialType => SpecialType.None;
    public TypeKind TypeKind => TypeKind.TypeParameter;
    public bool IsNamespace => false;
    public bool IsType => true;
    public bool IsReferenceType => specialConstraints.HasFlag(TypeParameterConstraintKind.ReferenceType) || BaseType is { SpecialType: not SpecialType.System_Object };
    public bool IsValueType => specialConstraints.HasFlag(TypeParameterConstraintKind.ValueType);
    public INamedTypeSymbol? BaseType => ConstraintTypes.OfType<INamedTypeSymbol>().FirstOrDefault(t => t.TypeKind == TypeKind.Class);
    public ITypeSymbol? OriginalDefinition => this;
    public ImmutableArray<INamedTypeSymbol> Interfaces => [.. ConstraintTypes.OfType<INamedTypeSymbol>().Where(t => t.TypeKind == TypeKind.Interface)];
    public ImmutableArray<INamedTypeSymbol> AllInterfaces
    {
        get
        {
            var result = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
            var seen = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);
            foreach (var bound in Interfaces)
            {
                if (seen.Add(bound)) result.Add(bound);
                foreach (var inherited in bound.AllInterfaces)
                    if (seen.Add(inherited)) result.Add(inherited);
            }
            return result.ToImmutable();
        }
    }
    public ImmutableArray<ISymbol> GetMembers() => [];
    public ImmutableArray<ISymbol> GetMembers(string name) => [];
    public ITypeSymbol? LookupType(string name) => null;
    public bool IsMemberDefined(string name, out ISymbol? symbol) { symbol = null; return false; }
    public override IModuleSymbol ContainingModule => ContainingSymbol!.ContainingModule!;
    public override IAssemblySymbol ContainingAssembly => ContainingSymbol!.ContainingAssembly!;
    public override void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
}
