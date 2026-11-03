using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NativeTypeParameterSymbol : SymbolBase, TypeParameterSymbol
{
    private readonly Compilation compilation;
    private readonly Lazy<ImmutableArray<TypeSymbol>> constraintTypes;
    private readonly Lazy<ImmutableArray<AttributeData>> attributes;
    public override ImmutableArray<AttributeData> GetAttributes() => attributes.Value;

    internal NativeTypeParameterSymbol(Compilation compilation, string name, int ordinal, Symbol owner, Func<ImmutableArray<TypeSymbol>>? getConstraints = null, VarianceKind variance = VarianceKind.None, TypeParameterConstraintKind specialConstraints = TypeParameterConstraintKind.None, Func<ImmutableArray<AttributeData>>? getAttributes = null)
        : base(SymbolKind.TypeParameter, name, owner, owner as NamedTypeSymbol ?? owner.ContainingType, owner.ContainingNamespace, [], [])
    {
        this.compilation = compilation;
        Ordinal = ordinal;
        Variance = variance;
        this.specialConstraints = specialConstraints;
        constraintTypes = new(getConstraints ?? (() => []));
        attributes = new(getAttributes ?? (() => []));
    }

    private readonly TypeParameterConstraintKind specialConstraints;
    public int Ordinal { get; }
    public TypeParameterOwnerKind OwnerKind => ContainingSymbol is MethodSymbol ? TypeParameterOwnerKind.Method : TypeParameterOwnerKind.Type;
    public NamedTypeSymbol? DeclaringTypeParameterOwner => ContainingSymbol as NamedTypeSymbol;
    public MethodSymbol? DeclaringMethodParameterOwner => ContainingSymbol as MethodSymbol;
    public TypeParameterConstraintKind ConstraintKind => specialConstraints | (ConstraintTypes.IsEmpty ? TypeParameterConstraintKind.None : TypeParameterConstraintKind.TypeConstraint);
    public ImmutableArray<TypeSymbol> ConstraintTypes => constraintTypes.Value;
    public VarianceKind Variance { get; }
    public SpecialType SpecialType => SpecialType.None;
    public TypeKind TypeKind => TypeKind.TypeParameter;
    public bool IsNamespace => false;
    public bool IsType => true;
    public bool IsReferenceType => specialConstraints.HasFlag(TypeParameterConstraintKind.ReferenceType) || BaseType is { SpecialType: not SpecialType.System_Object };
    public bool IsValueType => specialConstraints.HasFlag(TypeParameterConstraintKind.ValueType);
    public NamedTypeSymbol? BaseType => ConstraintTypes.OfType<NamedTypeSymbol>().FirstOrDefault(t => t.TypeKind == TypeKind.Class);
    public TypeSymbol? OriginalDefinition => this;
    public ImmutableArray<NamedTypeSymbol> Interfaces => [.. ConstraintTypes.OfType<NamedTypeSymbol>().Where(t => t.TypeKind == TypeKind.Interface)];
    public ImmutableArray<NamedTypeSymbol> AllInterfaces
    {
        get
        {
            var result = ImmutableArray.CreateBuilder<NamedTypeSymbol>();
            var seen = new HashSet<NamedTypeSymbol>(SymbolEqualityComparer.Default);
            foreach (var bound in Interfaces)
            {
                if (seen.Add(bound)) result.Add(bound);
                foreach (var inherited in bound.AllInterfaces)
                    if (seen.Add(inherited)) result.Add(inherited);
            }
            return result.ToImmutable();
        }
    }
    public ImmutableArray<Symbol> GetMembers() => compilation.GetFunctionContractMembers(this);
    public ImmutableArray<Symbol> GetMembers(string name) => [.. GetMembers().Where(member => member.Name == name)];
    public TypeSymbol? LookupType(string name) => null;
    public bool IsMemberDefined(string name, out Symbol? symbol) { symbol = GetMembers(name).FirstOrDefault(); return symbol is not null; }
    public override ModuleSymbol ContainingModule => ContainingSymbol!.ContainingModule!;
    public override AssemblySymbol ContainingAssembly => ContainingSymbol!.ContainingAssembly!;
    public override void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
}
