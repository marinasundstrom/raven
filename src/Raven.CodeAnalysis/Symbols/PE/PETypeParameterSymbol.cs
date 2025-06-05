using System;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Symbols;

internal sealed partial class PETypeParameterSymbol : Symbol, ITypeParameterSymbol
{
    private readonly Type _type;

    public PETypeParameterSymbol(
        Type type,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations)
        : base(containingSymbol, containingType, containingNamespace, locations, [])
    {
        if (!type.IsGenericParameter)
            throw new ArgumentException("Type must be a generic parameter", nameof(type));

        _type = type;
    }

    public override string Name => _type.Name;

    public override SymbolKind Kind => SymbolKind.TypeParameter;
    public override IAssemblySymbol ContainingAssembly => ContainingNamespace?.ContainingAssembly!;
    public override IModuleSymbol ContainingModule => ContainingNamespace?.ContainingModule!;
    protected PEAssemblySymbol PEContainingAssembly => (PEAssemblySymbol)ContainingAssembly;
    protected PEModuleSymbol PEContainingModule => (PEModuleSymbol)ContainingModule;

    public int Ordinal => _type.GenericParameterPosition;

    //public bool HasConstructorConstraint => (_type.GenericParameterAttributes & GenericParameterAttributes.DefaultConstructorConstraint) != 0;
    //public bool HasReferenceTypeConstraint => (_type.GenericParameterAttributes & GenericParameterAttributes.ReferenceTypeConstraint) != 0;
    //public bool HasValueTypeConstraint => (_type.GenericParameterAttributes & GenericParameterAttributes.NotNullableValueTypeConstraint) != 0;

    /*
    public VarianceKind Variance =>
        (_type.GenericParameterAttributes & GenericParameterAttributes.VarianceMask) switch
        {
            GenericParameterAttributes.Covariant => VarianceKind.Out,
            GenericParameterAttributes.Contravariant => VarianceKind.In,
            _ => VarianceKind.None
        };*/

    /*
public ImmutableArray<ITypeSymbol> ConstraintTypes =>
    _type.GetGenericParameterConstraints()
        .Select(t => new PENamedTypeSymbol(t, ContainingSymbol)) // assumes PENamedTypeSymbol exists
        .Cast<ITypeSymbol>()
        .ToImmutableArray();
*/

    public INamedTypeSymbol? BaseType => null; // Not applicable to type parameters
    public ITypeSymbol? OriginalDefinition => this;
    public SpecialType SpecialType => SpecialType.None;
    public TypeKind TypeKind => TypeKind.TypeParameter;

    public bool IsNamespace => false;
    public bool IsType => true;

    public ImmutableArray<ISymbol> GetMembers() => ImmutableArray<ISymbol>.Empty;
    public ImmutableArray<ISymbol> GetMembers(string name) => ImmutableArray<ISymbol>.Empty;
    public ITypeSymbol? LookupType(string name) => null;
    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        symbol = null;
        return false;
    }
}