using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PETypeParameterSymbol : Symbol, ITypeParameterSymbol
{
    private readonly Type _runtimeType;
    private readonly MethodBase? _declaringMethod;
    private readonly Type[] _constraintRuntimeTypes;
    private readonly string _name;
    private readonly int _ordinal;
    private readonly TypeParameterOwnerKind _ownerKind;
    private readonly VarianceKind _variance;
    private readonly GenericParameterAttributes _genericParameterAttributes;
    private readonly PETypeParameterIdentity _metadataIdentity;

    private readonly ReflectionTypeLoader _reflectionTypeLoader;
    private TypeParameterConstraintKind? _lazyConstraintKind;
    private ImmutableArray<ITypeSymbol>? _lazyConstraintTypes;

    public PETypeParameterSymbol(
        Type type,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        ReflectionTypeLoader reflectionTypeLoader)
        : base(containingSymbol, containingType, containingNamespace, locations, [], addAsMember: false)
    {
        if (!type.IsGenericParameter)
            throw new ArgumentException("Type must be a generic parameter", nameof(type));

        _runtimeType = type;
        _declaringMethod = type.DeclaringMethod;
        _constraintRuntimeTypes = type.GetGenericParameterConstraints();
        _name = type.Name;
        _ordinal = type.GenericParameterPosition;
        _ownerKind = _declaringMethod is null ? TypeParameterOwnerKind.Type : TypeParameterOwnerKind.Method;
        _genericParameterAttributes = type.GenericParameterAttributes;
        _variance = (_genericParameterAttributes & GenericParameterAttributes.VarianceMask) switch
        {
            GenericParameterAttributes.Covariant => VarianceKind.Out,
            GenericParameterAttributes.Contravariant => VarianceKind.In,
            _ => VarianceKind.None
        };
        _metadataIdentity = new PETypeParameterIdentity(
            _ownerKind,
            _ordinal,
            GetOwnerIdentity(containingSymbol),
            _name);
        _reflectionTypeLoader = reflectionTypeLoader;
    }

    public override string Name => _name;

    public override SymbolKind Kind => SymbolKind.TypeParameter;
    public override IAssemblySymbol ContainingAssembly => ContainingNamespace?.ContainingAssembly!;
    public override IModuleSymbol ContainingModule => ContainingNamespace?.ContainingModule!;
    protected PEAssemblySymbol PEContainingAssembly => (PEAssemblySymbol)ContainingAssembly;
    protected PEModuleSymbol PEContainingModule => (PEModuleSymbol)ContainingModule;

    public int Ordinal => _ordinal;

    public TypeParameterOwnerKind OwnerKind => _ownerKind;
    internal PETypeParameterIdentity MetadataIdentity => _metadataIdentity;

    public INamedTypeSymbol? DeclaringTypeParameterOwner =>
        OwnerKind == TypeParameterOwnerKind.Type ? (ContainingSymbol as INamedTypeSymbol) : null;

    public IMethodSymbol? DeclaringMethodParameterOwner =>
        OwnerKind == TypeParameterOwnerKind.Method ? (ContainingSymbol as IMethodSymbol) : null;

    public TypeParameterConstraintKind ConstraintKind
    {
        get
        {
            if (_lazyConstraintKind is not null)
                return _lazyConstraintKind.Value;

            var attributes = _genericParameterAttributes;
            var kind = TypeParameterConstraintKind.None;

            if ((attributes & GenericParameterAttributes.ReferenceTypeConstraint) != 0)
                kind |= TypeParameterConstraintKind.ReferenceType;

            if ((attributes & GenericParameterAttributes.DefaultConstructorConstraint) != 0)
                kind |= TypeParameterConstraintKind.Constructor;

            if ((attributes & GenericParameterAttributes.NotNullableValueTypeConstraint) != 0)
                kind |= TypeParameterConstraintKind.ValueType;

            if (_constraintRuntimeTypes.Length > 0)
                kind |= TypeParameterConstraintKind.TypeConstraint;

            _lazyConstraintKind = kind;
            return kind;
        }
    }

    public bool IsReferenceType => (ConstraintKind & TypeParameterConstraintKind.ReferenceType) != 0;

    public bool IsValueType => (ConstraintKind & TypeParameterConstraintKind.ValueType) != 0;

    //public bool HasConstructorConstraint => (_type.GenericParameterAttributes & GenericParameterAttributes.DefaultConstructorConstraint) != 0;
    //public bool HasReferenceTypeConstraint => (_type.GenericParameterAttributes & GenericParameterAttributes.ReferenceTypeConstraint) != 0;
    //public bool HasValueTypeConstraint => (_type.GenericParameterAttributes & GenericParameterAttributes.NotNullableValueTypeConstraint) != 0;

    public VarianceKind Variance => _variance;

    /*
public ImmutableArray<ITypeSymbol> ConstraintTypes =>
    _type.GetGenericParameterConstraints()
        .Select(t => new PENamedTypeSymbol(t, ContainingSymbol)) // assumes PENamedTypeSymbol exists
        .Cast<ITypeSymbol>()
        .ToImmutableArray();
*/

    public ImmutableArray<ITypeSymbol> ConstraintTypes
    {
        get
        {
            if (_lazyConstraintTypes.HasValue)
                return _lazyConstraintTypes.Value;

            var constraints = _constraintRuntimeTypes;
            if (constraints.Length == 0)
            {
                _lazyConstraintTypes = ImmutableArray<ITypeSymbol>.Empty;
                return _lazyConstraintTypes.Value;
            }

            var builder = ImmutableArray.CreateBuilder<ITypeSymbol>(constraints.Length);
            foreach (var constraint in constraints)
            {
                var resolved = _reflectionTypeLoader.ResolveType(constraint, _declaringMethod);
                if (resolved is not null)
                    builder.Add(resolved);
            }

            _lazyConstraintTypes = builder.ToImmutable();
            return _lazyConstraintTypes.Value;
        }
    }

    public INamedTypeSymbol? BaseType => null; // Not applicable to type parameters
    public ITypeSymbol? OriginalDefinition => this;
    public SpecialType SpecialType => SpecialType.None;
    public TypeKind TypeKind => TypeKind.TypeParameter;

    public bool IsNamespace => false;
    public bool IsType => true;

    public ImmutableArray<INamedTypeSymbol> Interfaces => GetConstraintInterfaces(includeInherited: false);

    public ImmutableArray<INamedTypeSymbol> AllInterfaces => GetConstraintInterfaces(includeInherited: true);

    public ImmutableArray<ISymbol> GetMembers() => ImmutableArray<ISymbol>.Empty;
    public ImmutableArray<ISymbol> GetMembers(string name) => ImmutableArray<ISymbol>.Empty;
    public ITypeSymbol? LookupType(string name) => null;
    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        symbol = null;
        return false;
    }

    internal Type GetTypeInfo() => _runtimeType;

    private ImmutableArray<INamedTypeSymbol> GetConstraintInterfaces(bool includeInherited)
    {
        var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
        var seen = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);

        foreach (var constraint in ConstraintTypes)
        {
            if (constraint is not INamedTypeSymbol named || named.TypeKind != TypeKind.Interface)
                continue;

            AddInterface(named, includeInherited, builder, seen);
        }

        return builder.ToImmutable();
    }

    private static void AddInterface(
        INamedTypeSymbol interfaceSymbol,
        bool includeInherited,
        ImmutableArray<INamedTypeSymbol>.Builder builder,
        HashSet<INamedTypeSymbol> seen)
    {
        if (!seen.Add(interfaceSymbol))
            return;

        builder.Add(interfaceSymbol);

        if (!includeInherited)
            return;

        foreach (var inherited in interfaceSymbol.AllInterfaces)
            if (seen.Add(inherited))
                builder.Add(inherited);
    }

    private static string GetOwnerIdentity(ISymbol containingSymbol)
    {
        return containingSymbol switch
        {
            PEMethodSymbol peMethod => BuildMethodOwnerIdentity(peMethod.GetMethodBase()),
            IMethodSymbol method => $"{method.ContainingType?.ToFullyQualifiedMetadataName()}::{method.MetadataName}",
            INamedTypeSymbol type => type.ToFullyQualifiedMetadataName(),
            _ => containingSymbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)
        };
    }

    private static string BuildMethodOwnerIdentity(MethodBase methodBase)
    {
        var declaringTypeName = methodBase.DeclaringType?.FullName ?? string.Empty;
        return $"{declaringTypeName}::{methodBase.Name}";
    }
}
