using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PETypeParameterSymbol : SymbolBase, TypeParameterSymbol
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
    private ImmutableArray<AttributeData>? _attributes;
    private TypeParameterConstraintKind? _lazyConstraintKind;
    private ImmutableArray<TypeSymbol>? _lazyConstraintTypes;

    public PETypeParameterSymbol(
        Type type,
        Symbol containingSymbol,
        NamedTypeSymbol? containingType,
        NamespaceSymbol? containingNamespace,
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

    public override ImmutableArray<AttributeData> GetAttributes()
    {
        if (_attributes.HasValue)
            return _attributes.Value;

        try
        {
            _attributes = ImmutableArray.CreateRange(
                _runtimeType.GetCustomAttributesData()
                    .Select(attribute => PEAttributeDataFactory.Create(_reflectionTypeLoader, attribute))
                    .OfType<AttributeData>());
        }
        catch
        {
            _attributes = ImmutableArray<AttributeData>.Empty;
        }

        return _attributes.Value;
    }

    public override string Name => _name;

    public override SymbolKind Kind => SymbolKind.TypeParameter;
    public override AssemblySymbol ContainingAssembly => ContainingNamespace?.ContainingAssembly!;
    public override ModuleSymbol ContainingModule => ContainingNamespace?.ContainingModule!;
    protected PEAssemblySymbol PEContainingAssembly => (PEAssemblySymbol)ContainingAssembly;
    protected PEModuleSymbol PEContainingModule => (PEModuleSymbol)ContainingModule;

    public int Ordinal => _ordinal;

    public TypeParameterOwnerKind OwnerKind => _ownerKind;
    internal PETypeParameterIdentity MetadataIdentity => _metadataIdentity;

    public NamedTypeSymbol? DeclaringTypeParameterOwner =>
        OwnerKind == TypeParameterOwnerKind.Type ? (ContainingSymbol as NamedTypeSymbol) : null;

    public MethodSymbol? DeclaringMethodParameterOwner =>
        OwnerKind == TypeParameterOwnerKind.Method ? (ContainingSymbol as MethodSymbol) : null;

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

            if ((attributes & GenericParameterAttributes.AllowByRefLike) != 0)
                kind |= TypeParameterConstraintKind.AllowByRefLike;

            if (_constraintRuntimeTypes.Length > 0)
                kind |= TypeParameterConstraintKind.TypeConstraint;

            if ((kind & (TypeParameterConstraintKind.ReferenceType | TypeParameterConstraintKind.ValueType)) == 0 &&
                HasNotNullConstraint(_runtimeType.GetCustomAttributesData()))
            {
                kind |= TypeParameterConstraintKind.NotNull;
            }

            _lazyConstraintKind = kind;
            return kind;
        }
    }

    public bool IsReferenceType => TypeParameterFacts.IsKnownReferenceType(this);

    public bool IsValueType => (ConstraintKind & TypeParameterConstraintKind.ValueType) != 0;

    //public bool HasConstructorConstraint => (_type.GenericParameterAttributes & GenericParameterAttributes.DefaultConstructorConstraint) != 0;
    //public bool HasReferenceTypeConstraint => (_type.GenericParameterAttributes & GenericParameterAttributes.ReferenceTypeConstraint) != 0;
    //public bool HasValueTypeConstraint => (_type.GenericParameterAttributes & GenericParameterAttributes.NotNullableValueTypeConstraint) != 0;

    public VarianceKind Variance => _variance;

    /*
public ImmutableArray<TypeSymbol> ConstraintTypes =>
    _type.GetGenericParameterConstraints()
        .Select(t => new PENamedTypeSymbol(t, ContainingSymbol)) // assumes PENamedTypeSymbol exists
        .Cast<TypeSymbol>()
        .ToImmutableArray();
*/

    public ImmutableArray<TypeSymbol> ConstraintTypes
    {
        get
        {
            if (_lazyConstraintTypes.HasValue)
                return _lazyConstraintTypes.Value;

            var constraints = _constraintRuntimeTypes;
            if (constraints.Length == 0)
            {
                _lazyConstraintTypes = ImmutableArray<TypeSymbol>.Empty;
                return _lazyConstraintTypes.Value;
            }

            var builder = ImmutableArray.CreateBuilder<TypeSymbol>(constraints.Length);
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

    public NamedTypeSymbol? BaseType => null; // Not applicable to type parameters
    public TypeSymbol? OriginalDefinition => this;
    public SpecialType SpecialType => SpecialType.None;
    public TypeKind TypeKind => TypeKind.TypeParameter;

    public bool IsNamespace => false;
    public bool IsType => true;

    public ImmutableArray<NamedTypeSymbol> Interfaces => GetConstraintInterfaces(includeInherited: false);

    public ImmutableArray<NamedTypeSymbol> AllInterfaces => GetConstraintInterfaces(includeInherited: true);

    public ImmutableArray<Symbol> GetMembers() => ImmutableArray<Symbol>.Empty;
    public ImmutableArray<Symbol> GetMembers(string name) => ImmutableArray<Symbol>.Empty;
    public TypeSymbol? LookupType(string name) => null;
    public bool IsMemberDefined(string name, out Symbol? symbol)
    {
        symbol = null;
        return false;
    }

    internal Type GetTypeInfo() => _runtimeType;

    private ImmutableArray<NamedTypeSymbol> GetConstraintInterfaces(bool includeInherited)
    {
        var builder = ImmutableArray.CreateBuilder<NamedTypeSymbol>();
        var seen = new HashSet<NamedTypeSymbol>(SymbolEqualityComparer.Default);

        foreach (var constraint in ConstraintTypes)
        {
            if (constraint is not NamedTypeSymbol named || named.TypeKind != TypeKind.Interface)
                continue;

            AddInterface(named, includeInherited, builder, seen);
        }

        return builder.ToImmutable();
    }

    private static void AddInterface(
        NamedTypeSymbol interfaceSymbol,
        bool includeInherited,
        ImmutableArray<NamedTypeSymbol>.Builder builder,
        HashSet<NamedTypeSymbol> seen)
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

    private static bool HasNotNullConstraint(IList<CustomAttributeData> attributes)
    {
        foreach (var attribute in attributes)
        {
            if (attribute.AttributeType.FullName != "System.Runtime.CompilerServices.NullableAttribute" ||
                attribute.ConstructorArguments.Count != 1)
            {
                continue;
            }

            if (attribute.ConstructorArguments[0].Value is byte flag && flag == 1)
                return true;
        }

        return false;
    }

    private static string GetOwnerIdentity(Symbol containingSymbol)
    {
        return containingSymbol switch
        {
            PEMethodSymbol peMethod => BuildMethodOwnerIdentity(peMethod.GetMethodBase()),
            MethodSymbol method => $"{method.ContainingType?.ToFullyQualifiedMetadataName()}::{method.MetadataName}",
            NamedTypeSymbol type => type.ToFullyQualifiedMetadataName(),
            _ => containingSymbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)
        };
    }

    private static string BuildMethodOwnerIdentity(MethodBase methodBase)
    {
        var declaringTypeName = methodBase.DeclaringType?.FullName ?? string.Empty;
        return $"{declaringTypeName}::{methodBase.Name}";
    }
}
