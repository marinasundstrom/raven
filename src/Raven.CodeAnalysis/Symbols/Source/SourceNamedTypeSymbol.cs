using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.CompilerServices;

namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceNamedTypeSymbol : SourceSymbol, INamedTypeSymbol
{
    private readonly List<ISymbol> _members = new List<ISymbol>();
    private ImmutableArray<INamedTypeSymbol> _interfaces = ImmutableArray<INamedTypeSymbol>.Empty;
    private ImmutableArray<INamedTypeSymbol>? _allInterfaces;
    private ImmutableArray<ITypeParameterSymbol> _typeParameters = ImmutableArray<ITypeParameterSymbol>.Empty;
    private ImmutableArray<ITypeSymbol> _typeArguments = ImmutableArray<ITypeSymbol>.Empty;
    private ITypeSymbol? _extensionReceiverType;

    public SourceNamedTypeSymbol(string name, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences, Accessibility declaredAccessibility = Accessibility.NotApplicable)
        : base(SymbolKind.Type, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences, declaredAccessibility)
    {
        BaseType = containingSymbol.ContainingAssembly!.GetTypeByMetadataName("System.Object");

        TypeKind = DetermineTypeKind(TypeKind.Class, BaseType);
        IsSealed = true;
        IsAbstract = false;
    }

    public SourceNamedTypeSymbol(
        string name,
        INamedTypeSymbol baseType,
        TypeKind typeKind,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        bool isSealed = false,
        bool isAbstract = false,
        Accessibility declaredAccessibility = Accessibility.NotApplicable)
    : base(SymbolKind.Type, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences, declaredAccessibility)
    {
        BaseType = baseType;

        TypeKind = DetermineTypeKind(typeKind, baseType);
        IsSealed = isSealed;
        IsAbstract = isAbstract;
    }

    public bool IsNamespace { get; } = false;
    public bool IsType { get; } = true;

    public ImmutableArray<IMethodSymbol> Constructors => _members
        .OfType<SourceMethodSymbol>()
        .Where(x => x.MethodKind == MethodKind.Constructor)
        .Cast<IMethodSymbol>()
        .ToImmutableArray();

    public ImmutableArray<IMethodSymbol> InstanceConstructors => Constructors;

    public IMethodSymbol? StaticConstructor { get; }
    public ImmutableArray<ITypeSymbol> TypeArguments => _typeArguments;
    public ImmutableArray<ITypeParameterSymbol> TypeParameters => _typeParameters;
    public ITypeSymbol? ConstructedFrom => this;
    public bool IsAbstract { get; private set; }
    public bool IsSealed { get; private set; }
    public bool IsGenericType => !_typeParameters.IsDefaultOrEmpty && _typeParameters.Length > 0;
    public bool IsUnboundGenericType => false;

    public bool HasPartialModifier { get; private set; }

    public bool HasNonPartialDeclaration { get; private set; }

    public bool IsExtensionDeclaration { get; private set; }
    internal ITypeSymbol? ExtensionReceiverType => _extensionReceiverType;

    public ImmutableArray<INamedTypeSymbol> Interfaces => _interfaces;
    public ImmutableArray<INamedTypeSymbol> AllInterfaces =>
        _allInterfaces ??= ComputeAllInterfaces();

    public bool IsValueType => TypeKind == TypeKind.Struct || TypeKind == TypeKind.Enum;

    public override string MetadataName
    {
        get
        {
            var name = Name;

            if (IsGenericType)
                name = $"{name}`{Arity}";

            if (ContainingType is INamedTypeSymbol containingType)
                return $"{containingType.MetadataName}+{name}";

            if (ContainingNamespace is { IsGlobalNamespace: false } containingNamespace)
            {
                var namespaceMetadata = containingNamespace.MetadataName;
                return string.IsNullOrEmpty(namespaceMetadata)
                    ? name
                    : $"{namespaceMetadata}.{name}";
            }

            return name;
        }
    }

    public bool IsReferenceType =>
        TypeKind == TypeKind.Class ||
        TypeKind == TypeKind.Interface ||
        TypeKind == TypeKind.Delegate ||
        TypeKind == TypeKind.Array;

    public SpecialType SpecialType => SpecialType.None;

    public virtual INamedTypeSymbol? BaseType { get; }

    public TypeKind TypeKind { get; }

    public ITypeSymbol? OriginalDefinition => this;

    public int Arity => _typeParameters.Length;

    public INamedTypeSymbol UnderlyingTupleType => throw new NotImplementedException();

    public ImmutableArray<IFieldSymbol> TupleElements => throw new NotImplementedException();

    public ImmutableArray<ISymbol> GetMembers()
    {
        return _members.Distinct(SymbolReferenceComparer.Instance).ToImmutableArray();
    }

    public ImmutableArray<ISymbol> GetMembers(string name)
    {
        return _members
            .Where(x => x.Name == name)
            .Distinct(SymbolReferenceComparer.Instance)
            .ToImmutableArray();
    }

    private sealed class SymbolReferenceComparer : IEqualityComparer<ISymbol>
    {
        public static readonly SymbolReferenceComparer Instance = new();

        public bool Equals(ISymbol? x, ISymbol? y) => ReferenceEquals(x, y);

        public int GetHashCode(ISymbol obj) => RuntimeHelpers.GetHashCode(obj);
    }

    public ITypeSymbol? LookupType(string name) =>
        _members.OfType<INamedTypeSymbol>().FirstOrDefault(t => t.Name == name);

    internal void AddMember(ISymbol member)
    {
        _members.Add(member);
    }

    internal void SetInterfaces(IEnumerable<INamedTypeSymbol> interfaces)
    {
        _interfaces = interfaces.ToImmutableArray();
        _allInterfaces = null;
    }

    internal void AddDeclaration(Location location, SyntaxReference reference)
    {
        Locations = Locations.Add(location);
        DeclaringSyntaxReferences = DeclaringSyntaxReferences.Add(reference);
    }

    internal void UpdateDeclarationModifiers(bool isSealed, bool isAbstract)
    {
        if (!isSealed)
            IsSealed = false;

        if (isAbstract)
            IsAbstract = true;
    }

    internal void RegisterPartialModifier(bool hasPartialModifier)
    {
        if (hasPartialModifier)
            HasPartialModifier = true;

        if (!hasPartialModifier)
            HasNonPartialDeclaration = true;
    }

    internal void MarkAsExtensionContainer()
    {
        IsExtensionDeclaration = true;
        IsSealed = true;
    }

    internal void SetExtensionReceiverType(ITypeSymbol? receiverType)
    {
        if (!IsExtensionDeclaration)
            return;

        _extensionReceiverType = receiverType;
    }

    internal void SetTypeParameters(IEnumerable<ITypeParameterSymbol> typeParameters)
    {
        _typeParameters = typeParameters.ToImmutableArray();
        _typeArguments = _typeParameters.Length == 0
            ? ImmutableArray<ITypeSymbol>.Empty
            : _typeParameters.Select(static tp => (ITypeSymbol)tp).ToImmutableArray();
    }

    private ImmutableArray<INamedTypeSymbol> ComputeAllInterfaces()
    {
        if (_interfaces.IsDefaultOrEmpty && (BaseType?.AllInterfaces.IsDefaultOrEmpty ?? true))
            return ImmutableArray<INamedTypeSymbol>.Empty;

        var seen = new HashSet<ISymbol>(SymbolEqualityComparer.Default)
        {
            this
        };

        var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();

        foreach (var interfaceType in _interfaces)
            AddInterface(interfaceType);

        if (BaseType is INamedTypeSymbol baseType)
        {
            foreach (var inherited in baseType.AllInterfaces)
            {
                if (seen.Add(inherited))
                    builder.Add(inherited);
            }
        }

        return builder.ToImmutable();

        void AddInterface(INamedTypeSymbol interfaceType)
        {
            if (!seen.Add(interfaceType))
                return;

            builder.Add(interfaceType);

            foreach (var inherited in interfaceType.Interfaces)
                AddInterface(inherited);
        }
    }

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        symbol = _members.FirstOrDefault(m => m.Name == name);
        return symbol is not null;
    }

    public ITypeSymbol Construct(params ITypeSymbol[] typeArguments)
    {
        if (!IsGenericType)
            return this;

        if (typeArguments.Length != Arity)
            throw new ArgumentException($"Type '{Name}' expects {Arity} type arguments but received {typeArguments.Length}.", nameof(typeArguments));

        return new ConstructedNamedTypeSymbol(this, typeArguments.ToImmutableArray());
    }

    private static TypeKind DetermineTypeKind(TypeKind declaredTypeKind, INamedTypeSymbol? baseType)
    {
        if (declaredTypeKind == TypeKind.Class && baseType is not null)
        {
            if (baseType.TypeKind == TypeKind.Delegate)
                return TypeKind.Delegate;

            if (baseType.SpecialType is SpecialType.System_MulticastDelegate or SpecialType.System_Delegate)
                return TypeKind.Delegate;
        }

        return declaredTypeKind;
    }
}
