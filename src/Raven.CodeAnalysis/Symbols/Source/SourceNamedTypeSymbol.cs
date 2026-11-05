using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceNamedTypeSymbol : SourceSymbol, NamedTypeSymbol, Raven.CodeAnalysis.Metadata.IArrayTypeProvider
{
    private readonly List<Symbol> _members = new List<Symbol>();
    private readonly object _membersGate = new();
    private readonly string _metadataName;
    private bool _isStatic;
    private ImmutableArray<NamedTypeSymbol> _interfaces = ImmutableArray<NamedTypeSymbol>.Empty;
    private ImmutableArray<NamedTypeSymbol>? _allInterfaces;
    private ImmutableArray<TypeParameterSymbol> _typeParameters = ImmutableArray<TypeParameterSymbol>.Empty;
    private ImmutableArray<TypeSymbol> _typeArguments = ImmutableArray<TypeSymbol>.Empty;
    private TypeSymbol? _extensionReceiverType;
    private ImmutableArray<SourcePropertySymbol> _recordProperties = ImmutableArray<SourcePropertySymbol>.Empty;
    private ImmutableArray<SourcePropertySymbol> _deconstructProperties = ImmutableArray<SourcePropertySymbol>.Empty;
    private bool _isSealedHierarchy;
    private bool _hasExplicitPermits;
    private ImmutableArray<NamedTypeSymbol> _permittedDirectSubtypes = ImmutableArray<NamedTypeSymbol>.Empty;
    private string? _sealedHierarchySourceFile;
    private bool? _lazyHasPrimaryConstructorSyntax;
    private bool _primaryConstructorMembersEnsured;
    private bool _ensuringPrimaryConstructorMembers;

    public SourceNamedTypeSymbol(string name, Symbol containingSymbol, NamedTypeSymbol? containingType, NamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences, bool isStatic = false, Accessibility declaredAccessibility = Accessibility.NotApplicable, string? metadataName = null)
        : base(SymbolKind.Type, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences, declaredAccessibility)
    {
        _metadataName = metadataName ?? name;
        BaseType = containingSymbol.ContainingAssembly!.GetTypeByMetadataName("System.Object");

        TypeKind = DetermineTypeKind(TypeKind.Class, BaseType);
        _isStatic = isStatic;

        if (isStatic)
        {
            IsClosed = true;
            IsAbstract = true;
        }
        else
        {
            IsClosed = true;
            IsAbstract = false;
        }
    }

    public SourceNamedTypeSymbol(
        string name,
        NamedTypeSymbol baseType,
        TypeKind typeKind,
        Symbol containingSymbol,
        NamedTypeSymbol? containingType,
        NamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        bool isSealed = false,
        bool isAbstract = false,
        bool isStatic = false,
        Accessibility declaredAccessibility = Accessibility.NotApplicable,
        string? metadataName = null,
        bool addAsMember = true)
    : base(SymbolKind.Type, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences, declaredAccessibility, addAsMember)
    {
        _metadataName = metadataName ?? name;
        BaseType = baseType;

        TypeKind = DetermineTypeKind(typeKind, baseType);
        _isStatic = isStatic;

        if (isStatic)
        {
            IsClosed = true;
            IsAbstract = true;
        }
        else
        {
            IsClosed = isSealed;
            IsAbstract = isAbstract;
        }
    }

    public bool IsNamespace { get; } = false;
    public bool IsType { get; } = true;

    public ImmutableArray<MethodSymbol> Constructors => GetMembers()
        .OfType<MethodSymbol>()
        .Where(x => x.MethodKind == MethodKind.Constructor)
        .Cast<MethodSymbol>()
        .ToImmutableArray();

    public ImmutableArray<MethodSymbol> InstanceConstructors => Constructors;

    public MethodSymbol? StaticConstructor => GetMembers()
        .OfType<MethodSymbol>()
        .FirstOrDefault(m => m.MethodKind == MethodKind.StaticConstructor);
    public ImmutableArray<TypeSymbol> TypeArguments => _typeArguments;
    public ImmutableArray<TypeParameterSymbol> TypeParameters => _typeParameters;
    public TypeSymbol? ConstructedFrom => this;
    public bool IsAbstract { get; private set; }
    public bool IsClosed { get; private set; }
    public override bool IsStatic => _isStatic;
    public bool IsGenericType => !_typeParameters.IsDefaultOrEmpty && _typeParameters.Length > 0;
    public bool IsUnboundGenericType => false;
    public bool IsRefLikeType => TypeKind == TypeKind.Struct &&
        DeclaringSyntaxReferences.Any(reference =>
            reference.GetSyntax() is StructDeclarationSyntax declaration &&
            declaration.Modifiers.Any(modifier => modifier.IsKind(SyntaxKind.RefKeyword)));
    public virtual bool IsReadOnly => TypeKind == TypeKind.Struct &&
        DeclaringSyntaxReferences.Any(reference =>
            reference.GetSyntax() is TypeDeclarationSyntax declaration &&
            declaration.Modifiers.Any(modifier => modifier.IsKind(SyntaxKind.ReadonlyKeyword)));

    public bool HasPartialModifier { get; private set; }

    public bool HasNonPartialDeclaration { get; private set; }

    public bool IsExtensionDeclaration { get; private set; }
    internal TypeSymbol? ExtensionReceiverType => _extensionReceiverType;
    internal bool IsRecord { get; private set; }
    internal ImmutableArray<SourcePropertySymbol> RecordProperties => _recordProperties;
    internal ImmutableArray<SourcePropertySymbol> DeconstructProperties => _deconstructProperties;
    public bool IsSealedHierarchy => _isSealedHierarchy;
    public bool HasExplicitPermits => _hasExplicitPermits;
    public ImmutableArray<NamedTypeSymbol> PermittedDirectSubtypes => _permittedDirectSubtypes;
    internal string? SealedHierarchySourceFile => _sealedHierarchySourceFile;
    internal bool HasPrimaryConstructorSyntax => _lazyHasPrimaryConstructorSyntax ??= ComputeHasPrimaryConstructorSyntax();

    public ImmutableArray<NamedTypeSymbol> Interfaces => _interfaces;
    public ImmutableArray<NamedTypeSymbol> AllInterfaces =>
        GetDeclaringCompilation() is { SourceDeclarationsComplete: false }
            ? ComputeAllInterfaces() : _allInterfaces ??= ComputeAllInterfaces();

    public bool IsValueType => TypeKind == TypeKind.Struct || TypeKind == TypeKind.Enum;

    public override string MetadataName
    {
        get
        {
            var name = _metadataName;

            if (IsGenericType)
                name = $"{name}`{Arity}";

            return name;
        }
    }

    public bool IsReferenceType =>
        TypeKind == TypeKind.Class ||
        TypeKind == TypeKind.Interface ||
        TypeKind == TypeKind.Delegate ||
        TypeKind == TypeKind.Array;

    public SpecialType SpecialType
    {
        get
        {
            var compilation = GetDeclaringCompilation();
            if (compilation?.GetSourceCoreSpecialType(this) is { } sourceSpecial && sourceSpecial != SpecialType.None)
                return sourceSpecial;
            if (Name == "Object" && MetadataName == "Object" && TypeKind == TypeKind.Class &&
                ContainingType is null && ContainingNamespace?.ToDisplayString() == "System" &&
                compilation?.UsesSourceObjectRoot == true)
                return SpecialType.System_Object;
            if (ContainingType is null && compilation?.Options.TargetPlatform == TargetPlatform.NeoCLR &&
                compilation.Options.MetadataImportOptions?.AsyncAssemblyName == ContainingAssembly?.Name)
                return (ContainingNamespace?.ToMetadataName() + "." + MetadataName) switch
                {
                    "System.Runtime.CompilerServices.AsyncStateMachine" => SpecialType.System_Runtime_CompilerServices_IAsyncStateMachine,
                    "System.Concurrency.Tasks.Task`1" => SpecialType.System_Threading_Tasks_Task_T,
                    "System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1" => SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder_T,
                    _ => SpecialType.None
                };
            return SpecialType.None;
        }
    }

    public virtual NamedTypeSymbol? BaseType { get; private set; }

    public TypeKind TypeKind { get; }

    public TypeSymbol? OriginalDefinition => this;

    public int Arity => _typeParameters.Length;

    public TypeSymbol? EnumUnderlyingType { get; private set; }

    public NamedTypeSymbol? UnderlyingTupleType => null;

    public ImmutableArray<FieldSymbol> TupleElements => [];

    public ImmutableArray<Symbol> GetMembers()
    {
        EnsurePrimaryConstructorMembersDeclared();

        lock (_membersGate)
            return _members.Distinct(SymbolReferenceComparer.Instance).ToImmutableArray();
    }

    public ImmutableArray<Symbol> GetMembers(string name)
    {
        EnsurePrimaryConstructorMembersDeclared();

        lock (_membersGate)
        {
            return _members
                .Where(x => x.Name == name)
                .Distinct(SymbolReferenceComparer.Instance)
                .ToImmutableArray();
        }
    }

    private sealed class SymbolReferenceComparer : IEqualityComparer<Symbol>
    {
        public static readonly SymbolReferenceComparer Instance = new();

        public bool Equals(Symbol? x, Symbol? y) => ReferenceEquals(x, y);

        public int GetHashCode(Symbol obj) => RuntimeHelpers.GetHashCode(obj);
    }

    public TypeSymbol? LookupType(string name) =>
        TypeLookupUtilities.SelectBestTypeByName(GetMembers(name).OfType<TypeSymbol>());

    internal ImmutableArray<Symbol> GetDeclaredMembersWithoutEnsuring(string name)
    {
        lock (_membersGate)
        {
            return _members
                .Where(x => x.Name == name)
                .Distinct(SymbolReferenceComparer.Instance)
                .ToImmutableArray();
        }
    }

    internal ImmutableArray<Symbol> GetDeclaredMembersWithoutEnsuring()
    {
        lock (_membersGate)
        {
            return _members
                .Distinct(SymbolReferenceComparer.Instance)
                .ToImmutableArray();
        }
    }

    internal void MarkPrimaryConstructorMembersDeclared()
    {
        if (!HasPrimaryConstructorSyntax)
            return;

        lock (_membersGate)
            _primaryConstructorMembersEnsured = true;
    }

    internal void AddMember(Symbol member)
    {
        lock (_membersGate)
        {
            var duplicateIndex = _members.FindIndex(existing => IsDuplicateMember(existing, member));
            if (duplicateIndex >= 0)
            {
                if (_members[duplicateIndex] is SourceMethodSymbol { IsSignatureSkeleton: true } &&
                    member is SourceMethodSymbol { IsSignatureSkeleton: false })
                {
                    _members[duplicateIndex] = member;
                }
                else if (ShouldReplaceDuplicateMember(_members[duplicateIndex], member))
                {
                    _members[duplicateIndex] = member;
                }

                return;
            }

            _members.Add(member);
        }
    }

    private static bool ShouldReplaceDuplicateMember(Symbol existing, Symbol member)
    {
        if (existing is PropertySymbol existingProperty &&
            member is PropertySymbol property)
        {
            return existingProperty.Type.ContainsErrorType() &&
                   !property.Type.ContainsErrorType();
        }

        if (existing is FieldSymbol existingField &&
            member is FieldSymbol field)
        {
            return existingField.Type.ContainsErrorType() &&
                   !field.Type.ContainsErrorType();
        }

        if (existing is EventSymbol existingEvent &&
            member is EventSymbol @event)
        {
            return existingEvent.Type.ContainsErrorType() &&
                   !@event.Type.ContainsErrorType();
        }

        return false;
    }

    private static bool IsDuplicateMember(Symbol existing, Symbol member)
    {
        if (ReferenceEquals(existing, member))
            return true;

        if (existing.Kind != member.Kind || existing.Name != member.Name)
            return false;

        if (existing is MethodSymbol existingMethod &&
            member is MethodSymbol method &&
            IsDuplicateSynthesizedMethod(existingMethod, method))
        {
            return true;
        }

        if (IsTopLevelMainSourceAndSynthesizedPair(existing, member))
            return false;

        if (existing.DeclaringSyntaxReferences.IsDefaultOrEmpty ||
            member.DeclaringSyntaxReferences.IsDefaultOrEmpty)
        {
            if (!existing.DeclaringSyntaxReferences.IsDefaultOrEmpty ||
                !member.DeclaringSyntaxReferences.IsDefaultOrEmpty)
            {
                return false;
            }

            return existing is MethodSymbol existingSignature &&
                member is MethodSymbol memberSignature
                    ? HaveSameSignature(existingSignature, memberSignature)
                    : true;
        }

        foreach (var existingReference in existing.DeclaringSyntaxReferences)
        {
            foreach (var memberReference in member.DeclaringSyntaxReferences)
            {
                if (existingReference.SyntaxTree == memberReference.SyntaxTree &&
                    existingReference.Span == memberReference.Span)
                {
                    return true;
                }
            }
        }

        return false;
    }

    private static bool IsTopLevelMainSourceAndSynthesizedPair(Symbol existing, Symbol member)
    {
        if (existing.Name != EntryPointSignature.EntryPointName ||
            member.Name != EntryPointSignature.EntryPointName)
        {
            return false;
        }

        var existingIsSynthesizedMain = existing is SynthesizedMainMethodSymbol;
        var memberIsSynthesizedMain = member is SynthesizedMainMethodSymbol;
        if (existingIsSynthesizedMain == memberIsSynthesizedMain)
            return false;

        var sourceMethod = existingIsSynthesizedMain ? member : existing;
        return sourceMethod.DeclaringSyntaxReferences.Any(static reference =>
            reference.GetSyntax() is FunctionStatementSyntax);
    }

    private static bool IsDuplicateSynthesizedMethod(MethodSymbol existing, MethodSymbol method)
    {
        if (!ReferenceEquals(existing.ContainingType, method.ContainingType) &&
            !SymbolEqualityComparer.Default.Equals(existing.ContainingType, method.ContainingType))
        {
            return false;
        }

        if (existing.Name == ".ctor" &&
            method.Name == ".ctor" &&
            IsPrimaryConstructorSymbol(existing) &&
            IsPrimaryConstructorSymbol(method))
        {
            return true;
        }

        if (IsSynthesizedUnionHelperName(existing.Name))
            return true;

        if (existing.MethodKind != method.MethodKind ||
            existing.IsStatic != method.IsStatic)
            return false;

        return false;
    }

    private static bool IsPrimaryConstructorSymbol(MethodSymbol method)
        => method.DeclaringSyntaxReferences.Any(static reference =>
            reference.GetSyntax() is TypeDeclarationSyntax { ParameterList: not null });

    private static bool IsSynthesizedUnionHelperName(string name)
        => name is SynthesizedUnionMethodNames.DisplayNameHelper
            or SynthesizedUnionMethodNames.FriendlyTypeNameHelper
            or SynthesizedUnionMethodNames.FormatValueHelper;

    private static bool HaveSameSignature(MethodSymbol left, MethodSymbol right)
    {
        var leftParameters = left.Parameters;
        var rightParameters = right.Parameters;
        if (leftParameters.IsDefault || rightParameters.IsDefault)
            return false;

        if (leftParameters.Length != rightParameters.Length)
            return false;

        for (var i = 0; i < leftParameters.Length; i++)
        {
            if (!SymbolEqualityComparer.Default.Equals(leftParameters[i].Type, rightParameters[i].Type))
                return false;
        }

        return true;
    }

    internal void RemoveMember(Symbol member)
    {
        lock (_membersGate)
        {
            var index = _members.FindIndex(existing => ReferenceEquals(existing, member));
            if (index >= 0)
                _members.RemoveAt(index);
        }
    }

    internal void SetInterfaces(IEnumerable<NamedTypeSymbol> interfaces)
    {
        _interfaces = interfaces.ToImmutableArray();
        _allInterfaces = null;
    }

    internal void SetBaseType(NamedTypeSymbol? baseType)
    {
        BaseType = baseType;
        _allInterfaces = null;
    }

    internal void AddDeclaration(Location location, SyntaxReference reference)
        => base.AddDeclaration(location, reference);

    internal void UpdateDeclarationModifiers(bool isSealed, bool isAbstract, bool isStatic)
    {
        if (isStatic)
        {
            _isStatic = true;
            IsClosed = true;
            IsAbstract = true;
            return;
        }

        if (!isSealed)
            IsClosed = false;

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

    internal void RegisterRecordModifier(bool hasRecordModifier)
    {
        if (hasRecordModifier)
            IsRecord = true;
    }

    internal void SetRecordProperties(ImmutableArray<SourcePropertySymbol> properties)
    {
        if (!properties.IsDefault)
            _recordProperties = properties;
    }

    internal void SetDeconstructProperties(ImmutableArray<SourcePropertySymbol> properties)
    {
        if (!properties.IsDefault)
            _deconstructProperties = properties;
    }

    private bool ComputeHasPrimaryConstructorSyntax()
    {
        foreach (var syntaxReference in DeclaringSyntaxReferences)
        {
            if (syntaxReference.GetSyntax() is TypeDeclarationSyntax { ParameterList: not null })
                return true;
        }

        return false;
    }

    internal void MarkAsExtensionContainer()
    {
        IsExtensionDeclaration = true;
        IsClosed = true;
    }

    internal void SetExtensionReceiverType(TypeSymbol? receiverType)
    {
        if (!IsExtensionDeclaration)
            return;

        _extensionReceiverType = receiverType;
    }

    internal void SetTypeParameters(IEnumerable<TypeParameterSymbol> typeParameters)
    {
        _typeParameters = typeParameters.ToImmutableArray();
        _typeArguments = _typeParameters.Length == 0
            ? ImmutableArray<TypeSymbol>.Empty
            : _typeParameters.Select(static tp => (TypeSymbol)tp).ToImmutableArray();
    }

    private ImmutableArray<NamedTypeSymbol> ComputeAllInterfaces()
    {
        if (_interfaces.IsDefaultOrEmpty && (BaseType?.AllInterfaces.IsDefaultOrEmpty ?? true))
            return ImmutableArray<NamedTypeSymbol>.Empty;

        var seen = new HashSet<Symbol>(SymbolEqualityComparer.Default)
        {
            this
        };

        var builder = ImmutableArray.CreateBuilder<NamedTypeSymbol>();

        foreach (var interfaceType in _interfaces)
            AddInterface(interfaceType);

        if (BaseType is NamedTypeSymbol baseType)
        {
            foreach (var inherited in baseType.AllInterfaces)
            {
                if (seen.Add(inherited))
                    builder.Add(inherited);
            }
        }

        return builder.ToImmutable();

        void AddInterface(NamedTypeSymbol interfaceType)
        {
            if (!seen.Add(interfaceType))
                return;

            builder.Add(interfaceType);

            var inheritedInterfaces = interfaceType.Interfaces;
            if (inheritedInterfaces.IsDefaultOrEmpty)
                return;

            foreach (var inherited in inheritedInterfaces)
                AddInterface(inherited);
        }
    }

    public bool IsMemberDefined(string name, out Symbol? symbol)
    {
        symbol = GetMembers(name).FirstOrDefault();
        return symbol is not null;
    }

    public TypeSymbol Construct(params TypeSymbol[] typeArguments)
    {
        if (!IsGenericType)
            return this;

        if (typeArguments.Length != Arity)
            throw new ArgumentException($"Type '{Name}' expects {Arity} type arguments but received {typeArguments.Length}.", nameof(typeArguments));

        return new ConstructedNamedTypeSymbol(this, typeArguments.ToImmutableArray());
    }

    public override ImmutableArray<AttributeData> GetAttributes()
    {
        var attributes = base.GetAttributes();
        if (IsExtensionDeclaration && CreateExtensionAttributeData() is { } extensionAttribute)
            attributes = attributes.Add(extensionAttribute);

        // RequiredMember is a paired metadata contract: members advertise the
        // requirement and the declaring type advertises the required-member list.
        if (GetMembers().Any(member =>
                SymbolEqualityComparer.Default.Equals(member.ContainingType, this) &&
                member is PropertySymbol { IsRequired: true } or FieldSymbol { IsRequired: true }) &&
            CreateRequiredMemberAttribute() is { } requiredMember)
            attributes = attributes.Add(requiredMember);

        return attributes;
    }

    private AttributeData? CreateExtensionAttributeData()
    {
        var compilation = GetDeclaringCompilation();
        if (compilation is null)
            return null;

        if (DeclaringSyntaxReferences.IsDefaultOrEmpty)
            return null;

        var syntaxReference = DeclaringSyntaxReferences.FirstOrDefault();
        if (syntaxReference is null)
            return null;

        var attributeType = compilation.GetTypeByMetadataName("System.Runtime.CompilerServices.ExtensionAttribute");
        if (attributeType is not NamedTypeSymbol namedAttributeType)
            return null;

        var constructor = namedAttributeType.Constructors.FirstOrDefault(c => !c.IsStatic && c.Parameters.Length == 0);
        if (constructor is null)
            return null;

        return new AttributeData(
            namedAttributeType,
            constructor,
            ImmutableArray<TypedConstant>.Empty,
            ImmutableArray<KeyValuePair<string, TypedConstant>>.Empty,
            syntaxReference);
    }

    private static TypeKind DetermineTypeKind(TypeKind declaredTypeKind, NamedTypeSymbol? baseType)
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

    internal void MarkAsSealedHierarchy(string sourceFile, bool hasExplicitPermits)
    {
        _isSealedHierarchy = true;
        _hasExplicitPermits = hasExplicitPermits;
        _sealedHierarchySourceFile = sourceFile;
        IsClosed = false;
    }

    internal void SetPermittedDirectSubtypes(ImmutableArray<NamedTypeSymbol> subtypes)
    {
        if (!subtypes.IsDefault)
            _permittedDirectSubtypes = subtypes;
    }

    private void EnsurePrimaryConstructorMembersDeclared()
    {
        if (!HasPrimaryConstructorSyntax)
            return;

        lock (_membersGate)
        {
            if (_primaryConstructorMembersEnsured || _ensuringPrimaryConstructorMembers)
                return;

            _ensuringPrimaryConstructorMembers = true;
        }

        try
        {
            var compilation = GetDeclaringCompilation();
            if (compilation is null)
                return;

            compilation.EnsureSourceDeclarationsComplete();
        }
        finally
        {
            lock (_membersGate)
            {
                _primaryConstructorMembersEnsured = true;
                _ensuringPrimaryConstructorMembers = false;
            }
        }
    }

    public void SetEnumUnderlyingType(TypeSymbol underlyingType)
    {
        EnumUnderlyingType = underlyingType;
    }
}
