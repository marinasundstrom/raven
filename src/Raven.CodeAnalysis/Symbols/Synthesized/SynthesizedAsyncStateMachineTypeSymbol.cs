using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class SynthesizedAsyncStateMachineTypeSymbol : SourceNamedTypeSymbol
{
    private static readonly Location[] s_emptyLocations = Array.Empty<Location>();
    private static readonly SyntaxReference[] s_emptySyntax = Array.Empty<SyntaxReference>();

    private readonly Dictionary<LocalSymbol, SourceFieldSymbol> _hoistedLocalMap = new(SymbolEqualityComparer.Default);
    private ImmutableArray<SourceFieldSymbol> _hoistedLocals;
    private ImmutableArray<SourceFieldSymbol> _hoistedLocalsToDispose;
    private readonly ImmutableDictionary<ParameterSymbol, SourceFieldSymbol> _parameterFieldMap;
    private readonly ImmutableDictionary<TypeParameterSymbol, TypeParameterSymbol> _asyncToStateTypeParameterMap;
    private readonly ImmutableDictionary<TypeParameterSymbol, TypeParameterSymbol> _stateToAsyncTypeParameterMap;
    private readonly ImmutableArray<TypeParameterMapping> _typeParameterMappings;
    private readonly AsyncBuilderMemberMap _builderMemberMap;
    private readonly Dictionary<MethodSymbol, ConstructedMembers> _constructedMembersCache = new(ReferenceEqualityComparer.Instance);

    public SynthesizedAsyncStateMachineTypeSymbol(
        Compilation compilation,
        MethodSymbol asyncMethod,
        string name,
        TypeSymbol? selfType = null)
        : base(
            name,
            compilation.GetSpecialType(compilation.Options.UseHeapAsyncStateMachines ? SpecialType.System_Object : SpecialType.System_ValueType),
            compilation.Options.UseHeapAsyncStateMachines ? TypeKind.Class : TypeKind.Struct,
            asyncMethod.ContainingSymbol ?? asyncMethod,
            asyncMethod.ContainingType,
            asyncMethod.ContainingNamespace,
            s_emptyLocations,
            s_emptySyntax,
            isSealed: true,
            declaredAccessibility: Accessibility.Private)
    {
        if (compilation is null)
            throw new ArgumentNullException(nameof(compilation));
        if (asyncMethod is null)
            throw new ArgumentNullException(nameof(asyncMethod));

        Compilation = compilation;
        AsyncMethod = asyncMethod ?? throw new ArgumentNullException(nameof(asyncMethod));

        (
            _asyncToStateTypeParameterMap,
            _stateToAsyncTypeParameterMap,
            _typeParameterMappings) = InitializeTypeParameters(asyncMethod);

        StateField = CreateField("_state", compilation.GetSpecialType(SpecialType.System_Int32));

        if (!asyncMethod.IsStatic)
        {
            var thisType = selfType ?? asyncMethod.ContainingType ?? compilation.GetSpecialType(SpecialType.System_Object);
            ThisField = CreateField("_this", thisType);
        }

        ParameterFields = CreateParameterFields(asyncMethod, out _parameterFieldMap);
        _hoistedLocals = ImmutableArray<SourceFieldSymbol>.Empty;
        _hoistedLocalsToDispose = ImmutableArray<SourceFieldSymbol>.Empty;

        var asyncStateMachineInterface = compilation.GetSpecialType(SpecialType.System_Runtime_CompilerServices_IAsyncStateMachine);
        SetInterfaces(new[] { asyncStateMachineInterface });

        BuilderField = CreateBuilderField(compilation, asyncMethod);
        _builderMemberMap = new AsyncBuilderMemberMap(this, CreateBuilderMembers(BuilderField));

        Constructor = CreateConstructor(compilation, asyncMethod);
        MoveNextMethod = CreateMoveNextMethod(compilation, asyncMethod);
        SetStateMachineMethod = CreateSetStateMachineMethod(compilation, asyncMethod, asyncStateMachineInterface);
    }

    public Compilation Compilation { get; }

    public MethodSymbol AsyncMethod { get; }

    public SourceFieldSymbol StateField { get; }

    public SourceFieldSymbol? ThisField { get; }

    public ImmutableArray<SourceFieldSymbol> ParameterFields { get; }

    public ImmutableDictionary<ParameterSymbol, SourceFieldSymbol> ParameterFieldMap => _parameterFieldMap;
    internal ImmutableDictionary<TypeParameterSymbol, TypeParameterSymbol> AsyncMethodTypeParameterMap => _asyncToStateTypeParameterMap;

    internal ImmutableArray<TypeParameterMapping> TypeParameterMappings => _typeParameterMappings;

    internal bool TryMapToStateMachineTypeParameter(TypeParameterSymbol asyncParameter, out TypeParameterSymbol mapped)
        => _asyncToStateTypeParameterMap.TryGetValue(asyncParameter, out mapped);

    internal bool TryMapToAsyncMethodTypeParameter(TypeParameterSymbol stateMachineParameter, out TypeParameterSymbol mapped)
        => _stateToAsyncTypeParameterMap.TryGetValue(stateMachineParameter, out mapped);

    public ImmutableArray<SourceFieldSymbol> HoistedLocals => _hoistedLocals;

    public ImmutableArray<SourceFieldSymbol> HoistedLocalsToDispose => _hoistedLocalsToDispose;

    public SourceFieldSymbol BuilderField { get; }

    public SourceMethodSymbol Constructor { get; }

    internal ImmutableArray<SourceFieldSymbol> ConstructorFields => IsValueType
        ? ImmutableArray<SourceFieldSymbol>.Empty
        : ThisField is null ? ParameterFields : ParameterFields.Insert(0, ThisField);

    public SourceMethodSymbol MoveNextMethod { get; }

    public SourceMethodSymbol SetStateMachineMethod { get; }

    public BoundBlockStatement? OriginalBody { get; private set; }

    public BoundBlockStatement? MoveNextBody { get; private set; }

    public BoundBlockStatement? SetStateMachineBody { get; private set; }

    public ConstructedMembers GetConstructedMembers(MethodSymbol method)
    {
        if (method is null)
            throw new ArgumentNullException(nameof(method));

        if (!ReferenceEquals(method, AsyncMethod))
            throw new ArgumentException("State machine constructed for different method.", nameof(method));

        if (_constructedMembersCache.TryGetValue(method, out var cached))
            return cached;

        var members = CreateConstructedMembers(method);
        _constructedMembersCache[method] = members;
        return members;
    }

    public BuilderMembers GetBuilderMembers(MethodSymbol method)
    {
        return _builderMemberMap.StateMachineMembers;
    }

    private ConstructedMembers CreateConstructedMembers(MethodSymbol method)
    {
        var stateMachineType = (NamedTypeSymbol)GetConstructedStateMachine(method);
        var constructor = GetConstructedMethod(Constructor, stateMachineType);
        var moveNext = GetConstructedMethod(MoveNextMethod, stateMachineType);
        var stateField = GetConstructedField(StateField, stateMachineType);
        var builderField = GetConstructedField(BuilderField, stateMachineType);
        var thisField = ThisField is null ? null : GetConstructedField(ThisField, stateMachineType);
        var parameterFields = ConstructParameterFieldMap(stateMachineType);
        var stateMachineBuilderMembers = _builderMemberMap.GetStateMachineMembers(builderField);
        var asyncMethodBuilderMembers = _builderMemberMap.GetAsyncMethodMembers(builderField);

        return new ConstructedMembers(
            stateMachineType,
            constructor,
            moveNext,
            stateField,
            builderField,
            thisField,
            parameterFields,
            stateMachineBuilderMembers,
            asyncMethodBuilderMembers);
    }

    public NamedTypeSymbol GetConstructedStateMachine(MethodSymbol method)
    {
        if (method is null)
            throw new ArgumentNullException(nameof(method));

        if (!ReferenceEquals(method, AsyncMethod))
            throw new ArgumentException("State machine constructed for different method.", nameof(method));

        if (TypeParameters.Length == 0)
        {
            // A nongeneric async method can still require a constructed state
            // because its enclosing class contributes generic parameters.
            for (var owner = ContainingType; owner is not null; owner = owner.ContainingType)
                if (!owner.TypeParameters.IsDefaultOrEmpty)
                    return new ConstructedNamedTypeSymbol(this, ImmutableArray<TypeSymbol>.Empty);
            return this;
        }

        var typeArguments = method.TypeArguments;
        if ((typeArguments.IsDefaultOrEmpty || typeArguments.Length == 0) &&
            !method.TypeParameters.IsDefaultOrEmpty &&
            method.TypeParameters.Length == TypeParameters.Length)
        {
            typeArguments = method.TypeParameters.Cast<TypeSymbol>().ToImmutableArray();
        }

        if (typeArguments.Length != TypeParameters.Length)
            return this;

        return new ConstructedNamedTypeSymbol(this, typeArguments);
    }

    public SourceFieldSymbol AddHoistedLocal(string name, TypeSymbol type, bool requiresDispose = false, LocalSymbol? local = null)
    {
        if (string.IsNullOrEmpty(name))
            throw new ArgumentException("Field name cannot be null or empty.", nameof(name));
        if (type is null)
            throw new ArgumentNullException(nameof(type));

        var field = CreateField(name, type);
        if (local is not null)
            _hoistedLocalMap[local] = field;
        _hoistedLocals = _hoistedLocals.Add(field);
        if (requiresDispose)
            _hoistedLocalsToDispose = _hoistedLocalsToDispose.Add(field);
        return field;
    }

    internal bool TryGetHoistedLocalField(LocalSymbol local, out SourceFieldSymbol field)
    {
        if (local is null)
            throw new ArgumentNullException(nameof(local));

        return _hoistedLocalMap.TryGetValue(local, out field!);
    }

    public void SetMoveNextBody(BoundBlockStatement body)
    {
        if (body is null)
            throw new ArgumentNullException(nameof(body));

        if (MoveNextBody is not null && !ReferenceEquals(MoveNextBody, body))
            throw new InvalidOperationException("MoveNext body already assigned.");

        MoveNextBody = body;
    }

    public void SetOriginalBody(BoundBlockStatement body)
    {
        if (body is null)
            throw new ArgumentNullException(nameof(body));

        if (OriginalBody is not null && !ReferenceEquals(OriginalBody, body))
            throw new InvalidOperationException("Original body already assigned.");

        OriginalBody = body;
    }

    public void SetSetStateMachineBody(BoundBlockStatement body)
    {
        if (body is null)
            throw new ArgumentNullException(nameof(body));

        if (SetStateMachineBody is not null && !ReferenceEquals(SetStateMachineBody, body))
            throw new InvalidOperationException("SetStateMachine body already assigned.");

        SetStateMachineBody = body;
    }

    private ImmutableArray<SourceFieldSymbol> CreateParameterFields(MethodSymbol method, out ImmutableDictionary<ParameterSymbol, SourceFieldSymbol> parameterFieldMap)
    {
        if (method.Parameters.Length == 0)
        {
            parameterFieldMap = ImmutableDictionary<ParameterSymbol, SourceFieldSymbol>.Empty;
            return ImmutableArray<SourceFieldSymbol>.Empty;
        }

        var orderedFields = ImmutableArray.CreateBuilder<SourceFieldSymbol>(method.Parameters.Length);
        var mapBuilder = ImmutableDictionary.CreateBuilder<ParameterSymbol, SourceFieldSymbol>(SymbolEqualityComparer.Default);

        foreach (var parameter in method.Parameters)
        {
            var field = CreateField($"_{parameter.Name}", parameter.Type);
            orderedFields.Add(field);
            mapBuilder.Add(parameter, field);
        }

        parameterFieldMap = mapBuilder.ToImmutable();
        return orderedFields.ToImmutable();
    }

    private SourceFieldSymbol CreateField(string name, TypeSymbol type)
    {
        var substitutedType = SubstituteAsyncMethodTypeParameters(type);

        var field = new SourceFieldSymbol(
            name,
            substitutedType,
            isStatic: false,
            isMutable: true,
            isConst: false,
            constantValue: null,
            this,
            this,
            ContainingNamespace,
            s_emptyLocations,
            s_emptySyntax,
            declaredAccessibility: Accessibility.Internal);

        AddMember(field);
        return field;
    }

    private SourceFieldSymbol CreateBuilderField(Compilation compilation, MethodSymbol asyncMethod)
    {
        var builderType = DetermineBuilderType(compilation, asyncMethod);
        return CreateField("_builder", builderType);
    }

    private FieldSymbol GetConstructedField(SourceFieldSymbol field, NamedTypeSymbol stateMachineType)
    {
        if (ReferenceEquals(stateMachineType, this))
            return field;

        return stateMachineType
            .GetMembers(field.Name)
            .OfType<FieldSymbol>()
            .First();
    }

    private MethodSymbol GetConstructedMethod(SourceMethodSymbol method, NamedTypeSymbol stateMachineType)
    {
        if (ReferenceEquals(stateMachineType, this))
            return method;

        foreach (var candidate in stateMachineType.GetMembers(method.Name).OfType<MethodSymbol>())
        {
            var original = candidate.OriginalDefinition ?? candidate;
            if (SymbolEqualityComparer.Default.Equals(original, method))
                return candidate;

            if (candidate.Parameters.Length == method.Parameters.Length && candidate.MethodKind == method.MethodKind)
                return candidate;
        }

        return method;
    }

    private ImmutableDictionary<ParameterSymbol, FieldSymbol> ConstructParameterFieldMap(NamedTypeSymbol stateMachineType)
    {
        if (_parameterFieldMap.IsEmpty)
            return ImmutableDictionary<ParameterSymbol, FieldSymbol>.Empty;

        var builder = ImmutableDictionary.CreateBuilder<ParameterSymbol, FieldSymbol>(SymbolEqualityComparer.Default);

        foreach (var (parameter, field) in _parameterFieldMap)
        {
            var constructedField = GetConstructedField(field, stateMachineType);
            builder[parameter] = constructedField;
        }

        return builder.ToImmutable();
    }

    private BuilderMembers CreateBuilderMembers(FieldSymbol builderField, NamedTypeSymbol? builderTypeOverride = null)
    {
        var builderType = builderTypeOverride ?? builderField.Type as NamedTypeSymbol;
        if (builderType is null)
            return new BuilderMembers(builderField, null, null, null, null, null, null, null);

        var create = FindParameterlessStaticMethod(builderType, "Create");
        var start = FindStartMethod(builderType);
        var setStateMachine = FindSetStateMachineMethod(builderType);
        var setResult = FindSetResultMethod(builderType);
        var setException = Compilation.Options.CaptureAsyncExceptions ? FindSetExceptionMethod(builderType) : null;
        var awaitOnCompleted = FindAwaitOnCompletedMethod(builderType);
        var taskProperty = FindTaskProperty(builderType);

        return new BuilderMembers(
            builderField,
            create,
            start,
            setStateMachine,
            setResult,
            setException,
            awaitOnCompleted,
            taskProperty);
    }

    private BuilderMembers CreateAsyncMethodBuilderMembers(
        FieldSymbol asyncBuilderField,
        BuilderMembers stateMachineMembers)
    {
        var asyncBuilderType = asyncBuilderField.Type as NamedTypeSymbol;
        if (asyncBuilderType is null)
        {
            return new BuilderMembers(
                asyncBuilderField,
                stateMachineMembers.Create is null ? null : SubstituteStateMachineTypeParameters(stateMachineMembers.Create),
                stateMachineMembers.Start is null ? null : SubstituteStateMachineTypeParameters(stateMachineMembers.Start),
                stateMachineMembers.SetStateMachine is null ? null : SubstituteStateMachineTypeParameters(stateMachineMembers.SetStateMachine),
                stateMachineMembers.SetResult is null ? null : SubstituteStateMachineTypeParameters(stateMachineMembers.SetResult),
                stateMachineMembers.SetException is null ? null : SubstituteStateMachineTypeParameters(stateMachineMembers.SetException),
                stateMachineMembers.AwaitOnCompleted is null ? null : SubstituteStateMachineTypeParameters(stateMachineMembers.AwaitOnCompleted),
                stateMachineMembers.TaskProperty is null ? null : SubstituteStateMachineTypeParameters(stateMachineMembers.TaskProperty));
        }

        var create = SubstituteBuilderMethodForAsyncMethod(stateMachineMembers.Create, asyncBuilderType);
        var start = SubstituteBuilderMethodForAsyncMethod(stateMachineMembers.Start, asyncBuilderType);
        var setStateMachine = SubstituteBuilderMethodForAsyncMethod(stateMachineMembers.SetStateMachine, asyncBuilderType);
        var setResult = SubstituteBuilderMethodForAsyncMethod(stateMachineMembers.SetResult, asyncBuilderType);
        var setException = SubstituteBuilderMethodForAsyncMethod(stateMachineMembers.SetException, asyncBuilderType);
        var awaitOnCompleted = SubstituteBuilderMethodForAsyncMethod(stateMachineMembers.AwaitOnCompleted, asyncBuilderType);
        var taskProperty = SubstituteBuilderPropertyForAsyncMethod(stateMachineMembers.TaskProperty, asyncBuilderType);

        return new BuilderMembers(
            asyncBuilderField,
            create is null ? null : SubstituteStateMachineTypeParameters(create),
            start is null ? null : SubstituteStateMachineTypeParameters(start),
            setStateMachine is null ? null : SubstituteStateMachineTypeParameters(setStateMachine),
            setResult is null ? null : SubstituteStateMachineTypeParameters(setResult),
            setException is null ? null : SubstituteStateMachineTypeParameters(setException),
            awaitOnCompleted is null ? null : SubstituteStateMachineTypeParameters(awaitOnCompleted),
            taskProperty is null ? null : SubstituteStateMachineTypeParameters(taskProperty));
    }

    private static MethodSymbol? SubstituteBuilderMethodForAsyncMethod(
        MethodSymbol? method,
        NamedTypeSymbol asyncBuilderType)
    {
        if (method is null)
            return null;

        if (SymbolEqualityComparer.Default.Equals(method.ContainingType, asyncBuilderType))
            return method;

        var targetDefinition = method.OriginalDefinition ?? method;

        foreach (var candidate in asyncBuilderType.GetMembers(method.Name).OfType<MethodSymbol>())
        {
            var candidateDefinition = candidate.OriginalDefinition ?? candidate;
            if (SymbolEqualityComparer.Default.Equals(candidateDefinition, targetDefinition))
                return candidate;
        }

        // Fall back to a shape match when symbols differ by source/PE wrappers.
        foreach (var candidate in asyncBuilderType.GetMembers(method.Name).OfType<MethodSymbol>())
        {
            if (candidate.Arity != method.Arity)
                continue;

            if (candidate.Parameters.Length != method.Parameters.Length)
                continue;

            if (candidate.IsStatic != method.IsStatic)
                continue;

            return candidate;
        }

        return method;
    }

    private static PropertySymbol? SubstituteBuilderPropertyForAsyncMethod(
        PropertySymbol? property,
        NamedTypeSymbol asyncBuilderType)
    {
        if (property is null)
            return null;

        if (SymbolEqualityComparer.Default.Equals(property.ContainingType, asyncBuilderType))
            return property;

        foreach (var candidate in asyncBuilderType.GetMembers(property.Name).OfType<PropertySymbol>())
            return candidate;

        return property;
    }

    private (
        ImmutableDictionary<TypeParameterSymbol, TypeParameterSymbol> AsyncToState,
        ImmutableDictionary<TypeParameterSymbol, TypeParameterSymbol> StateToAsync,
        ImmutableArray<TypeParameterMapping> Mappings) InitializeTypeParameters(MethodSymbol asyncMethod)
    {
        if (asyncMethod.TypeParameters.IsDefaultOrEmpty || asyncMethod.TypeParameters.Length == 0)
        {
            SetTypeParameters(ImmutableArray<SourceTypeParameterSymbol>.Empty);
            return (
                ImmutableDictionary<TypeParameterSymbol, TypeParameterSymbol>.Empty,
                ImmutableDictionary<TypeParameterSymbol, TypeParameterSymbol>.Empty,
                ImmutableArray<TypeParameterMapping>.Empty);
        }

        var builder = ImmutableArray.CreateBuilder<TypeParameterSymbol>(asyncMethod.TypeParameters.Length);
        var asyncToStateBuilder = ImmutableDictionary.CreateBuilder<TypeParameterSymbol, TypeParameterSymbol>(SymbolEqualityComparer.Default);
        var stateToAsyncBuilder = ImmutableDictionary.CreateBuilder<TypeParameterSymbol, TypeParameterSymbol>(SymbolEqualityComparer.Default);
        var mappingBuilder = ImmutableArray.CreateBuilder<TypeParameterMapping>(asyncMethod.TypeParameters.Length);

        foreach (var typeParameter in asyncMethod.TypeParameters)
        {
            var constraintKind = typeParameter is SourceTypeParameterSymbol sourceParameter
                ? sourceParameter.ConstraintKind
                : TypeParameterConstraintKind.None;
            var constraintTypeReferences = typeParameter is SourceTypeParameterSymbol sourceConstraintProvider
                ? sourceConstraintProvider.ConstraintTypeReferences
                : ImmutableArray<SyntaxReference>.Empty;

            var synthesized = new SourceTypeParameterSymbol(
                typeParameter.Name,
                this,
                this,
                ContainingNamespace,
                s_emptyLocations,
                s_emptySyntax,
                typeParameter.Ordinal,
                constraintKind,
                constraintTypeReferences,
                typeParameter.Variance);

            builder.Add(synthesized);
            asyncToStateBuilder.Add(typeParameter, synthesized);
            stateToAsyncBuilder.Add(synthesized, typeParameter);
            mappingBuilder.Add(new TypeParameterMapping(typeParameter, synthesized));
        }

        var mapped = asyncToStateBuilder.ToImmutable();
        SetTypeParameters(builder.MoveToImmutable());

        foreach (var typeParameter in asyncMethod.TypeParameters)
        {
            if (typeParameter is not SourceTypeParameterSymbol sourceParameter ||
                !sourceParameter.HasResolvedConstraintTypes ||
                !mapped.TryGetValue(typeParameter, out var mappedParameter) ||
                mappedParameter is not SourceTypeParameterSymbol synthesized)
            {
                continue;
            }

            var substitutedConstraints = SubstituteConstraintTypes(mapped, sourceParameter.ConstraintTypes);
            synthesized.SetConstraintTypes(substitutedConstraints);
        }

        return (
            mapped,
            stateToAsyncBuilder.ToImmutable(),
            mappingBuilder.MoveToImmutable());
    }

    private ImmutableArray<TypeSymbol> SubstituteConstraintTypes(
        ImmutableDictionary<TypeParameterSymbol, TypeParameterSymbol> substitutionMap,
        ImmutableArray<TypeSymbol> constraintTypes)
    {
        if (constraintTypes.IsDefaultOrEmpty)
            return constraintTypes;

        var builder = ImmutableArray.CreateBuilder<TypeSymbol>(constraintTypes.Length);

        foreach (var constraintType in constraintTypes)
        {
            builder.Add(SubstituteAsyncMethodTypeParameters(constraintType, substitutionMap));
        }

        return builder.MoveToImmutable();
    }

    internal TypeSymbol SubstituteAsyncMethodTypeParameters(
        TypeSymbol type,
        ImmutableDictionary<TypeParameterSymbol, TypeParameterSymbol>? substitutionMap = null)
    {
        var map = substitutionMap ?? _asyncToStateTypeParameterMap;

        if (map.Count == 0)
            return type;

        return Substitute(type);

        TypeSymbol Substitute(TypeSymbol symbol)
        {
            if (symbol is TypeParameterSymbol typeParameter &&
                TryMapTypeParameter(map, typeParameter, out var replacement))
            {
                return replacement;
            }

            if (symbol is RefTypeSymbol refType)
            {
                var substitutedElement = Substitute(refType.ElementType);
                if (!SymbolEqualityComparer.Default.Equals(substitutedElement, refType.ElementType))
                    return new RefTypeSymbol(substitutedElement);

                return symbol;
            }

            if (symbol is AddressTypeSymbol address)
            {
                var substitutedElement = Substitute(address.ReferencedType);
                if (!SymbolEqualityComparer.Default.Equals(substitutedElement, address.ReferencedType))
                    return new AddressTypeSymbolImplementation(substitutedElement);

                return symbol;
            }

            if (symbol is NamedTypeSymbol named && named.IsGenericType && !named.IsUnboundGenericType)
            {
                var typeArguments = named.TypeArguments;
                var substitutedArguments = new TypeSymbol[typeArguments.Length];
                var changed = false;

                for (int i = 0; i < typeArguments.Length; i++)
                {
                    substitutedArguments[i] = Substitute(typeArguments[i]);
                    if (!SymbolEqualityComparer.Default.Equals(substitutedArguments[i], typeArguments[i]))
                        changed = true;
                }

                if (changed)
                {
                    var definition = named.ConstructedFrom as NamedTypeSymbol ?? named;
                    return definition.Construct(substitutedArguments);
                }

                return named;
            }

            if (symbol is ArrayTypeSymbol array)
            {
                var substitutedElement = Substitute(array.ElementType);
                if (!SymbolEqualityComparer.Default.Equals(substitutedElement, array.ElementType))
                    return Compilation.CreateArrayTypeSymbol(substitutedElement, array.Rank, array.FixedLength);

                return symbol;
            }

            if (symbol is PointerTypeSymbol pointer)
            {
                var substitutedElement = Substitute(pointer.PointedAtType);
                if (!SymbolEqualityComparer.Default.Equals(substitutedElement, pointer.PointedAtType))
                    return Compilation.CreatePointerTypeSymbol(substitutedElement);

                return symbol;
            }

            return symbol;
        }
    }

    private static bool TryMapTypeParameter(
        ImmutableDictionary<TypeParameterSymbol, TypeParameterSymbol> map,
        TypeParameterSymbol typeParameter,
        out TypeParameterSymbol replacement)
    {
        static MethodSymbol? NormalizeMethodOwner(MethodSymbol? method)
            => method is null ? null : (MethodSymbol)(method.OriginalDefinition ?? method);

        static NamedTypeSymbol? NormalizeTypeOwner(NamedTypeSymbol? type)
            => type is null ? null : (NamedTypeSymbol)(type.OriginalDefinition ?? type);

        if (map.TryGetValue(typeParameter, out replacement!))
            return true;

        var normalized = (TypeParameterSymbol)(typeParameter.OriginalDefinition ?? typeParameter);
        if (!ReferenceEquals(normalized, typeParameter) && map.TryGetValue(normalized, out replacement!))
            return true;

        foreach (var entry in map)
        {
            var key = entry.Key;
            var normalizedKey = (TypeParameterSymbol)(key.OriginalDefinition ?? key);

            if (normalized.OwnerKind != normalizedKey.OwnerKind)
                continue;

            if (normalized.Ordinal != normalizedKey.Ordinal)
                continue;

            if (normalized.OwnerKind == TypeParameterOwnerKind.Method)
            {
                var normalizedMethodOwner = NormalizeMethodOwner(normalized.DeclaringMethodParameterOwner);
                var normalizedKeyMethodOwner = NormalizeMethodOwner(normalizedKey.DeclaringMethodParameterOwner);

                if (!SymbolEqualityComparer.Default.Equals(normalizedMethodOwner, normalizedKeyMethodOwner))
                {
                    continue;
                }
            }
            else
            {
                var normalizedTypeOwner = NormalizeTypeOwner(normalized.DeclaringTypeParameterOwner);
                var normalizedKeyTypeOwner = NormalizeTypeOwner(normalizedKey.DeclaringTypeParameterOwner);

                if (!SymbolEqualityComparer.Default.Equals(normalizedTypeOwner, normalizedKeyTypeOwner))
                {
                    continue;
                }
            }

            replacement = entry.Value;
            return true;
        }

        replacement = null!;
        return false;
    }

    internal TypeSymbol SubstituteStateMachineTypeParameters(TypeSymbol type)
        => SubstituteAsyncMethodTypeParameters(type, _stateToAsyncTypeParameterMap);

    internal MethodSymbol SubstituteAsyncMethodTypeParameters(MethodSymbol method)
    {
        if (method is null)
            throw new ArgumentNullException(nameof(method));

        if (_asyncToStateTypeParameterMap.Count == 0)
            return method;

        if (method is MappedMethodSymbol mapped && ReferenceEquals(mapped.Owner, this) && mapped.MappingKind == TypeParameterMappingKind.AsyncToStateMachine)
            return method;

        if (method.UnderlyingSymbol is MethodSymbol underlying && !ReferenceEquals(underlying, method))
            method = underlying;

        if (!RequiresSubstitution(method, type => SubstituteAsyncMethodTypeParameters(type)))
            return method;

        return new MappedMethodSymbol(this, method, TypeParameterMappingKind.AsyncToStateMachine);
    }

    internal MethodSymbol SubstituteStateMachineTypeParameters(MethodSymbol method)
    {
        if (method is null)
            throw new ArgumentNullException(nameof(method));

        if (_stateToAsyncTypeParameterMap.Count == 0)
            return method;

        if (method is MappedMethodSymbol mapped && ReferenceEquals(mapped.Owner, this) && mapped.MappingKind == TypeParameterMappingKind.StateMachineToAsyncMethod)
            return method;

        if (method.UnderlyingSymbol is MethodSymbol underlying && !ReferenceEquals(underlying, method))
            method = underlying;

        if (!RequiresSubstitution(method, type => SubstituteStateMachineTypeParameters(type)))
            return method;

        return new MappedMethodSymbol(this, method, TypeParameterMappingKind.StateMachineToAsyncMethod);
    }

    internal PropertySymbol SubstituteAsyncMethodTypeParameters(PropertySymbol property)
    {
        if (property is null)
            throw new ArgumentNullException(nameof(property));

        if (_asyncToStateTypeParameterMap.Count == 0)
            return property;

        if (property is MappedPropertySymbol mapped && ReferenceEquals(mapped.Owner, this) && mapped.MappingKind == TypeParameterMappingKind.AsyncToStateMachine)
            return property;

        if (property.UnderlyingSymbol is PropertySymbol underlying && !ReferenceEquals(underlying, property))
            property = underlying;

        if (!TypeRequiresSubstitution(property.Type, type => SubstituteAsyncMethodTypeParameters(type)))
            return property;

        return new MappedPropertySymbol(this, property, TypeParameterMappingKind.AsyncToStateMachine);
    }

    internal PropertySymbol SubstituteStateMachineTypeParameters(PropertySymbol property)
    {
        if (property is null)
            throw new ArgumentNullException(nameof(property));

        if (_stateToAsyncTypeParameterMap.Count == 0)
            return property;

        if (property is MappedPropertySymbol mapped && ReferenceEquals(mapped.Owner, this) && mapped.MappingKind == TypeParameterMappingKind.StateMachineToAsyncMethod)
            return property;

        if (property.UnderlyingSymbol is PropertySymbol underlying && !ReferenceEquals(underlying, property))
            property = underlying;

        if (!TypeRequiresSubstitution(property.Type, type => SubstituteStateMachineTypeParameters(type)))
            return property;

        return new MappedPropertySymbol(this, property, TypeParameterMappingKind.StateMachineToAsyncMethod);
    }

    private bool RequiresSubstitution(MethodSymbol method, Func<TypeSymbol, TypeSymbol> substituteType)
    {
        if (TypeRequiresSubstitution(method.ReturnType, substituteType))
            return true;

        if (method.ContainingType is TypeSymbol containingType && TypeRequiresSubstitution(containingType, substituteType))
            return true;

        foreach (var parameter in method.Parameters)
        {
            if (TypeRequiresSubstitution(parameter.Type, substituteType))
                return true;
        }

        foreach (var argument in method.TypeArguments)
        {
            if (TypeRequiresSubstitution(argument, substituteType))
                return true;
        }

        return false;
    }

    private static bool TypeRequiresSubstitution(TypeSymbol? type, Func<TypeSymbol, TypeSymbol> substituteType)
    {
        if (type is null)
            return false;

        var substituted = substituteType(type);
        return !SymbolEqualityComparer.Default.Equals(substituted, type);
    }

    private TypeSymbol DetermineBuilderType(Compilation compilation, MethodSymbol asyncMethod)
    {
        var returnType = asyncMethod.ReturnType is NullableTypeSymbol nullable
            ? nullable.UnderlyingType
            : asyncMethod.ReturnType;

        if (returnType.SpecialType == SpecialType.System_Void)
            return compilation.GetSpecialType(SpecialType.System_Runtime_CompilerServices_AsyncVoidMethodBuilder);

        if (returnType.SpecialType == SpecialType.System_Threading_Tasks_Task)
            return compilation.GetSpecialType(SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder);

        if (IsNonGenericValueTask(returnType))
        {
            return compilation.GetTypeByMetadataName("System.Runtime.CompilerServices.AsyncValueTaskMethodBuilder")
                ?? compilation.GetSpecialType(SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder);
        }

        if (returnType is NamedTypeSymbol named &&
            named.TypeArguments.Length == 1 &&
            IsTaskOfT(named.OriginalDefinition as NamedTypeSymbol ?? named.ConstructedFrom as NamedTypeSymbol ?? named))
        {
            var awaitedType = SubstituteAsyncMethodTypeParameters(named.TypeArguments[0]);

            if (awaitedType.TypeKind == TypeKind.Error)
                return compilation.GetSpecialType(SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder);

            if (compilation.GetSpecialType(SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder_T)
                is NamedTypeSymbol builderDefinition &&
                builderDefinition.TypeKind != TypeKind.Error)
            {
                return builderDefinition.Construct(awaitedType);
            }
        }

        if (returnType is NamedTypeSymbol valueTaskNamed &&
            valueTaskNamed.TypeArguments.Length == 1 &&
            IsValueTaskOfT(valueTaskNamed.OriginalDefinition as NamedTypeSymbol ?? valueTaskNamed.ConstructedFrom as NamedTypeSymbol ?? valueTaskNamed))
        {
            var awaitedType = SubstituteAsyncMethodTypeParameters(valueTaskNamed.TypeArguments[0]);
            if (awaitedType.TypeKind == TypeKind.Error)
                return compilation.GetTypeByMetadataName("System.Runtime.CompilerServices.AsyncValueTaskMethodBuilder")
                    ?? compilation.GetSpecialType(SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder);

            var valueTaskBuilderDefinition = compilation.GetTypeByMetadataName("System.Runtime.CompilerServices.AsyncValueTaskMethodBuilder`1")
                as NamedTypeSymbol;
            if (valueTaskBuilderDefinition is not null &&
                valueTaskBuilderDefinition.TypeKind != TypeKind.Error)
            {
                return valueTaskBuilderDefinition.Construct(awaitedType);
            }
        }

        return compilation.GetSpecialType(SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder);
    }

    private static bool IsTaskOfT(NamedTypeSymbol definition)
    {
        if (definition.SpecialType == SpecialType.System_Threading_Tasks_Task_T)
            return true;

        return definition.MetadataName == "Task`1" &&
            definition.ContainingNamespace is
            {
                Name: "Tasks",
                ContainingNamespace:
                {
                    Name: "Threading",
                    ContainingNamespace:
                    {
                        Name: "System",
                        ContainingNamespace.IsGlobalNamespace: true
                    }
                }
            };
    }

    private static bool IsNonGenericValueTask(TypeSymbol type)
    {
        if (type is not NamedTypeSymbol named)
            return false;

        return named.MetadataName == "ValueTask" &&
            named.TypeArguments.IsDefaultOrEmpty &&
            named.ContainingNamespace is
            {
                Name: "Tasks",
                ContainingNamespace:
                {
                    Name: "Threading",
                    ContainingNamespace:
                    {
                        Name: "System",
                        ContainingNamespace.IsGlobalNamespace: true
                    }
                }
            };
    }

    private static bool IsValueTaskOfT(NamedTypeSymbol definition)
    {
        return definition.MetadataName == "ValueTask`1" &&
            definition.ContainingNamespace is
            {
                Name: "Tasks",
                ContainingNamespace:
                {
                    Name: "Threading",
                    ContainingNamespace:
                    {
                        Name: "System",
                        ContainingNamespace.IsGlobalNamespace: true
                    }
                }
            };
    }

    private SourceMethodSymbol CreateConstructor(Compilation compilation, MethodSymbol asyncMethod)
    {
        return new SourceMethodSymbol(
            ".ctor",
            compilation.GetSpecialType(SpecialType.System_Void),
            ConstructorFields.Select(field => new SourceParameterSymbol(field.Name, field.Type,
                this, this, asyncMethod.ContainingNamespace, s_emptyLocations, s_emptySyntax)).ToImmutableArray(),
            this,
            this,
            asyncMethod.ContainingNamespace,
            s_emptyLocations,
            s_emptySyntax,
            isStatic: false,
            methodKind: MethodKind.Constructor,
            declaredAccessibility: Accessibility.Public);
    }

    private SourceMethodSymbol CreateMoveNextMethod(Compilation compilation, MethodSymbol asyncMethod)
    {
        var voidType = compilation.GetSpecialType(SpecialType.System_Void);

        return new SourceMethodSymbol(
            "MoveNext",
            voidType,
            ImmutableArray<SourceParameterSymbol>.Empty,
            this,
            this,
            asyncMethod.ContainingNamespace,
            s_emptyLocations,
            s_emptySyntax,
            isStatic: false,
            methodKind: MethodKind.Ordinary,
            declaredAccessibility: Accessibility.Public);
    }

    private SourceMethodSymbol CreateSetStateMachineMethod(
        Compilation compilation,
        MethodSymbol asyncMethod,
        NamedTypeSymbol asyncStateMachineInterface)
    {
        var voidType = compilation.GetSpecialType(SpecialType.System_Void);
        var parameter = new SourceParameterSymbol(
            "stateMachine",
            asyncStateMachineInterface,
            this,
            this,
            asyncMethod.ContainingNamespace,
            s_emptyLocations,
            s_emptySyntax);

        return new SourceMethodSymbol(
            "SetStateMachine",
            voidType,
            ImmutableArray.Create(parameter),
            this,
            this,
            asyncMethod.ContainingNamespace,
            s_emptyLocations,
            s_emptySyntax,
            isStatic: false,
            methodKind: MethodKind.Ordinary,
            declaredAccessibility: Accessibility.Public);
    }

    private static MethodSymbol? FindParameterlessStaticMethod(NamedTypeSymbol type, string name)
    {
        foreach (var member in type.GetMembers(name))
        {
            if (member is MethodSymbol { Parameters.Length: 0, IsStatic: true } method)
                return method;
        }

        return null;
    }

    private static MethodSymbol? FindStartMethod(NamedTypeSymbol builderType)
    {
        foreach (var member in builderType.GetMembers("Start"))
        {
            if (member is MethodSymbol { Parameters.Length: 1 } method)
                return method;
        }

        return null;
    }

    private static MethodSymbol? FindSetStateMachineMethod(NamedTypeSymbol builderType)
    {
        foreach (var member in builderType.GetMembers("SetStateMachine"))
        {
            if (member is MethodSymbol { Parameters.Length: 1 } method)
                return method;
        }

        return null;
    }

    private static MethodSymbol? FindSetResultMethod(NamedTypeSymbol builderType)
    {
        foreach (var member in builderType.GetMembers("SetResult"))
        {
            if (member is MethodSymbol method)
                return method;
        }

        return null;
    }

    private static MethodSymbol? FindSetExceptionMethod(NamedTypeSymbol builderType)
    {
        foreach (var member in builderType.GetMembers("SetException"))
        {
            if (member is MethodSymbol { Parameters.Length: 1 } method)
                return method;
        }

        return null;
    }

    private static MethodSymbol? FindAwaitOnCompletedMethod(NamedTypeSymbol builderType)
    {
        return FindAwaitMethod(builderType, "AwaitUnsafeOnCompleted")
            ?? FindAwaitMethod(builderType, "AwaitOnCompleted");
    }

    private static MethodSymbol? FindAwaitMethod(NamedTypeSymbol builderType, string name)
    {
        foreach (var member in builderType.GetMembers(name))
        {
            if (member is MethodSymbol { Parameters.Length: 2 } method)
                return method;
        }

        return null;
    }

    private static PropertySymbol? FindTaskProperty(NamedTypeSymbol builderType)
    {
        foreach (var member in builderType.GetMembers())
        {
            if (member is PropertySymbol { Name: "Task" } property)
                return property;
        }

        return null;
    }

    internal readonly struct ConstructedMembers
    {
        public ConstructedMembers(
            NamedTypeSymbol stateMachineType,
            MethodSymbol constructor,
            MethodSymbol moveNext,
            FieldSymbol stateField,
            FieldSymbol builderField,
            FieldSymbol? thisField,
            ImmutableDictionary<ParameterSymbol, FieldSymbol> parameterFields,
            BuilderMembers stateMachineBuilderMembers,
            BuilderMembers asyncMethodBuilderMembers)
        {
            StateMachineType = stateMachineType;
            Constructor = constructor;
            MoveNext = moveNext;
            StateField = stateField;
            BuilderField = builderField;
            ThisField = thisField;
            ParameterFields = parameterFields;
            StateMachineBuilderMembers = stateMachineBuilderMembers;
            AsyncMethodBuilderMembers = asyncMethodBuilderMembers;
        }

        public NamedTypeSymbol StateMachineType { get; }
        public MethodSymbol Constructor { get; }
        public MethodSymbol MoveNext { get; }
        public FieldSymbol StateField { get; }
        public FieldSymbol BuilderField { get; }
        public FieldSymbol? ThisField { get; }
        public ImmutableDictionary<ParameterSymbol, FieldSymbol> ParameterFields { get; }
        public BuilderMembers StateMachineBuilderMembers { get; }
        public BuilderMembers AsyncMethodBuilderMembers { get; }
    }

    internal readonly struct BuilderMembers
    {
        public BuilderMembers(
            FieldSymbol builderField,
            MethodSymbol? create,
            MethodSymbol? start,
            MethodSymbol? setStateMachine,
            MethodSymbol? setResult,
            MethodSymbol? setException,
            MethodSymbol? awaitOnCompleted,
            PropertySymbol? taskProperty)
        {
            BuilderField = builderField;
            Create = create;
            Start = start;
            SetStateMachine = setStateMachine;
            SetResult = setResult;
            SetException = setException;
            AwaitOnCompleted = awaitOnCompleted;
            TaskProperty = taskProperty;
        }

        public FieldSymbol BuilderField { get; }
        public MethodSymbol? Create { get; }
        public MethodSymbol? Start { get; }
        public MethodSymbol? SetStateMachine { get; }
        public MethodSymbol? SetResult { get; }
        public MethodSymbol? SetException { get; }
        public MethodSymbol? AwaitOnCompleted { get; }
        public PropertySymbol? TaskProperty { get; }
    }

    private sealed class AsyncBuilderMemberMap
    {
        private readonly SynthesizedAsyncStateMachineTypeSymbol _owner;
        private readonly Dictionary<FieldSymbol, BuilderMembers> _stateMachineCache;
        private readonly Dictionary<FieldSymbol, BuilderMembers> _asyncMethodCache;

        public AsyncBuilderMemberMap(
            SynthesizedAsyncStateMachineTypeSymbol owner,
            BuilderMembers stateMachineMembers)
        {
            _owner = owner ?? throw new ArgumentNullException(nameof(owner));
            _stateMachineCache = new Dictionary<FieldSymbol, BuilderMembers>(SymbolEqualityComparer.Default)
            {
                [stateMachineMembers.BuilderField] = stateMachineMembers
            };
            _asyncMethodCache = new Dictionary<FieldSymbol, BuilderMembers>(SymbolEqualityComparer.Default);
        }

        public BuilderMembers StateMachineMembers
            => _stateMachineCache[_owner.BuilderField];

        public BuilderMembers GetStateMachineMembers(FieldSymbol builderField)
        {
            if (builderField is null)
                throw new ArgumentNullException(nameof(builderField));

            if (_stateMachineCache.TryGetValue(builderField, out var members))
                return members;

            members = _owner.CreateBuilderMembers(builderField);
            _stateMachineCache.Add(builderField, members);
            return members;
        }

        public BuilderMembers GetAsyncMethodMembers(FieldSymbol builderField)
        {
            if (builderField is null)
                throw new ArgumentNullException(nameof(builderField));

            if (_asyncMethodCache.TryGetValue(builderField, out var members))
                return members;

            members = _owner.CreateAsyncMethodBuilderMembers(builderField, GetStateMachineMembers(builderField));
            _asyncMethodCache.Add(builderField, members);
            return members;
        }
    }

    private enum TypeParameterMappingKind
    {
        AsyncToStateMachine,
        StateMachineToAsyncMethod
    }

    private sealed class MappedMethodSymbol : MethodSymbol
    {
        private readonly SynthesizedAsyncStateMachineTypeSymbol _owner;
        private readonly MethodSymbol _original;
        private readonly TypeParameterMappingKind _kind;
        private readonly Func<TypeSymbol, TypeSymbol> _typeSubstitution;
        private readonly Func<MethodSymbol, MethodSymbol> _methodSubstitution;
        private ImmutableArray<ParameterSymbol>? _parameters;
        private ImmutableArray<MethodSymbol>? _explicitInterfaceImplementations;
        private ImmutableArray<TypeSymbol>? _typeArguments;

        public MappedMethodSymbol(
            SynthesizedAsyncStateMachineTypeSymbol owner,
            MethodSymbol original,
            TypeParameterMappingKind kind)
        {
            _owner = owner ?? throw new ArgumentNullException(nameof(owner));
            _original = original ?? throw new ArgumentNullException(nameof(original));
            _kind = kind;
            _typeSubstitution = kind switch
            {
                TypeParameterMappingKind.AsyncToStateMachine => type => owner.SubstituteAsyncMethodTypeParameters(type),
                TypeParameterMappingKind.StateMachineToAsyncMethod => type => owner.SubstituteStateMachineTypeParameters(type),
                _ => throw new ArgumentOutOfRangeException(nameof(kind))
            };
            _methodSubstitution = kind switch
            {
                TypeParameterMappingKind.AsyncToStateMachine => method => owner.SubstituteAsyncMethodTypeParameters(method),
                TypeParameterMappingKind.StateMachineToAsyncMethod => method => owner.SubstituteStateMachineTypeParameters(method),
                _ => throw new ArgumentOutOfRangeException(nameof(kind))
            };
        }

        public SynthesizedAsyncStateMachineTypeSymbol Owner => _owner;

        public TypeParameterMappingKind MappingKind => _kind;

        public string Name => _original.Name;

        public TypeSymbol ReturnType => _typeSubstitution(_original.ReturnType);

        public ImmutableArray<ParameterSymbol> Parameters =>
            _parameters ??= _original.Parameters
                .Select(parameter => (ParameterSymbol)new MappedParameterSymbol(parameter, _typeSubstitution, this))
                .ToImmutableArray();

        public ImmutableArray<AttributeData> GetReturnTypeAttributes()
            => _original.GetReturnTypeAttributes();

        public MethodKind MethodKind => _original.MethodKind;

        public bool IsConstructor => _original.IsConstructor;

        public MethodSymbol? OriginalDefinition => _original.OriginalDefinition;

        public bool IsAbstract => _original.IsAbstract;

        public bool IsAsync => _original.IsAsync;

        public bool IsCheckedBuiltin => _original.IsCheckedBuiltin;

        public bool IsDefinition => _original.IsDefinition;

        public bool IsExtensionMethod => _original.IsExtensionMethod;

        public bool IsExtern => _original.IsExtern;

        public bool IsUnsafe => _original.IsUnsafe;

        public bool IsGenericMethod => _original.IsGenericMethod;

        public bool IsOverride => _original.IsOverride;

        public bool IsReadOnly => _original.IsReadOnly;

        public bool IsFinal => _original.IsFinal;

        public bool IsVirtual => _original.IsVirtual;

        public bool IsIterator => _original.IsIterator;

        public IteratorMethodKind IteratorKind => _original.IteratorKind;

        public TypeSymbol? IteratorElementType
            => _original.IteratorElementType is { } iteratorType
                ? _typeSubstitution(iteratorType)
                : null;

        public ImmutableArray<MethodSymbol> ExplicitInterfaceImplementations =>
            _explicitInterfaceImplementations ??=
                _original.ExplicitInterfaceImplementations
                    .Select(method => _methodSubstitution(method))
                    .ToImmutableArray();

        public ImmutableArray<TypeParameterSymbol> TypeParameters => _original.TypeParameters;

        public ImmutableArray<TypeSymbol> TypeArguments =>
            _typeArguments ??=
                _original.TypeArguments
                    .Select(argument => _typeSubstitution(argument))
                    .ToImmutableArray();

        public MethodSymbol? ConstructedFrom
        {
            get
            {
                var constructedFrom = _original.ConstructedFrom;
                if (constructedFrom is null || ReferenceEquals(constructedFrom, _original))
                    return constructedFrom;

                return _methodSubstitution(constructedFrom);
            }
        }

        public SymbolKind Kind => _original.Kind;

        public string MetadataName => _original.MetadataName;

        public Symbol? ContainingSymbol
            => _original.ContainingSymbol is NamedTypeSymbol containingType
                ? _typeSubstitution(containingType) as Symbol
                : _original.ContainingSymbol;

        public AssemblySymbol? ContainingAssembly => _original.ContainingAssembly;

        public ModuleSymbol? ContainingModule => _original.ContainingModule;

        public NamedTypeSymbol? ContainingType
            => _original.ContainingType is null
                ? null
                : _typeSubstitution(_original.ContainingType) as NamedTypeSymbol;

        public NamespaceSymbol? ContainingNamespace => _original.ContainingNamespace;

        public ImmutableArray<Location> Locations => _original.Locations;

        public Accessibility DeclaredAccessibility => _original.DeclaredAccessibility;

        public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _original.DeclaringSyntaxReferences;

        public bool IsImplicitlyDeclared => _original.IsImplicitlyDeclared;

        public bool IsStatic => _original.IsStatic;

        public Symbol UnderlyingSymbol => _original;

        public bool IsAlias => false;

        public bool SetsRequiredMembers => false;

        public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();

        public void Accept(SymbolVisitor visitor)
        {
            visitor.VisitMethod(this);
        }

        public TResult Accept<TResult>(SymbolVisitor<TResult> visitor)
        {
            return visitor.VisitMethod(this);
        }

        public bool Equals(Symbol? other, SymbolEqualityComparer comparer)
            => comparer.Equals(this, other);

        public bool Equals(Symbol? other)
            => SymbolEqualityComparer.Default.Equals(this, other);

        public MethodSymbol Construct(params TypeSymbol[] typeArguments)
        {
            if (typeArguments is null)
                throw new ArgumentNullException(nameof(typeArguments));

            if (typeArguments.Length == 0)
            {
                var constructedWithoutArguments = _original.Construct(typeArguments);
                return _methodSubstitution(constructedWithoutArguments);
            }

            var substitutedArguments = new TypeSymbol[typeArguments.Length];
            var changed = false;

            for (var i = 0; i < typeArguments.Length; i++)
            {
                var substituted = _typeSubstitution(typeArguments[i]);
                substitutedArguments[i] = substituted;
                changed |= !SymbolEqualityComparer.Default.Equals(substituted, typeArguments[i]);
            }

            var argumentsToUse = changed ? substitutedArguments : typeArguments;
            var constructed = _original.Construct(argumentsToUse);
            return _methodSubstitution(constructed);
        }
    }

    private sealed class MappedParameterSymbol : ParameterSymbol
    {
        private readonly ParameterSymbol _original;
        private readonly Func<TypeSymbol, TypeSymbol> _typeSubstitution;
        private readonly Symbol _containingSymbol;

        public MappedParameterSymbol(
            ParameterSymbol original,
            Func<TypeSymbol, TypeSymbol> typeSubstitution,
            Symbol containingSymbol)
        {
            _original = original ?? throw new ArgumentNullException(nameof(original));
            _typeSubstitution = typeSubstitution ?? throw new ArgumentNullException(nameof(typeSubstitution));
            _containingSymbol = containingSymbol ?? throw new ArgumentNullException(nameof(containingSymbol));
        }

        public string Name => _original.Name;

        public TypeSymbol Type => _typeSubstitution(_original.Type);

        public bool HasImplicitName => _original.HasImplicitName;

        public Syntax.PatternSyntax? BindingPattern => _original.BindingPattern;

        public SymbolKind Kind => _original.Kind;

        public string MetadataName => _original.MetadataName;

        public Symbol? ContainingSymbol => _containingSymbol;

        public AssemblySymbol? ContainingAssembly => _original.ContainingAssembly;

        public ModuleSymbol? ContainingModule => _original.ContainingModule;

        public NamedTypeSymbol? ContainingType => _containingSymbol switch
        {
            NamedTypeSymbol namedType => namedType,
            MethodSymbol methodSymbol => methodSymbol.ContainingType,
            _ => _original.ContainingType
        };

        public NamespaceSymbol? ContainingNamespace => _original.ContainingNamespace;

        public ImmutableArray<Location> Locations => _original.Locations;

        public Accessibility DeclaredAccessibility => _original.DeclaredAccessibility;

        public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _original.DeclaringSyntaxReferences;

        public bool IsImplicitlyDeclared => _original.IsImplicitlyDeclared;

        public bool IsStatic => false;

        public Symbol UnderlyingSymbol => _original;

        public bool IsAlias => false;

        public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();

        public bool IsVarParams => _original.IsVarParams;

        public RefKind RefKind => _original.RefKind;

        public bool IsMutable => _original.IsMutable;

        public bool HasExplicitDefaultValue => _original.HasExplicitDefaultValue;

        public object? ExplicitDefaultValue => _original.ExplicitDefaultValue;

        public void Accept(SymbolVisitor visitor)
        {
            visitor.VisitParameter(this);
        }

        public TResult Accept<TResult>(SymbolVisitor<TResult> visitor)
        {
            return visitor.VisitParameter(this);
        }

        public bool Equals(Symbol? other, SymbolEqualityComparer comparer)
            => comparer.Equals(this, other);

        public bool Equals(Symbol? other)
            => SymbolEqualityComparer.Default.Equals(this, other);
    }

    private sealed class MappedPropertySymbol : PropertySymbol
    {
        private readonly SynthesizedAsyncStateMachineTypeSymbol _owner;
        private readonly PropertySymbol _original;
        private readonly TypeParameterMappingKind _kind;
        private readonly Func<TypeSymbol, TypeSymbol> _typeSubstitution;
        private readonly Func<MethodSymbol, MethodSymbol> _methodSubstitution;

        public MappedPropertySymbol(
            SynthesizedAsyncStateMachineTypeSymbol owner,
            PropertySymbol original,
            TypeParameterMappingKind kind)
        {
            _owner = owner ?? throw new ArgumentNullException(nameof(owner));
            _original = original ?? throw new ArgumentNullException(nameof(original));
            _kind = kind;
            _typeSubstitution = kind switch
            {
                TypeParameterMappingKind.AsyncToStateMachine => type => owner.SubstituteAsyncMethodTypeParameters(type),
                TypeParameterMappingKind.StateMachineToAsyncMethod => type => owner.SubstituteStateMachineTypeParameters(type),
                _ => throw new ArgumentOutOfRangeException(nameof(kind))
            };
            _methodSubstitution = kind switch
            {
                TypeParameterMappingKind.AsyncToStateMachine => method => owner.SubstituteAsyncMethodTypeParameters(method),
                TypeParameterMappingKind.StateMachineToAsyncMethod => method => owner.SubstituteStateMachineTypeParameters(method),
                _ => throw new ArgumentOutOfRangeException(nameof(kind))
            };
        }

        public SynthesizedAsyncStateMachineTypeSymbol Owner => _owner;

        public TypeParameterMappingKind MappingKind => _kind;

        public string Name => _original.Name;

        public TypeSymbol Type => _typeSubstitution(_original.Type);

        public MethodSymbol? GetMethod
            => _original.GetMethod is null ? null : _methodSubstitution(_original.GetMethod);

        public MethodSymbol? SetMethod
            => _original.SetMethod is null ? null : _methodSubstitution(_original.SetMethod);

        public bool IsIndexer => _original.IsIndexer;

        public SymbolKind Kind => _original.Kind;

        public string MetadataName => _original.MetadataName;

        public Symbol? ContainingSymbol
            => _original.ContainingSymbol is NamedTypeSymbol containingType
                ? _typeSubstitution(containingType) as Symbol
                : _original.ContainingSymbol;

        public AssemblySymbol? ContainingAssembly => _original.ContainingAssembly;

        public ModuleSymbol? ContainingModule => _original.ContainingModule;

        public NamedTypeSymbol? ContainingType
            => _original.ContainingType is null
                ? null
                : _typeSubstitution(_original.ContainingType) as NamedTypeSymbol;

        public NamespaceSymbol? ContainingNamespace => _original.ContainingNamespace;

        public ImmutableArray<Location> Locations => _original.Locations;

        public Accessibility DeclaredAccessibility => _original.DeclaredAccessibility;

        public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _original.DeclaringSyntaxReferences;

        public bool IsImplicitlyDeclared => _original.IsImplicitlyDeclared;

        public bool IsStatic => _original.IsStatic;

        public Symbol UnderlyingSymbol => _original;

        public bool IsAlias => false;

        public bool IsRequired => _original.IsRequired;

        public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();

        public void Accept(SymbolVisitor visitor) => visitor.VisitProperty(this);

        public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitProperty(this);

        public bool Equals(Symbol? other, SymbolEqualityComparer comparer)
            => comparer.Equals(this, other);

        public bool Equals(Symbol? other)
            => SymbolEqualityComparer.Default.Equals(this, other);
    }

    internal readonly struct TypeParameterMapping
    {
        public TypeParameterMapping(TypeParameterSymbol asyncParameter, TypeParameterSymbol stateMachineParameter)
        {
            AsyncParameter = asyncParameter ?? throw new ArgumentNullException(nameof(asyncParameter));
            StateMachineParameter = stateMachineParameter ?? throw new ArgumentNullException(nameof(stateMachineParameter));
        }

        public TypeParameterSymbol AsyncParameter { get; }

        public TypeParameterSymbol StateMachineParameter { get; }
    }
}
