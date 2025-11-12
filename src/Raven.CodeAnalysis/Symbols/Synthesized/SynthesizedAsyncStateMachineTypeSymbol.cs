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

    private ImmutableArray<SourceFieldSymbol> _hoistedLocals;
    private ImmutableArray<SourceFieldSymbol> _hoistedLocalsToDispose;
    private readonly ImmutableDictionary<IParameterSymbol, SourceFieldSymbol> _parameterFieldMap;
    private readonly ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol> _asyncToStateTypeParameterMap;
    private readonly ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol> _stateToAsyncTypeParameterMap;
    private readonly ImmutableArray<TypeParameterMapping> _typeParameterMappings;
    private readonly AsyncBuilderMemberMap _builderMemberMap;
    private readonly Dictionary<SourceMethodSymbol, ConstructedMembers> _constructedMembersCache = new(ReferenceEqualityComparer.Instance);

    public SynthesizedAsyncStateMachineTypeSymbol(
        Compilation compilation,
        SourceMethodSymbol asyncMethod,
        string name)
        : base(
            name,
            compilation.GetSpecialType(SpecialType.System_ValueType),
            TypeKind.Struct,
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
        AsyncMethod = asyncMethod;

        (
            _asyncToStateTypeParameterMap,
            _stateToAsyncTypeParameterMap,
            _typeParameterMappings) = InitializeTypeParameters(asyncMethod);

        StateField = CreateField("_state", compilation.GetSpecialType(SpecialType.System_Int32));

        if (!asyncMethod.IsStatic)
            ThisField = CreateField("_this", asyncMethod.ContainingType ?? compilation.GetSpecialType(SpecialType.System_Object));

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

    public SourceMethodSymbol AsyncMethod { get; }

    public SourceFieldSymbol StateField { get; }

    public SourceFieldSymbol? ThisField { get; }

    public ImmutableArray<SourceFieldSymbol> ParameterFields { get; }

    public ImmutableDictionary<IParameterSymbol, SourceFieldSymbol> ParameterFieldMap => _parameterFieldMap;
    internal ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol> AsyncMethodTypeParameterMap => _asyncToStateTypeParameterMap;

    internal ImmutableArray<TypeParameterMapping> TypeParameterMappings => _typeParameterMappings;

    internal bool TryMapToStateMachineTypeParameter(ITypeParameterSymbol asyncParameter, out ITypeParameterSymbol mapped)
        => _asyncToStateTypeParameterMap.TryGetValue(asyncParameter, out mapped);

    internal bool TryMapToAsyncMethodTypeParameter(ITypeParameterSymbol stateMachineParameter, out ITypeParameterSymbol mapped)
        => _stateToAsyncTypeParameterMap.TryGetValue(stateMachineParameter, out mapped);

    public ImmutableArray<SourceFieldSymbol> HoistedLocals => _hoistedLocals;

    public ImmutableArray<SourceFieldSymbol> HoistedLocalsToDispose => _hoistedLocalsToDispose;

    public SourceFieldSymbol BuilderField { get; }

    public SourceMethodSymbol Constructor { get; }

    public SourceMethodSymbol MoveNextMethod { get; }

    public SourceMethodSymbol SetStateMachineMethod { get; }

    public BoundBlockStatement? OriginalBody { get; private set; }

    public BoundBlockStatement? MoveNextBody { get; private set; }

    public BoundBlockStatement? SetStateMachineBody { get; private set; }

    public ConstructedMembers GetConstructedMembers(SourceMethodSymbol method)
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

    public BuilderMembers GetBuilderMembers(SourceMethodSymbol method)
    {
        return _builderMemberMap.StateMachineMembers;
    }

    private ConstructedMembers CreateConstructedMembers(SourceMethodSymbol method)
    {
        var stateMachineType = (INamedTypeSymbol)GetConstructedStateMachine(method);
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

    public INamedTypeSymbol GetConstructedStateMachine(SourceMethodSymbol method)
    {
        if (method is null)
            throw new ArgumentNullException(nameof(method));

        if (!ReferenceEquals(method, AsyncMethod))
            throw new ArgumentException("State machine constructed for different method.", nameof(method));

        if (TypeParameters.Length == 0)
            return this;

        var typeArguments = method.TypeArguments;
        if (typeArguments.Length != TypeParameters.Length)
            return this;

        return new ConstructedNamedTypeSymbol(this, typeArguments);
    }

    public SourceFieldSymbol AddHoistedLocal(string name, ITypeSymbol type, bool requiresDispose = false)
    {
        if (string.IsNullOrEmpty(name))
            throw new ArgumentException("Field name cannot be null or empty.", nameof(name));
        if (type is null)
            throw new ArgumentNullException(nameof(type));

        var field = CreateField(name, type);
        _hoistedLocals = _hoistedLocals.Add(field);
        if (requiresDispose)
            _hoistedLocalsToDispose = _hoistedLocalsToDispose.Add(field);
        return field;
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

    private ImmutableArray<SourceFieldSymbol> CreateParameterFields(SourceMethodSymbol method, out ImmutableDictionary<IParameterSymbol, SourceFieldSymbol> parameterFieldMap)
    {
        if (method.Parameters.Length == 0)
        {
            parameterFieldMap = ImmutableDictionary<IParameterSymbol, SourceFieldSymbol>.Empty;
            return ImmutableArray<SourceFieldSymbol>.Empty;
        }

        var orderedFields = ImmutableArray.CreateBuilder<SourceFieldSymbol>(method.Parameters.Length);
        var mapBuilder = ImmutableDictionary.CreateBuilder<IParameterSymbol, SourceFieldSymbol>(SymbolEqualityComparer.Default);

        foreach (var parameter in method.Parameters)
        {
            var field = CreateField($"_{parameter.Name}", parameter.Type);
            orderedFields.Add(field);
            mapBuilder.Add(parameter, field);
        }

        parameterFieldMap = mapBuilder.ToImmutable();
        return orderedFields.ToImmutable();
    }

    private SourceFieldSymbol CreateField(string name, ITypeSymbol type)
    {
        var substitutedType = SubstituteAsyncMethodTypeParameters(type);

        var field = new SourceFieldSymbol(
            name,
            substitutedType,
            isStatic: false,
            isLiteral: false,
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

    private SourceFieldSymbol CreateBuilderField(Compilation compilation, SourceMethodSymbol asyncMethod)
    {
        var builderType = DetermineBuilderType(compilation, asyncMethod);
        return CreateField("_builder", builderType);
    }

    private IFieldSymbol GetConstructedField(SourceFieldSymbol field, INamedTypeSymbol stateMachineType)
    {
        if (ReferenceEquals(stateMachineType, this))
            return field;

        return stateMachineType
            .GetMembers(field.Name)
            .OfType<IFieldSymbol>()
            .First();
    }

    private IMethodSymbol GetConstructedMethod(SourceMethodSymbol method, INamedTypeSymbol stateMachineType)
    {
        if (ReferenceEquals(stateMachineType, this))
            return method;

        foreach (var candidate in stateMachineType.GetMembers(method.Name).OfType<IMethodSymbol>())
        {
            var original = candidate.OriginalDefinition ?? candidate;
            if (SymbolEqualityComparer.Default.Equals(original, method))
                return candidate;

            if (candidate.Parameters.Length == method.Parameters.Length && candidate.MethodKind == method.MethodKind)
                return candidate;
        }

        return method;
    }

    private ImmutableDictionary<IParameterSymbol, IFieldSymbol> ConstructParameterFieldMap(INamedTypeSymbol stateMachineType)
    {
        if (_parameterFieldMap.IsEmpty)
            return ImmutableDictionary<IParameterSymbol, IFieldSymbol>.Empty;

        var builder = ImmutableDictionary.CreateBuilder<IParameterSymbol, IFieldSymbol>(SymbolEqualityComparer.Default);

        foreach (var (parameter, field) in _parameterFieldMap)
        {
            var constructedField = GetConstructedField(field, stateMachineType);
            builder[parameter] = constructedField;
        }

        return builder.ToImmutable();
    }

    private BuilderMembers CreateBuilderMembers(IFieldSymbol builderField, INamedTypeSymbol? builderTypeOverride = null)
    {
        var builderType = builderTypeOverride ?? builderField.Type as INamedTypeSymbol;
        if (builderType is null)
            return new BuilderMembers(builderField, null, null, null, null, null, null, null);

        var create = FindParameterlessStaticMethod(builderType, "Create");
        var start = FindStartMethod(builderType);
        var setStateMachine = FindSetStateMachineMethod(builderType);
        var setResult = FindSetResultMethod(builderType);
        var setException = FindSetExceptionMethod(builderType);
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
        IFieldSymbol asyncBuilderField,
        BuilderMembers stateMachineMembers)
    {
        var asyncBuilderType = asyncBuilderField.Type as INamedTypeSymbol;
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

    private static IMethodSymbol? SubstituteBuilderMethodForAsyncMethod(
        IMethodSymbol? method,
        INamedTypeSymbol asyncBuilderType)
    {
        if (method is null)
            return null;

        if (SymbolEqualityComparer.Default.Equals(method.ContainingType, asyncBuilderType))
            return method;

        var targetDefinition = method.OriginalDefinition ?? method;

        foreach (var candidate in asyncBuilderType.GetMembers(method.Name).OfType<IMethodSymbol>())
        {
            var candidateDefinition = candidate.OriginalDefinition ?? candidate;
            if (SymbolEqualityComparer.Default.Equals(candidateDefinition, targetDefinition))
                return candidate;
        }

        return method;
    }

    private static IPropertySymbol? SubstituteBuilderPropertyForAsyncMethod(
        IPropertySymbol? property,
        INamedTypeSymbol asyncBuilderType)
    {
        if (property is null)
            return null;

        if (SymbolEqualityComparer.Default.Equals(property.ContainingType, asyncBuilderType))
            return property;

        foreach (var candidate in asyncBuilderType.GetMembers(property.Name).OfType<IPropertySymbol>())
            return candidate;

        return property;
    }

    private (
        ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol> AsyncToState,
        ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol> StateToAsync,
        ImmutableArray<TypeParameterMapping> Mappings) InitializeTypeParameters(SourceMethodSymbol asyncMethod)
    {
        if (asyncMethod.TypeParameters.IsDefaultOrEmpty || asyncMethod.TypeParameters.Length == 0)
        {
            SetTypeParameters(ImmutableArray<SourceTypeParameterSymbol>.Empty);
            return (
                ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol>.Empty,
                ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol>.Empty,
                ImmutableArray<TypeParameterMapping>.Empty);
        }

        var builder = ImmutableArray.CreateBuilder<ITypeParameterSymbol>(asyncMethod.TypeParameters.Length);
        var asyncToStateBuilder = ImmutableDictionary.CreateBuilder<ITypeParameterSymbol, ITypeParameterSymbol>(SymbolEqualityComparer.Default);
        var stateToAsyncBuilder = ImmutableDictionary.CreateBuilder<ITypeParameterSymbol, ITypeParameterSymbol>(SymbolEqualityComparer.Default);
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

    private ImmutableArray<ITypeSymbol> SubstituteConstraintTypes(
        ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol> substitutionMap,
        ImmutableArray<ITypeSymbol> constraintTypes)
    {
        if (constraintTypes.IsDefaultOrEmpty)
            return constraintTypes;

        var builder = ImmutableArray.CreateBuilder<ITypeSymbol>(constraintTypes.Length);

        foreach (var constraintType in constraintTypes)
        {
            builder.Add(SubstituteAsyncMethodTypeParameters(constraintType, substitutionMap));
        }

        return builder.MoveToImmutable();
    }

    internal ITypeSymbol SubstituteAsyncMethodTypeParameters(
        ITypeSymbol type,
        ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol>? substitutionMap = null)
    {
        var map = substitutionMap ?? _asyncToStateTypeParameterMap;

        if (map.Count == 0)
            return type;

        return Substitute(type);

        ITypeSymbol Substitute(ITypeSymbol symbol)
        {
            if (symbol is ITypeParameterSymbol typeParameter &&
                map.TryGetValue(typeParameter, out var replacement))
            {
                return replacement;
            }

            if (symbol is ByRefTypeSymbol byRef)
            {
                var substitutedElement = Substitute(byRef.ElementType);
                if (!SymbolEqualityComparer.Default.Equals(substitutedElement, byRef.ElementType))
                    return new ByRefTypeSymbol(substitutedElement);

                return symbol;
            }

            if (symbol is IAddressTypeSymbol address)
            {
                var substitutedElement = Substitute(address.ReferencedType);
                if (!SymbolEqualityComparer.Default.Equals(substitutedElement, address.ReferencedType))
                    return new AddressTypeSymbol(substitutedElement);

                return symbol;
            }

            if (symbol is INamedTypeSymbol named && named.IsGenericType && !named.IsUnboundGenericType)
            {
                var typeArguments = named.TypeArguments;
                var substitutedArguments = new ITypeSymbol[typeArguments.Length];
                var changed = false;

                for (int i = 0; i < typeArguments.Length; i++)
                {
                    substitutedArguments[i] = Substitute(typeArguments[i]);
                    if (!SymbolEqualityComparer.Default.Equals(substitutedArguments[i], typeArguments[i]))
                        changed = true;
                }

                if (changed)
                {
                    var definition = named.ConstructedFrom as INamedTypeSymbol ?? named;
                    return definition.Construct(substitutedArguments);
                }

                return named;
            }

            if (symbol is IArrayTypeSymbol array)
            {
                var substitutedElement = Substitute(array.ElementType);
                if (!SymbolEqualityComparer.Default.Equals(substitutedElement, array.ElementType))
                    return Compilation.CreateArrayTypeSymbol(substitutedElement, array.Rank);

                return symbol;
            }

            if (symbol is IPointerTypeSymbol pointer)
            {
                var substitutedElement = Substitute(pointer.PointedAtType);
                if (!SymbolEqualityComparer.Default.Equals(substitutedElement, pointer.PointedAtType))
                    return Compilation.CreatePointerTypeSymbol(substitutedElement);

                return symbol;
            }

            return symbol;
        }
    }

    internal ITypeSymbol SubstituteStateMachineTypeParameters(ITypeSymbol type)
        => SubstituteAsyncMethodTypeParameters(type, _stateToAsyncTypeParameterMap);

    internal IMethodSymbol SubstituteAsyncMethodTypeParameters(IMethodSymbol method)
    {
        if (method is null)
            throw new ArgumentNullException(nameof(method));

        if (_asyncToStateTypeParameterMap.Count == 0)
            return method;

        if (method is MappedMethodSymbol mapped && ReferenceEquals(mapped.Owner, this) && mapped.MappingKind == TypeParameterMappingKind.AsyncToStateMachine)
            return method;

        if (method.UnderlyingSymbol is IMethodSymbol underlying && !ReferenceEquals(underlying, method))
            method = underlying;

        if (!RequiresSubstitution(method, type => SubstituteAsyncMethodTypeParameters(type)))
            return method;

        return new MappedMethodSymbol(this, method, TypeParameterMappingKind.AsyncToStateMachine);
    }

    internal IMethodSymbol SubstituteStateMachineTypeParameters(IMethodSymbol method)
    {
        if (method is null)
            throw new ArgumentNullException(nameof(method));

        if (_stateToAsyncTypeParameterMap.Count == 0)
            return method;

        if (method is MappedMethodSymbol mapped && ReferenceEquals(mapped.Owner, this) && mapped.MappingKind == TypeParameterMappingKind.StateMachineToAsyncMethod)
            return method;

        if (method.UnderlyingSymbol is IMethodSymbol underlying && !ReferenceEquals(underlying, method))
            method = underlying;

        if (!RequiresSubstitution(method, type => SubstituteStateMachineTypeParameters(type)))
            return method;

        return new MappedMethodSymbol(this, method, TypeParameterMappingKind.StateMachineToAsyncMethod);
    }

    internal IPropertySymbol SubstituteStateMachineTypeParameters(IPropertySymbol property)
    {
        if (property is null)
            throw new ArgumentNullException(nameof(property));

        if (_stateToAsyncTypeParameterMap.Count == 0)
            return property;

        if (property is MappedPropertySymbol mapped && ReferenceEquals(mapped.Owner, this) && mapped.MappingKind == TypeParameterMappingKind.StateMachineToAsyncMethod)
            return property;

        if (property.UnderlyingSymbol is IPropertySymbol underlying && !ReferenceEquals(underlying, property))
            property = underlying;

        if (!TypeRequiresSubstitution(property.Type, type => SubstituteStateMachineTypeParameters(type)))
            return property;

        return new MappedPropertySymbol(this, property, TypeParameterMappingKind.StateMachineToAsyncMethod);
    }

    private bool RequiresSubstitution(IMethodSymbol method, Func<ITypeSymbol, ITypeSymbol> substituteType)
    {
        if (TypeRequiresSubstitution(method.ReturnType, substituteType))
            return true;

        if (method.ContainingType is ITypeSymbol containingType && TypeRequiresSubstitution(containingType, substituteType))
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

    private static bool TypeRequiresSubstitution(ITypeSymbol? type, Func<ITypeSymbol, ITypeSymbol> substituteType)
    {
        if (type is null)
            return false;

        var substituted = substituteType(type);
        return !SymbolEqualityComparer.Default.Equals(substituted, type);
    }

    private ITypeSymbol DetermineBuilderType(Compilation compilation, SourceMethodSymbol asyncMethod)
    {
        var returnType = asyncMethod.ReturnType;

        if (asyncMethod is SynthesizedMainAsyncMethodSymbol { ReturnsInt: true } mainAsync)
        {
            var intType = mainAsync.ContainingAssembly?.GetTypeByMetadataName("System.Int32")
                ?? compilation.GetSpecialType(SpecialType.System_Int32);

            if (intType is ITypeSymbol { TypeKind: not TypeKind.Error } awaitedType &&
                compilation.GetSpecialType(SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder_T)
                    is INamedTypeSymbol builderDefinition &&
                builderDefinition.TypeKind != TypeKind.Error)
            {
                return builderDefinition.Construct(awaitedType);
            }
        }

        if (returnType.SpecialType == SpecialType.System_Void)
            return compilation.GetSpecialType(SpecialType.System_Runtime_CompilerServices_AsyncVoidMethodBuilder);

        if (returnType.SpecialType == SpecialType.System_Threading_Tasks_Task)
            return compilation.GetSpecialType(SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder);

        if (returnType is INamedTypeSymbol named &&
            named.TypeArguments.Length == 1 &&
            named.ConstructedFrom is INamedTypeSymbol constructed &&
            IsTaskOfT(constructed))
        {
            var awaitedType = SubstituteAsyncMethodTypeParameters(named.TypeArguments[0]);

            if (awaitedType.TypeKind == TypeKind.Error)
                return compilation.GetSpecialType(SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder);

            if (compilation.GetSpecialType(SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder_T)
                is INamedTypeSymbol builderDefinition &&
                builderDefinition.TypeKind != TypeKind.Error)
            {
                return builderDefinition.Construct(awaitedType);
            }
        }

        return compilation.GetSpecialType(SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder);
    }

    private static bool IsTaskOfT(INamedTypeSymbol definition)
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

    private SourceMethodSymbol CreateConstructor(Compilation compilation, SourceMethodSymbol asyncMethod)
    {
        return new SourceMethodSymbol(
            ".ctor",
            compilation.GetSpecialType(SpecialType.System_Void),
            ImmutableArray<SourceParameterSymbol>.Empty,
            this,
            this,
            asyncMethod.ContainingNamespace,
            s_emptyLocations,
            s_emptySyntax,
            isStatic: false,
            methodKind: MethodKind.Constructor,
            declaredAccessibility: Accessibility.Public);
    }

    private SourceMethodSymbol CreateMoveNextMethod(Compilation compilation, SourceMethodSymbol asyncMethod)
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
            declaredAccessibility: Accessibility.Internal);
    }

    private SourceMethodSymbol CreateSetStateMachineMethod(
        Compilation compilation,
        SourceMethodSymbol asyncMethod,
        INamedTypeSymbol asyncStateMachineInterface)
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
            declaredAccessibility: Accessibility.Internal);
    }

    private static IMethodSymbol? FindParameterlessStaticMethod(INamedTypeSymbol type, string name)
    {
        foreach (var member in type.GetMembers(name))
        {
            if (member is IMethodSymbol { Parameters.Length: 0, IsStatic: true } method)
                return method;
        }

        return null;
    }

    private static IMethodSymbol? FindStartMethod(INamedTypeSymbol builderType)
    {
        foreach (var member in builderType.GetMembers("Start"))
        {
            if (member is IMethodSymbol { Parameters.Length: 1 } method)
                return method;
        }

        return null;
    }

    private static IMethodSymbol? FindSetStateMachineMethod(INamedTypeSymbol builderType)
    {
        foreach (var member in builderType.GetMembers("SetStateMachine"))
        {
            if (member is IMethodSymbol { Parameters.Length: 1 } method)
                return method;
        }

        return null;
    }

    private static IMethodSymbol? FindSetResultMethod(INamedTypeSymbol builderType)
    {
        foreach (var member in builderType.GetMembers("SetResult"))
        {
            if (member is IMethodSymbol method)
                return method;
        }

        return null;
    }

    private static IMethodSymbol? FindSetExceptionMethod(INamedTypeSymbol builderType)
    {
        foreach (var member in builderType.GetMembers("SetException"))
        {
            if (member is IMethodSymbol { Parameters.Length: 1 } method)
                return method;
        }

        return null;
    }

    private static IMethodSymbol? FindAwaitOnCompletedMethod(INamedTypeSymbol builderType)
    {
        return FindAwaitMethod(builderType, "AwaitUnsafeOnCompleted")
            ?? FindAwaitMethod(builderType, "AwaitOnCompleted");
    }

    private static IMethodSymbol? FindAwaitMethod(INamedTypeSymbol builderType, string name)
    {
        foreach (var member in builderType.GetMembers(name))
        {
            if (member is IMethodSymbol { Parameters.Length: 2 } method)
                return method;
        }

        return null;
    }

    private static IPropertySymbol? FindTaskProperty(INamedTypeSymbol builderType)
    {
        foreach (var member in builderType.GetMembers())
        {
            if (member is IPropertySymbol { Name: "Task" } property)
                return property;
        }

        return null;
    }

    internal readonly struct ConstructedMembers
    {
        public ConstructedMembers(
            INamedTypeSymbol stateMachineType,
            IMethodSymbol constructor,
            IMethodSymbol moveNext,
            IFieldSymbol stateField,
            IFieldSymbol builderField,
            IFieldSymbol? thisField,
            ImmutableDictionary<IParameterSymbol, IFieldSymbol> parameterFields,
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

        public INamedTypeSymbol StateMachineType { get; }
        public IMethodSymbol Constructor { get; }
        public IMethodSymbol MoveNext { get; }
        public IFieldSymbol StateField { get; }
        public IFieldSymbol BuilderField { get; }
        public IFieldSymbol? ThisField { get; }
        public ImmutableDictionary<IParameterSymbol, IFieldSymbol> ParameterFields { get; }
        public BuilderMembers StateMachineBuilderMembers { get; }
        public BuilderMembers AsyncMethodBuilderMembers { get; }
    }

    internal readonly struct BuilderMembers
    {
        public BuilderMembers(
            IFieldSymbol builderField,
            IMethodSymbol? create,
            IMethodSymbol? start,
            IMethodSymbol? setStateMachine,
            IMethodSymbol? setResult,
            IMethodSymbol? setException,
            IMethodSymbol? awaitOnCompleted,
            IPropertySymbol? taskProperty)
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

        public IFieldSymbol BuilderField { get; }
        public IMethodSymbol? Create { get; }
        public IMethodSymbol? Start { get; }
        public IMethodSymbol? SetStateMachine { get; }
        public IMethodSymbol? SetResult { get; }
        public IMethodSymbol? SetException { get; }
        public IMethodSymbol? AwaitOnCompleted { get; }
        public IPropertySymbol? TaskProperty { get; }
    }

    private sealed class AsyncBuilderMemberMap
    {
        private readonly SynthesizedAsyncStateMachineTypeSymbol _owner;
        private readonly Dictionary<IFieldSymbol, BuilderMembers> _stateMachineCache;
        private readonly Dictionary<IFieldSymbol, BuilderMembers> _asyncMethodCache;

        public AsyncBuilderMemberMap(
            SynthesizedAsyncStateMachineTypeSymbol owner,
            BuilderMembers stateMachineMembers)
        {
            _owner = owner ?? throw new ArgumentNullException(nameof(owner));
            _stateMachineCache = new Dictionary<IFieldSymbol, BuilderMembers>(SymbolEqualityComparer.Default)
            {
                [stateMachineMembers.BuilderField] = stateMachineMembers
            };
            _asyncMethodCache = new Dictionary<IFieldSymbol, BuilderMembers>(SymbolEqualityComparer.Default);
        }

        public BuilderMembers StateMachineMembers
            => _stateMachineCache[_owner.BuilderField];

        public BuilderMembers GetStateMachineMembers(IFieldSymbol builderField)
        {
            if (builderField is null)
                throw new ArgumentNullException(nameof(builderField));

            if (_stateMachineCache.TryGetValue(builderField, out var members))
                return members;

            members = _owner.CreateBuilderMembers(builderField);
            _stateMachineCache.Add(builderField, members);
            return members;
        }

        public BuilderMembers GetAsyncMethodMembers(IFieldSymbol builderField)
        {
            if (builderField is null)
                throw new ArgumentNullException(nameof(builderField));

            if (_asyncMethodCache.TryGetValue(builderField, out var members))
                return members;

            members = _owner.CreateAsyncMethodBuilderMembers(builderField, StateMachineMembers);
            _asyncMethodCache.Add(builderField, members);
            return members;
        }
    }

    private enum TypeParameterMappingKind
    {
        AsyncToStateMachine,
        StateMachineToAsyncMethod
    }

    private sealed class MappedMethodSymbol : IMethodSymbol
    {
        private readonly SynthesizedAsyncStateMachineTypeSymbol _owner;
        private readonly IMethodSymbol _original;
        private readonly TypeParameterMappingKind _kind;
        private readonly Func<ITypeSymbol, ITypeSymbol> _typeSubstitution;
        private readonly Func<IMethodSymbol, IMethodSymbol> _methodSubstitution;
        private ImmutableArray<IParameterSymbol>? _parameters;
        private ImmutableArray<IMethodSymbol>? _explicitInterfaceImplementations;
        private ImmutableArray<ITypeSymbol>? _typeArguments;

        public MappedMethodSymbol(
            SynthesizedAsyncStateMachineTypeSymbol owner,
            IMethodSymbol original,
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

        public ITypeSymbol ReturnType => _typeSubstitution(_original.ReturnType);

        public ImmutableArray<IParameterSymbol> Parameters =>
            _parameters ??= _original.Parameters
                .Select(parameter => (IParameterSymbol)new MappedParameterSymbol(parameter, _typeSubstitution, this))
                .ToImmutableArray();

        public ImmutableArray<AttributeData> GetReturnTypeAttributes()
            => _original.GetReturnTypeAttributes();

        public MethodKind MethodKind => _original.MethodKind;

        public bool IsConstructor => _original.IsConstructor;

        public IMethodSymbol? OriginalDefinition => _original.OriginalDefinition;

        public bool IsAbstract => _original.IsAbstract;

        public bool IsAsync => _original.IsAsync;

        public bool IsCheckedBuiltin => _original.IsCheckedBuiltin;

        public bool IsDefinition => _original.IsDefinition;

        public bool IsExtensionMethod => _original.IsExtensionMethod;

        public bool IsExtern => _original.IsExtern;

        public bool IsGenericMethod => _original.IsGenericMethod;

        public bool IsOverride => _original.IsOverride;

        public bool IsReadOnly => _original.IsReadOnly;

        public bool IsSealed => _original.IsSealed;

        public bool IsVirtual => _original.IsVirtual;

        public bool IsIterator => _original.IsIterator;

        public IteratorMethodKind IteratorKind => _original.IteratorKind;

        public ITypeSymbol? IteratorElementType
            => _original.IteratorElementType is { } iteratorType
                ? _typeSubstitution(iteratorType)
                : null;

        public ImmutableArray<IMethodSymbol> ExplicitInterfaceImplementations =>
            _explicitInterfaceImplementations ??=
                _original.ExplicitInterfaceImplementations
                    .Select(method => _methodSubstitution(method))
                    .ToImmutableArray();

        public ImmutableArray<ITypeParameterSymbol> TypeParameters => _original.TypeParameters;

        public ImmutableArray<ITypeSymbol> TypeArguments =>
            _typeArguments ??=
                _original.TypeArguments
                    .Select(argument => _typeSubstitution(argument))
                    .ToImmutableArray();

        public IMethodSymbol? ConstructedFrom
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

        public ISymbol? ContainingSymbol
            => _original.ContainingSymbol is INamedTypeSymbol containingType
                ? _typeSubstitution(containingType) as ISymbol
                : _original.ContainingSymbol;

        public IAssemblySymbol? ContainingAssembly => _original.ContainingAssembly;

        public IModuleSymbol? ContainingModule => _original.ContainingModule;

        public INamedTypeSymbol? ContainingType
            => _original.ContainingType is null
                ? null
                : _typeSubstitution(_original.ContainingType) as INamedTypeSymbol;

        public INamespaceSymbol? ContainingNamespace => _original.ContainingNamespace;

        public ImmutableArray<Location> Locations => _original.Locations;

        public Accessibility DeclaredAccessibility => _original.DeclaredAccessibility;

        public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _original.DeclaringSyntaxReferences;

        public bool IsImplicitlyDeclared => _original.IsImplicitlyDeclared;

        public bool IsStatic => _original.IsStatic;

        public ISymbol UnderlyingSymbol => _original;

        public bool IsAlias => false;

        public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();

        public void Accept(SymbolVisitor visitor)
        {
            visitor.VisitMethod(this);
        }

        public TResult Accept<TResult>(SymbolVisitor<TResult> visitor)
        {
            return visitor.VisitMethod(this);
        }

        public bool Equals(ISymbol? other, SymbolEqualityComparer comparer)
            => comparer.Equals(this, other);

        public bool Equals(ISymbol? other)
            => SymbolEqualityComparer.Default.Equals(this, other);

        public IMethodSymbol Construct(params ITypeSymbol[] typeArguments)
        {
            if (typeArguments is null)
                throw new ArgumentNullException(nameof(typeArguments));

            if (typeArguments.Length == 0)
            {
                var constructedWithoutArguments = _original.Construct(typeArguments);
                return _methodSubstitution(constructedWithoutArguments);
            }

            var substitutedArguments = new ITypeSymbol[typeArguments.Length];
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

    private sealed class MappedParameterSymbol : IParameterSymbol
    {
        private readonly IParameterSymbol _original;
        private readonly Func<ITypeSymbol, ITypeSymbol> _typeSubstitution;
        private readonly ISymbol _containingSymbol;

        public MappedParameterSymbol(
            IParameterSymbol original,
            Func<ITypeSymbol, ITypeSymbol> typeSubstitution,
            ISymbol containingSymbol)
        {
            _original = original ?? throw new ArgumentNullException(nameof(original));
            _typeSubstitution = typeSubstitution ?? throw new ArgumentNullException(nameof(typeSubstitution));
            _containingSymbol = containingSymbol ?? throw new ArgumentNullException(nameof(containingSymbol));
        }

        public string Name => _original.Name;

        public ITypeSymbol Type => _typeSubstitution(_original.Type);

        public SymbolKind Kind => _original.Kind;

        public string MetadataName => _original.MetadataName;

        public ISymbol? ContainingSymbol => _containingSymbol;

        public IAssemblySymbol? ContainingAssembly => _original.ContainingAssembly;

        public IModuleSymbol? ContainingModule => _original.ContainingModule;

        public INamedTypeSymbol? ContainingType => _containingSymbol switch
        {
            INamedTypeSymbol namedType => namedType,
            IMethodSymbol methodSymbol => methodSymbol.ContainingType,
            _ => _original.ContainingType
        };

        public INamespaceSymbol? ContainingNamespace => _original.ContainingNamespace;

        public ImmutableArray<Location> Locations => _original.Locations;

        public Accessibility DeclaredAccessibility => _original.DeclaredAccessibility;

        public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _original.DeclaringSyntaxReferences;

        public bool IsImplicitlyDeclared => _original.IsImplicitlyDeclared;

        public bool IsStatic => false;

        public ISymbol UnderlyingSymbol => _original;

        public bool IsAlias => false;

        public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();

        public bool IsParams => _original.IsParams;

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

        public bool Equals(ISymbol? other, SymbolEqualityComparer comparer)
            => comparer.Equals(this, other);

        public bool Equals(ISymbol? other)
            => SymbolEqualityComparer.Default.Equals(this, other);
    }

    private sealed class MappedPropertySymbol : IPropertySymbol
    {
        private readonly SynthesizedAsyncStateMachineTypeSymbol _owner;
        private readonly IPropertySymbol _original;
        private readonly TypeParameterMappingKind _kind;
        private readonly Func<ITypeSymbol, ITypeSymbol> _typeSubstitution;
        private readonly Func<IMethodSymbol, IMethodSymbol> _methodSubstitution;

        public MappedPropertySymbol(
            SynthesizedAsyncStateMachineTypeSymbol owner,
            IPropertySymbol original,
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

        public ITypeSymbol Type => _typeSubstitution(_original.Type);

        public IMethodSymbol? GetMethod
            => _original.GetMethod is null ? null : _methodSubstitution(_original.GetMethod);

        public IMethodSymbol? SetMethod
            => _original.SetMethod is null ? null : _methodSubstitution(_original.SetMethod);

        public bool IsIndexer => _original.IsIndexer;

        public SymbolKind Kind => _original.Kind;

        public string MetadataName => _original.MetadataName;

        public ISymbol? ContainingSymbol
            => _original.ContainingSymbol is INamedTypeSymbol containingType
                ? _typeSubstitution(containingType) as ISymbol
                : _original.ContainingSymbol;

        public IAssemblySymbol? ContainingAssembly => _original.ContainingAssembly;

        public IModuleSymbol? ContainingModule => _original.ContainingModule;

        public INamedTypeSymbol? ContainingType
            => _original.ContainingType is null
                ? null
                : _typeSubstitution(_original.ContainingType) as INamedTypeSymbol;

        public INamespaceSymbol? ContainingNamespace => _original.ContainingNamespace;

        public ImmutableArray<Location> Locations => _original.Locations;

        public Accessibility DeclaredAccessibility => _original.DeclaredAccessibility;

        public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _original.DeclaringSyntaxReferences;

        public bool IsImplicitlyDeclared => _original.IsImplicitlyDeclared;

        public bool IsStatic => _original.IsStatic;

        public ISymbol UnderlyingSymbol => _original;

        public bool IsAlias => false;

        public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();

        public void Accept(SymbolVisitor visitor) => visitor.VisitProperty(this);

        public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitProperty(this);

        public bool Equals(ISymbol? other, SymbolEqualityComparer comparer)
            => comparer.Equals(this, other);

        public bool Equals(ISymbol? other)
            => SymbolEqualityComparer.Default.Equals(this, other);
    }

    internal readonly struct TypeParameterMapping
    {
        public TypeParameterMapping(ITypeParameterSymbol asyncParameter, ITypeParameterSymbol stateMachineParameter)
        {
            AsyncParameter = asyncParameter ?? throw new ArgumentNullException(nameof(asyncParameter));
            StateMachineParameter = stateMachineParameter ?? throw new ArgumentNullException(nameof(stateMachineParameter));
        }

        public ITypeParameterSymbol AsyncParameter { get; }

        public ITypeParameterSymbol StateMachineParameter { get; }
    }
}

