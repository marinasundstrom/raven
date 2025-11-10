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
    private ConstructedNamedTypeSymbol? _constructedFromAsyncMethod;
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
        return GetConstructedMembers(method).BuilderMembers;
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
        var builderMembers = CreateBuilderMembers(builderField);

        return new ConstructedMembers(
            stateMachineType,
            constructor,
            moveNext,
            stateField,
            builderField,
            thisField,
            parameterFields,
            builderMembers);
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

        if (!RequiresConstruction(typeArguments, method))
            return this;

        return _constructedFromAsyncMethod ??= new ConstructedNamedTypeSymbol(this, typeArguments);
    }

    private static bool RequiresConstruction(ImmutableArray<ITypeSymbol> typeArguments, SourceMethodSymbol method)
    {
        foreach (var argument in typeArguments)
        {
            if (argument is ITypeParameterSymbol typeParameter)
            {
                var containingSymbol = typeParameter.ContainingSymbol as IMethodSymbol;
                if (containingSymbol is not null && ReferenceEquals(containingSymbol, method))
                    continue;
            }

            return true;
        }

        return false;
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
            builder[parameter] = GetConstructedField(field, stateMachineType);

        return builder.ToImmutable();
    }

    private BuilderMembers CreateBuilderMembers(IFieldSymbol builderField)
    {
        if (builderField.Type is not INamedTypeSymbol builderType)
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

    private ITypeSymbol SubstituteAsyncMethodTypeParameters(
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

    private static ITypeSymbol DetermineBuilderType(Compilation compilation, SourceMethodSymbol asyncMethod)
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
            var awaitedType = named.TypeArguments[0];

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
            BuilderMembers builderMembers)
        {
            StateMachineType = stateMachineType;
            Constructor = constructor;
            MoveNext = moveNext;
            StateField = stateField;
            BuilderField = builderField;
            ThisField = thisField;
            ParameterFields = parameterFields;
            BuilderMembers = builderMembers;
        }

        public INamedTypeSymbol StateMachineType { get; }
        public IMethodSymbol Constructor { get; }
        public IMethodSymbol MoveNext { get; }
        public IFieldSymbol StateField { get; }
        public IFieldSymbol BuilderField { get; }
        public IFieldSymbol? ThisField { get; }
        public ImmutableDictionary<IParameterSymbol, IFieldSymbol> ParameterFields { get; }
        public BuilderMembers BuilderMembers { get; }
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
