using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Microsoft.CodeAnalysis;
using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class SynthesizedAsyncStateMachineTypeSymbol : SourceNamedTypeSymbol
{
    private static readonly Location[] s_emptyLocations = Array.Empty<Location>();
    private static readonly SyntaxReference[] s_emptySyntax = Array.Empty<SyntaxReference>();

    private ImmutableArray<SourceFieldSymbol> _hoistedLocals;
    private ImmutableArray<SourceFieldSymbol> _hoistedLocalsToDispose;
    private readonly ImmutableDictionary<IParameterSymbol, SourceFieldSymbol> _parameterFieldMap;
    private readonly ConstructedMethodSymbol? _asyncMethodTemplate;
    private Dictionary<ImmutableArray<ITypeSymbol>, ConstructedNamedTypeSymbol>? _constructedCache;
    private Dictionary<INamedTypeSymbol, AsyncBuilderMembers>? _builderMembersCache;

    public SynthesizedAsyncStateMachineTypeSymbol(
        Compilation compilation,
        SourceMethodSymbol asyncMethod,
        string name,
        ITypeSymbol builderType)
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

        _asyncMethodTemplate = InitializeTypeParameters(asyncMethod);

        StateField = CreateField("_state", compilation.GetSpecialType(SpecialType.System_Int32));

        if (!asyncMethod.IsStatic)
            ThisField = CreateField("_this", asyncMethod.ContainingType ?? compilation.GetSpecialType(SpecialType.System_Object));

        ParameterFields = CreateParameterFields(asyncMethod, out _parameterFieldMap);
        _hoistedLocals = ImmutableArray<SourceFieldSymbol>.Empty;
        _hoistedLocalsToDispose = ImmutableArray<SourceFieldSymbol>.Empty;

        var asyncStateMachineInterface = compilation.GetSpecialType(SpecialType.System_Runtime_CompilerServices_IAsyncStateMachine);
        SetInterfaces(new[] { asyncStateMachineInterface });

        BuilderType = builderType ?? throw new ArgumentNullException(nameof(builderType));
        BuilderField = CreateBuilderField(builderType);

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

    public ImmutableArray<SourceFieldSymbol> HoistedLocals => _hoistedLocals;

    public ImmutableArray<SourceFieldSymbol> HoistedLocalsToDispose => _hoistedLocalsToDispose;

    public ITypeSymbol BuilderType { get; }

    public SourceFieldSymbol BuilderField { get; }

    public SourceMethodSymbol Constructor { get; }

    public SourceMethodSymbol MoveNextMethod { get; }

    public SourceMethodSymbol SetStateMachineMethod { get; }

    public BoundBlockStatement? OriginalBody { get; private set; }

    public BoundBlockStatement? MoveNextBody { get; private set; }

    public BoundBlockStatement? SetStateMachineBody { get; private set; }

    public AsyncBuilderMembers GetBuilderMembers(INamedTypeSymbol builderType)
    {
        if (builderType is null)
            throw new ArgumentNullException(nameof(builderType));

        _builderMembersCache ??= new Dictionary<INamedTypeSymbol, AsyncBuilderMembers>(SymbolEqualityComparer.Default);

        if (_builderMembersCache.TryGetValue(builderType, out var cached))
            return cached;

        var members = CreateBuilderMembers(builderType);
        _builderMembersCache.Add(builderType, members);
        return members;
    }

    public INamedTypeSymbol GetConstructedStateMachine(ImmutableArray<ITypeSymbol> typeArguments)
    {
        if (typeArguments.IsDefaultOrEmpty || typeArguments.Length == 0 || TypeParameters.IsDefaultOrEmpty || TypeParameters.Length == 0)
            return this;

        if (typeArguments.Length != TypeParameters.Length)
            throw new ArgumentException("Type argument count must match state-machine arity.", nameof(typeArguments));

        _constructedCache ??= new Dictionary<ImmutableArray<ITypeSymbol>, ConstructedNamedTypeSymbol>(TypeArgumentsEqualityComparer.Instance);

        if (_constructedCache.TryGetValue(typeArguments, out var constructed))
            return constructed;

        var constructedType = (ConstructedNamedTypeSymbol)Construct(typeArguments.ToArray());
        _constructedCache.Add(typeArguments, constructedType);
        return constructedType;
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
        var substitutedType = SubstituteMethodType(type);

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

    private SourceFieldSymbol CreateBuilderField(ITypeSymbol builderType)
        => CreateField("_builder", builderType);

    private AsyncBuilderMembers CreateBuilderMembers(INamedTypeSymbol builderType)
    {
        var createMethod = FindParameterlessStaticMethod(builderType, "Create");
        var startMethod = FindInstanceMethod(builderType, "Start", parameterCount: 1);
        var setStateMachineMethod = FindInstanceMethod(builderType, "SetStateMachine", parameterCount: 1);
        var setResultMethod = FindSetResultMethod(builderType);
        var setExceptionMethod = FindSetExceptionMethod(builderType);
        var awaitMethod = FindAwaitMethod(builderType, "AwaitUnsafeOnCompleted")
            ?? FindAwaitMethod(builderType, "AwaitOnCompleted");
        var taskProperty = FindTaskProperty(builderType);

        return new AsyncBuilderMembers(
            createMethod,
            startMethod,
            setStateMachineMethod,
            setResultMethod,
            setExceptionMethod,
            awaitMethod,
            taskProperty);
    }

    private ConstructedMethodSymbol? InitializeTypeParameters(SourceMethodSymbol asyncMethod)
    {
        if (asyncMethod.TypeParameters.IsDefaultOrEmpty || asyncMethod.TypeParameters.Length == 0)
        {
            return null;
        }

        var builder = ImmutableArray.CreateBuilder<ITypeParameterSymbol>(asyncMethod.TypeParameters.Length);

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
        }

        var mappedParameters = builder.MoveToImmutable();
        SetTypeParameters(mappedParameters);

        var substitution = new ConstructedMethodSymbol(asyncMethod, TypeArguments);

        foreach (var typeParameter in asyncMethod.TypeParameters)
        {
            if (typeParameter is not SourceTypeParameterSymbol sourceParameter ||
                !sourceParameter.HasResolvedConstraintTypes)
            {
                continue;
            }

            if (mappedParameters[typeParameter.Ordinal] is SourceTypeParameterSymbol synthesized)
            {
                var substitutedConstraints = SubstituteConstraintTypes(substitution, sourceParameter.ConstraintTypes);
                synthesized.SetConstraintTypes(substitutedConstraints);
            }
        }

        return substitution;
    }

    private ImmutableArray<ITypeSymbol> SubstituteConstraintTypes(
        ConstructedMethodSymbol substitution,
        ImmutableArray<ITypeSymbol> constraintTypes)
    {
        if (constraintTypes.IsDefaultOrEmpty)
            return constraintTypes;

        var builder = ImmutableArray.CreateBuilder<ITypeSymbol>(constraintTypes.Length);

        foreach (var constraintType in constraintTypes)
        {
            builder.Add(SubstituteMethodType(constraintType, substitution));
        }

        return builder.MoveToImmutable();
    }

    private ITypeSymbol SubstituteMethodType(ITypeSymbol type) =>
        SubstituteMethodType(type, _asyncMethodTemplate);

    private static ITypeSymbol SubstituteMethodType(ITypeSymbol type, ConstructedMethodSymbol? substitution)
    {
        if (substitution is null)
            return type;

        return substitution.Substitute(type);
    }

    private static IMethodSymbol? FindParameterlessStaticMethod(INamedTypeSymbol type, string name)
    {
        foreach (var member in type.GetMembers(name))
        {
            if (member is IMethodSymbol { Parameters.Length: 0, IsStatic: true } method)
                return SubstituteBuilderMethodIfNeeded(type, method);
        }

        return null;
    }

    private static IMethodSymbol? FindInstanceMethod(INamedTypeSymbol type, string name, int parameterCount)
    {
        foreach (var member in type.GetMembers(name))
        {
            if (member is IMethodSymbol { Parameters.Length: var length, IsStatic: false } method && length == parameterCount)
                return SubstituteBuilderMethodIfNeeded(type, method);
        }

        return null;
    }

    private static IMethodSymbol? FindSetResultMethod(INamedTypeSymbol builderType)
    {
        foreach (var member in builderType.GetMembers("SetResult"))
        {
            if (member is IMethodSymbol method)
                return SubstituteBuilderMethodIfNeeded(builderType, method);
        }

        return null;
    }

    private static IMethodSymbol? FindSetExceptionMethod(INamedTypeSymbol builderType)
    {
        foreach (var member in builderType.GetMembers("SetException"))
        {
            if (member is IMethodSymbol { Parameters.Length: 1 } method)
                return SubstituteBuilderMethodIfNeeded(builderType, method);
        }

        return null;
    }

    private static IMethodSymbol? FindAwaitMethod(INamedTypeSymbol builderType, string name)
    {
        foreach (var member in builderType.GetMembers(name))
        {
            if (member is IMethodSymbol { Parameters.Length: 2 } method)
                return SubstituteBuilderMethodIfNeeded(builderType, method);
        }

        return null;
    }

    private static IPropertySymbol? FindTaskProperty(INamedTypeSymbol builderType)
    {
        foreach (var member in builderType.GetMembers())
        {
            if (member is IPropertySymbol { Name: "Task" } property)
                return SubstituteBuilderPropertyIfNeeded(builderType, property);
        }

        return null;
    }

    private static IMethodSymbol? SubstituteBuilderMethodIfNeeded(INamedTypeSymbol builderType, IMethodSymbol method)
    {
        if (builderType is ConstructedNamedTypeSymbol constructed &&
            method is not SubstitutedMethodSymbol &&
            !SymbolEqualityComparer.Default.Equals(method.ContainingType, constructed))
        {
            return new SubstitutedMethodSymbol(method, constructed);
        }

        return method;
    }

    private static IPropertySymbol? SubstituteBuilderPropertyIfNeeded(INamedTypeSymbol builderType, IPropertySymbol property)
    {
        if (builderType is ConstructedNamedTypeSymbol constructed &&
            property is not SubstitutedPropertySymbol &&
            !SymbolEqualityComparer.Default.Equals(property.ContainingType, constructed))
        {
            return new SubstitutedPropertySymbol(property, constructed);
        }

        return property;
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

    private sealed class TypeArgumentsEqualityComparer : IEqualityComparer<ImmutableArray<ITypeSymbol>>
    {
        public static readonly TypeArgumentsEqualityComparer Instance = new();

        public bool Equals(ImmutableArray<ITypeSymbol> x, ImmutableArray<ITypeSymbol> y)
        {
            if (x.Length != y.Length)
                return false;

            for (var i = 0; i < x.Length; i++)
            {
                if (!SymbolEqualityComparer.Default.Equals(x[i], y[i]))
                    return false;
            }

            return true;
        }

        public int GetHashCode(ImmutableArray<ITypeSymbol> obj)
        {
            var hash = obj.Length;
            for (var i = 0; i < obj.Length; i++)
                hash = HashCode.Combine(hash, SymbolEqualityComparer.Default.GetHashCode(obj[i]));

            return hash;
        }
    }

    internal sealed class AsyncBuilderMembers
    {
        public AsyncBuilderMembers(
            IMethodSymbol? createMethod,
            IMethodSymbol? startMethod,
            IMethodSymbol? setStateMachineMethod,
            IMethodSymbol? setResultMethod,
            IMethodSymbol? setExceptionMethod,
            IMethodSymbol? awaitOnCompletedMethod,
            IPropertySymbol? taskProperty)
        {
            CreateMethod = createMethod;
            StartMethod = startMethod;
            SetStateMachineMethod = setStateMachineMethod;
            SetResultMethod = setResultMethod;
            SetExceptionMethod = setExceptionMethod;
            AwaitOnCompletedMethod = awaitOnCompletedMethod;
            TaskProperty = taskProperty;
        }

        public IMethodSymbol? CreateMethod { get; }

        public IMethodSymbol? StartMethod { get; }

        public IMethodSymbol? SetStateMachineMethod { get; }

        public IMethodSymbol? SetResultMethod { get; }

        public IMethodSymbol? SetExceptionMethod { get; }

        public IMethodSymbol? AwaitOnCompletedMethod { get; }

        public IPropertySymbol? TaskProperty { get; }
    }
}
