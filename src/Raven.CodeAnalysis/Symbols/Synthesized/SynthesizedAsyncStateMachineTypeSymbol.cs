using System;
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
    private readonly ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol> _methodTypeParameterMap;

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

        _methodTypeParameterMap = CloneMethodTypeParameters(asyncMethod);

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

    public ImmutableArray<SourceFieldSymbol> HoistedLocals => _hoistedLocals;

    public ImmutableArray<SourceFieldSymbol> HoistedLocalsToDispose => _hoistedLocalsToDispose;

    public SourceFieldSymbol BuilderField { get; }

    public SourceMethodSymbol Constructor { get; }

    public SourceMethodSymbol MoveNextMethod { get; }

    public SourceMethodSymbol SetStateMachineMethod { get; }

    internal ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol> MethodTypeParameterMap => _methodTypeParameterMap;

    public BoundBlockStatement? OriginalBody { get; private set; }

    public BoundBlockStatement? MoveNextBody { get; private set; }

    public BoundBlockStatement? SetStateMachineBody { get; private set; }

    public ConstructedStateMachine CreateConstructedInstance(IMethodSymbol method)
    {
        if (method is null)
            throw new ArgumentNullException(nameof(method));

        var constructedType = ConstructType(method);
        var substitution = constructedType as ConstructedNamedTypeSymbol;

        var parameterMapBuilder = ImmutableDictionary.CreateBuilder<IParameterSymbol, IFieldSymbol>(SymbolEqualityComparer.Default);
        foreach (var (parameter, field) in _parameterFieldMap)
            parameterMapBuilder[parameter] = SubstituteField(field, substitution);

        return new ConstructedStateMachine(
            this,
            constructedType,
            SubstituteField(StateField, substitution),
            ThisField is null ? null : SubstituteField(ThisField, substitution),
            parameterMapBuilder.ToImmutable(),
            SubstituteField(BuilderField, substitution),
            SubstituteMethod(Constructor, substitution),
            SubstituteMethod(MoveNextMethod, substitution),
            SubstituteMethod(SetStateMachineMethod, substitution));
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

    private ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol> CloneMethodTypeParameters(SourceMethodSymbol asyncMethod)
    {
        if (asyncMethod.TypeParameters.IsDefaultOrEmpty || asyncMethod.TypeParameters.Length == 0)
        {
            SetTypeParameters(ImmutableArray<ITypeParameterSymbol>.Empty);
            return ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol>.Empty;
        }

        var clonedParameters = ImmutableArray.CreateBuilder<ITypeParameterSymbol>(asyncMethod.TypeParameters.Length);
        var mapBuilder = ImmutableDictionary.CreateBuilder<ITypeParameterSymbol, ITypeParameterSymbol>(SymbolEqualityComparer.Default);

        var ordinal = 0;
        foreach (var typeParameter in asyncMethod.TypeParameters)
        {
            var sourceParameter = typeParameter as SourceTypeParameterSymbol;
            var constraintKind = sourceParameter?.ConstraintKind ?? typeParameter.ConstraintKind;
            var constraintReferences = sourceParameter?.ConstraintTypeReferences ?? ImmutableArray<SyntaxReference>.Empty;
            var variance = sourceParameter?.Variance ?? typeParameter.Variance;

            var cloned = new SourceTypeParameterSymbol(
                typeParameter.Name,
                this,
                this,
                ContainingNamespace,
                s_emptyLocations,
                s_emptySyntax,
                ordinal++,
                constraintKind,
                constraintReferences,
                variance);

            clonedParameters.Add(cloned);
            mapBuilder.Add(typeParameter, cloned);
        }

        var map = mapBuilder.ToImmutable();
        SetTypeParameters(clonedParameters);

        foreach (var typeParameter in asyncMethod.TypeParameters)
        {
            if (!map.TryGetValue(typeParameter, out var cloned))
                continue;

            if (typeParameter.ConstraintTypes.IsDefaultOrEmpty || typeParameter.ConstraintTypes.Length == 0)
                continue;

            var substitutedConstraints = typeParameter.ConstraintTypes
                .Select(constraint => SubstituteType(constraint, map))
                .ToImmutableArray();

            if (cloned is SourceTypeParameterSymbol clonedSource)
                clonedSource.SetConstraintTypes(substitutedConstraints);
        }

        return map;
    }

    internal ITypeSymbol SubstituteMethodType(ITypeSymbol type)
    {
        if (type is null)
            throw new ArgumentNullException(nameof(type));

        return SubstituteType(type, _methodTypeParameterMap);
    }

    private ITypeSymbol SubstituteType(ITypeSymbol type, ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol> map)
    {
        if (type is null)
            return type!;

        if (map.IsEmpty)
            return type;

        return Substitute(type);

        ITypeSymbol Substitute(ITypeSymbol current)
        {
            if (current is ITypeParameterSymbol typeParameter &&
                map.TryGetValue(typeParameter, out var mapped))
                return mapped;

            switch (current)
            {
                case ByRefTypeSymbol byRef:
                {
                    var substitutedElement = Substitute(byRef.ElementType);
                    if (!SymbolEqualityComparer.Default.Equals(substitutedElement, byRef.ElementType))
                        return new ByRefTypeSymbol(substitutedElement, byRef.RefKind);

                    return current;
                }

                case NullableTypeSymbol nullable:
                {
                    var substitutedUnderlying = Substitute(nullable.UnderlyingType);
                    if (!SymbolEqualityComparer.Default.Equals(substitutedUnderlying, nullable.UnderlyingType))
                        return new NullableTypeSymbol(substitutedUnderlying, this, this, ContainingNamespace, s_emptyLocations);

                    return current;
                }

                case IArrayTypeSymbol array:
                {
                    var substitutedElement = Substitute(array.ElementType);
                    if (!SymbolEqualityComparer.Default.Equals(substitutedElement, array.ElementType))
                    {
                        var arrayBaseType = Compilation.GetSpecialType(SpecialType.System_Array);
                        return new ArrayTypeSymbol(
                            arrayBaseType,
                            substitutedElement,
                            this,
                            this,
                            ContainingNamespace,
                            s_emptyLocations,
                            array.Rank);
                    }

                    return current;
                }

                case INamedTypeSymbol named when named.IsGenericType && !named.IsUnboundGenericType:
                {
                    var substitutedArgs = named.TypeArguments.Select(Substitute).ToArray();
                    var originalArgs = named.TypeArguments;
                    var changed = false;

                    for (var i = 0; i < substitutedArgs.Length; i++)
                    {
                        if (!SymbolEqualityComparer.Default.Equals(substitutedArgs[i], originalArgs[i]))
                        {
                            changed = true;
                            break;
                        }
                    }

                    if (!changed)
                        return current;

                    if (named.Construct(substitutedArgs) is INamedTypeSymbol constructed)
                        return constructed;

                    return current;
                }

                default:
                    return current;
            }
        }
    }

    private SourceFieldSymbol CreateField(string name, ITypeSymbol type)
    {
        var substitutedType = SubstituteType(type, _methodTypeParameterMap);

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

    private ITypeSymbol DetermineBuilderType(Compilation compilation, SourceMethodSymbol asyncMethod)
    {
        var returnType = asyncMethod.ReturnType;

        if (returnType.SpecialType == SpecialType.System_Void)
            return compilation.GetSpecialType(SpecialType.System_Runtime_CompilerServices_AsyncVoidMethodBuilder);

        if (returnType.SpecialType == SpecialType.System_Threading_Tasks_Task)
            return compilation.GetSpecialType(SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder);

        if (returnType is INamedTypeSymbol named &&
            named.OriginalDefinition.SpecialType == SpecialType.System_Threading_Tasks_Task_T &&
            named.TypeArguments.Length == 1)
        {
            var awaitedType = SubstituteType(named.TypeArguments[0], _methodTypeParameterMap);

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

    private INamedTypeSymbol ConstructType(IMethodSymbol method)
    {
        if (method is null)
            throw new ArgumentNullException(nameof(method));

        if (!IsGenericType)
            return this;

        var typeArguments = GetMethodTypeArguments(method);
        if (typeArguments.IsDefaultOrEmpty || typeArguments.Length != TypeParameters.Length)
            return this;

        return new ConstructedNamedTypeSymbol(this, typeArguments);
    }

    private static ImmutableArray<ITypeSymbol> GetMethodTypeArguments(IMethodSymbol method)
    {
        if (method is ConstructedMethodSymbol constructed)
            return constructed.TypeArguments;

        if (method is SourceMethodSymbol source)
            return source.TypeArguments;

        return method.TypeArguments;
    }

    private static IFieldSymbol SubstituteField(SourceFieldSymbol field, ConstructedNamedTypeSymbol? substitution)
    {
        if (field is null)
            throw new ArgumentNullException(nameof(field));

        return substitution is null
            ? field
            : new SubstitutedFieldSymbol(field, substitution);
    }

    private static IMethodSymbol SubstituteMethod(SourceMethodSymbol method, ConstructedNamedTypeSymbol? substitution)
    {
        if (method is null)
            throw new ArgumentNullException(nameof(method));

        return substitution is null
            ? method
            : new SubstitutedMethodSymbol(method, substitution);
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

    public readonly struct ConstructedStateMachine
    {
        internal ConstructedStateMachine(
            SynthesizedAsyncStateMachineTypeSymbol definition,
            INamedTypeSymbol type,
            IFieldSymbol stateField,
            IFieldSymbol? thisField,
            ImmutableDictionary<IParameterSymbol, IFieldSymbol> parameterFieldMap,
            IFieldSymbol builderField,
            IMethodSymbol constructor,
            IMethodSymbol moveNextMethod,
            IMethodSymbol setStateMachineMethod)
        {
            Definition = definition;
            Type = type;
            StateField = stateField;
            ThisField = thisField;
            ParameterFieldMap = parameterFieldMap;
            BuilderField = builderField;
            Constructor = constructor;
            MoveNextMethod = moveNextMethod;
            SetStateMachineMethod = setStateMachineMethod;
        }

        public SynthesizedAsyncStateMachineTypeSymbol Definition { get; }

        public INamedTypeSymbol Type { get; }

        public IFieldSymbol StateField { get; }

        public IFieldSymbol? ThisField { get; }

        public ImmutableDictionary<IParameterSymbol, IFieldSymbol> ParameterFieldMap { get; }

        public IFieldSymbol BuilderField { get; }

        public IMethodSymbol Constructor { get; }

        public IMethodSymbol MoveNextMethod { get; }

        public IMethodSymbol SetStateMachineMethod { get; }
    }
}
