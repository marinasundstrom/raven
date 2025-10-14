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
    private readonly ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol> _stateMachineTypeParameterToMethodMap;

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

        _methodTypeParameterMap = InitializeTypeParameters(asyncMethod, out _stateMachineTypeParameterToMethodMap);

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

    public BoundBlockStatement? OriginalBody { get; private set; }

    public BoundBlockStatement? MoveNextBody { get; private set; }

    public BoundBlockStatement? SetStateMachineBody { get; private set; }

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

    internal bool TryGetAsyncMethodTypeParameter(ITypeParameterSymbol stateMachineParameter, out ITypeParameterSymbol methodParameter)
        => _stateMachineTypeParameterToMethodMap.TryGetValue(stateMachineParameter, out methodParameter);

    private ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol> InitializeTypeParameters(
        SourceMethodSymbol asyncMethod,
        out ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol> reverseMap)
    {
        if (asyncMethod.TypeParameters.IsDefaultOrEmpty)
        {
            reverseMap = ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol>.Empty;
            return ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol>.Empty;
        }

        var mapBuilder = ImmutableDictionary.CreateBuilder<ITypeParameterSymbol, ITypeParameterSymbol>(SymbolEqualityComparer.Default);
        var reverseBuilder = ImmutableDictionary.CreateBuilder<ITypeParameterSymbol, ITypeParameterSymbol>(SymbolEqualityComparer.Default);
        var parameterBuilder = ImmutableArray.CreateBuilder<ITypeParameterSymbol>(asyncMethod.TypeParameters.Length);

        for (var i = 0; i < asyncMethod.TypeParameters.Length; i++)
        {
            var typeParameter = asyncMethod.TypeParameters[i];
            var constraintReferences = typeParameter is SourceTypeParameterSymbol sourceParameter
                ? sourceParameter.ConstraintTypeReferences
                : ImmutableArray<SyntaxReference>.Empty;

            var synthesized = new SourceTypeParameterSymbol(
                typeParameter.Name,
                this,
                this,
                ContainingNamespace,
                s_emptyLocations,
                s_emptySyntax,
                i,
                typeParameter.ConstraintKind,
                constraintReferences,
                typeParameter.Variance);

            mapBuilder.Add(typeParameter, synthesized);
            reverseBuilder.Add(synthesized, typeParameter);
            parameterBuilder.Add(synthesized);
        }

        var map = mapBuilder.ToImmutable();
        reverseMap = reverseBuilder.ToImmutable();
        SetTypeParameters(parameterBuilder.ToImmutable());

        foreach (var typeParameter in asyncMethod.TypeParameters)
        {
            if (typeParameter.ConstraintTypes.IsDefaultOrEmpty)
                continue;

            if (map[typeParameter] is SourceTypeParameterSymbol synthesized)
            {
                var substitutedConstraints = typeParameter.ConstraintTypes
                    .Select(constraint => SubstituteMethodTypeParameters(constraint, map))
                    .ToImmutableArray();

                synthesized.SetConstraintTypes(substitutedConstraints);
            }
        }

        return map;
    }

    private ITypeSymbol SubstituteMethodTypeParameters(ITypeSymbol type)
        => SubstituteMethodTypeParameters(type, _methodTypeParameterMap);

    private ITypeSymbol SubstituteMethodTypeParameters(
        ITypeSymbol type,
        ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol> substitutionMap)
    {
        if (substitutionMap.IsEmpty)
            return type;

        return Substitute(type);

        ITypeSymbol Substitute(ITypeSymbol symbol)
        {
            if (symbol is ITypeParameterSymbol typeParameter && substitutionMap.TryGetValue(typeParameter, out var replacement))
                return replacement;

            if (symbol is ByRefTypeSymbol byRef)
            {
                var substitutedElement = Substitute(byRef.ElementType);
                if (!SymbolEqualityComparer.Default.Equals(byRef.ElementType, substitutedElement))
                    return new ByRefTypeSymbol(substitutedElement, byRef.RefKind);

                return symbol;
            }

            if (symbol is IArrayTypeSymbol array)
            {
                var substitutedElement = Substitute(array.ElementType);
                if (!SymbolEqualityComparer.Default.Equals(array.ElementType, substitutedElement))
                    return Compilation.CreateArrayTypeSymbol(substitutedElement, array.Rank);

                return symbol;
            }

            if (symbol is NullableTypeSymbol nullable)
            {
                var substitutedUnderlying = Substitute(nullable.UnderlyingType);
                if (!SymbolEqualityComparer.Default.Equals(nullable.UnderlyingType, substitutedUnderlying))
                    return new NullableTypeSymbol(substitutedUnderlying, this, this, ContainingNamespace, s_emptyLocations);

                return symbol;
            }

            if (symbol is LiteralTypeSymbol literal)
            {
                var substitutedUnderlying = Substitute(literal.UnderlyingType);
                if (!SymbolEqualityComparer.Default.Equals(literal.UnderlyingType, substitutedUnderlying))
                    return new LiteralTypeSymbol(substitutedUnderlying, literal.ConstantValue, Compilation);

                return symbol;
            }

            if (symbol is ITupleTypeSymbol tuple)
            {
                var substitutedUnderlying = Substitute(tuple.UnderlyingTupleType);
                var elementTypes = tuple.TupleElements
                    .Select(element => Substitute(element.Type))
                    .ToArray();

                var changed = !SymbolEqualityComparer.Default.Equals(tuple.UnderlyingTupleType, substitutedUnderlying)
                    || elementTypes.Where((t, i) => !SymbolEqualityComparer.Default.Equals(t, tuple.TupleElements[i].Type)).Any();

                if (changed)
                {
                    var elements = tuple.TupleElements
                        .Select((element, index) => (element.Name, elementTypes[index]))
                        .ToArray();

                    return Compilation.CreateTupleTypeSymbol(elements);
                }

                return symbol;
            }

            if (symbol is INamedTypeSymbol named && named.IsGenericType && !named.IsUnboundGenericType)
            {
                var substitutedArguments = named.TypeArguments
                    .Select(Substitute)
                    .ToArray();

                for (var i = 0; i < substitutedArguments.Length; i++)
                {
                    if (!SymbolEqualityComparer.Default.Equals(substitutedArguments[i], named.TypeArguments[i]))
                        return named.Construct(substitutedArguments);
                }
            }

            return symbol;
        }
    }

    private SourceFieldSymbol CreateField(string name, ITypeSymbol type)
    {
        var substitutedType = SubstituteMethodTypeParameters(type);

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

    private static ITypeSymbol DetermineBuilderType(Compilation compilation, SourceMethodSymbol asyncMethod)
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
}
