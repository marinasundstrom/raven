using System;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class SynthesizedIteratorTypeSymbol : SourceNamedTypeSymbol
{
    private static readonly Location[] s_emptyLocations = Array.Empty<Location>();
    private static readonly SyntaxReference[] s_emptySyntax = Array.Empty<SyntaxReference>();

    private ImmutableArray<SourceFieldSymbol> _hoistedLocals;
    private readonly ImmutableDictionary<IParameterSymbol, SourceFieldSymbol> _parameterFieldMap;

    public SynthesizedIteratorTypeSymbol(
        Compilation compilation,
        SourceMethodSymbol iteratorMethod,
        string name,
        IteratorMethodKind iteratorKind,
        ITypeSymbol elementType)
        : base(
            name,
            compilation.GetSpecialType(SpecialType.System_Object),
            TypeKind.Class,
            iteratorMethod.ContainingSymbol ?? iteratorMethod,
            iteratorMethod.ContainingType,
            iteratorMethod.ContainingNamespace,
            s_emptyLocations,
            s_emptySyntax,
            isSealed: true,
            declaredAccessibility: Accessibility.Private)
    {
        if (compilation is null)
            throw new ArgumentNullException(nameof(compilation));
        if (iteratorMethod is null)
            throw new ArgumentNullException(nameof(iteratorMethod));
        if (elementType is null)
            throw new ArgumentNullException(nameof(elementType));

        Compilation = compilation;
        IteratorMethod = iteratorMethod;
        IteratorKind = iteratorKind;
        ElementType = elementType;

        StateField = CreateField("_state", compilation.GetSpecialType(SpecialType.System_Int32));
        CurrentField = CreateField("_current", elementType);

        if (!iteratorMethod.IsStatic)
            ThisField = CreateField("_this", iteratorMethod.ContainingType ?? compilation.GetSpecialType(SpecialType.System_Object));

        ParameterFields = CreateParameterFields(iteratorMethod, out _parameterFieldMap);
        _hoistedLocals = ImmutableArray<SourceFieldSymbol>.Empty;

        Constructor = CreateConstructor(compilation, iteratorMethod);
        MoveNextMethod = CreateMoveNextMethod(compilation, iteratorMethod);

        CurrentProperty = CreateCurrentProperty(compilation, iteratorMethod, elementType);
        NonGenericCurrentProperty = CreateNonGenericCurrentProperty(compilation, iteratorMethod);
        DisposeMethod = CreateDisposeMethod(compilation, iteratorMethod);
        ResetMethod = CreateResetMethod(compilation, iteratorMethod);

        if (iteratorKind == IteratorMethodKind.Enumerable)
        {
            GenericGetEnumeratorMethod = CreateGetEnumeratorMethod(compilation, iteratorMethod, elementType);
            NonGenericGetEnumeratorMethod = CreateNonGenericGetEnumeratorMethod(compilation, iteratorMethod);
        }

        SetInterfaces(CreateInterfaceList(compilation, iteratorKind, elementType));
    }

    public Compilation Compilation { get; }

    public SourceMethodSymbol IteratorMethod { get; }

    public IteratorMethodKind IteratorKind { get; }

    public ITypeSymbol ElementType { get; }

    public SourceFieldSymbol StateField { get; }

    public SourceFieldSymbol CurrentField { get; }

    public SourceFieldSymbol? ThisField { get; }

    public ImmutableArray<SourceFieldSymbol> ParameterFields { get; }

    public ImmutableDictionary<IParameterSymbol, SourceFieldSymbol> ParameterFieldMap => _parameterFieldMap;

    public ImmutableArray<SourceFieldSymbol> HoistedLocals => _hoistedLocals;

    public SourceMethodSymbol Constructor { get; }

    public SourceMethodSymbol MoveNextMethod { get; }

    public BoundBlockStatement? MoveNextBody { get; private set; }

    public BoundBlockStatement? CurrentGetterBody { get; private set; }

    public BoundBlockStatement? NonGenericCurrentGetterBody { get; private set; }

    public BoundBlockStatement? DisposeBody { get; private set; }

    public BoundBlockStatement? ResetBody { get; private set; }

    public BoundBlockStatement? GenericGetEnumeratorBody { get; private set; }

    public BoundBlockStatement? NonGenericGetEnumeratorBody { get; private set; }

    public SourcePropertySymbol CurrentProperty { get; }

    public SourcePropertySymbol NonGenericCurrentProperty { get; }

    public SourceMethodSymbol DisposeMethod { get; }

    public SourceMethodSymbol ResetMethod { get; }

    public SourceMethodSymbol? GenericGetEnumeratorMethod { get; }

    public SourceMethodSymbol? NonGenericGetEnumeratorMethod { get; }

    public SourceFieldSymbol AddHoistedLocal(string name, ITypeSymbol type)
    {
        if (string.IsNullOrEmpty(name))
            throw new ArgumentException("Field name cannot be null or empty.", nameof(name));
        if (type is null)
            throw new ArgumentNullException(nameof(type));

        var field = CreateField(name, type);
        _hoistedLocals = _hoistedLocals.Add(field);
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

    public void SetCurrentGetterBody(BoundBlockStatement body)
    {
        if (body is null)
            throw new ArgumentNullException(nameof(body));

        if (CurrentGetterBody is not null && !ReferenceEquals(CurrentGetterBody, body))
            throw new InvalidOperationException("Current getter body already assigned.");

        CurrentGetterBody = body;
    }

    public void SetNonGenericCurrentGetterBody(BoundBlockStatement body)
    {
        if (body is null)
            throw new ArgumentNullException(nameof(body));

        if (NonGenericCurrentGetterBody is not null && !ReferenceEquals(NonGenericCurrentGetterBody, body))
            throw new InvalidOperationException("Non-generic Current getter body already assigned.");

        NonGenericCurrentGetterBody = body;
    }

    public void SetDisposeBody(BoundBlockStatement body)
    {
        if (body is null)
            throw new ArgumentNullException(nameof(body));

        if (DisposeBody is not null && !ReferenceEquals(DisposeBody, body))
            throw new InvalidOperationException("Dispose body already assigned.");

        DisposeBody = body;
    }

    public void SetResetBody(BoundBlockStatement body)
    {
        if (body is null)
            throw new ArgumentNullException(nameof(body));

        if (ResetBody is not null && !ReferenceEquals(ResetBody, body))
            throw new InvalidOperationException("Reset body already assigned.");

        ResetBody = body;
    }

    public void SetGenericGetEnumeratorBody(BoundBlockStatement body)
    {
        if (body is null)
            throw new ArgumentNullException(nameof(body));

        if (GenericGetEnumeratorBody is not null && !ReferenceEquals(GenericGetEnumeratorBody, body))
            throw new InvalidOperationException("Generic GetEnumerator body already assigned.");

        GenericGetEnumeratorBody = body;
    }

    public void SetNonGenericGetEnumeratorBody(BoundBlockStatement body)
    {
        if (body is null)
            throw new ArgumentNullException(nameof(body));

        if (NonGenericGetEnumeratorBody is not null && !ReferenceEquals(NonGenericGetEnumeratorBody, body))
            throw new InvalidOperationException("Non-generic GetEnumerator body already assigned.");

        NonGenericGetEnumeratorBody = body;
    }

    private ImmutableArray<SourceFieldSymbol> CreateParameterFields(
        SourceMethodSymbol iteratorMethod,
        out ImmutableDictionary<IParameterSymbol, SourceFieldSymbol> parameterFieldMap)
    {
        if (iteratorMethod.Parameters.IsDefaultOrEmpty)
        {
            parameterFieldMap = ImmutableDictionary<IParameterSymbol, SourceFieldSymbol>.Empty;
            return ImmutableArray<SourceFieldSymbol>.Empty;
        }

        var orderedFields = ImmutableArray.CreateBuilder<SourceFieldSymbol>(iteratorMethod.Parameters.Length);
        var mapBuilder = ImmutableDictionary.CreateBuilder<IParameterSymbol, SourceFieldSymbol>(SymbolEqualityComparer.Default);

        foreach (var parameter in iteratorMethod.Parameters)
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
        return new SourceFieldSymbol(
            name,
            type,
            isStatic: false,
            isLiteral: false,
            constantValue: null,
            this,
            this,
            ContainingNamespace,
            s_emptyLocations,
            s_emptySyntax,
            declaredAccessibility: Accessibility.Private);
    }

    private SourceMethodSymbol CreateConstructor(Compilation compilation, SourceMethodSymbol iteratorMethod)
    {
        return new SourceMethodSymbol(
            ".ctor",
            compilation.GetSpecialType(SpecialType.System_Void),
            ImmutableArray<SourceParameterSymbol>.Empty,
            this,
            this,
            iteratorMethod.ContainingNamespace,
            s_emptyLocations,
            s_emptySyntax,
            isStatic: false,
            methodKind: MethodKind.Constructor,
            declaredAccessibility: Accessibility.Public);
    }

    private SourceMethodSymbol CreateMoveNextMethod(Compilation compilation, SourceMethodSymbol iteratorMethod)
    {
        var boolType = compilation.GetSpecialType(SpecialType.System_Boolean);

        return new SourceMethodSymbol(
            "MoveNext",
            boolType,
            ImmutableArray<SourceParameterSymbol>.Empty,
            this,
            this,
            iteratorMethod.ContainingNamespace,
            s_emptyLocations,
            s_emptySyntax,
            isStatic: false,
            methodKind: MethodKind.Ordinary,
            declaredAccessibility: Accessibility.Public);
    }

    private SourcePropertySymbol CreateCurrentProperty(
        Compilation compilation,
        SourceMethodSymbol iteratorMethod,
        ITypeSymbol elementType)
    {
        var property = new SourcePropertySymbol(
            "Current",
            elementType,
            this,
            this,
            iteratorMethod.ContainingNamespace,
            s_emptyLocations,
            s_emptySyntax,
            isStatic: false,
            declaredAccessibility: Accessibility.Public);

        var getter = new SourceMethodSymbol(
            "get_Current",
            elementType,
            ImmutableArray<SourceParameterSymbol>.Empty,
            property,
            this,
            iteratorMethod.ContainingNamespace,
            s_emptyLocations,
            s_emptySyntax,
            isStatic: false,
            methodKind: MethodKind.PropertyGet,
            declaredAccessibility: Accessibility.Public);

        property.SetAccessors(getter, null);
        return property;
    }

    private SourcePropertySymbol CreateNonGenericCurrentProperty(
        Compilation compilation,
        SourceMethodSymbol iteratorMethod)
    {
        var objectType = compilation.GetSpecialType(SpecialType.System_Object);

        var property = new SourcePropertySymbol(
            "System.Collections.IEnumerator.Current",
            objectType,
            this,
            this,
            iteratorMethod.ContainingNamespace,
            s_emptyLocations,
            s_emptySyntax,
            isStatic: false,
            metadataName: "System.Collections.IEnumerator.Current",
            declaredAccessibility: Accessibility.Private);

        var getter = new SourceMethodSymbol(
            "System.Collections.IEnumerator.get_Current",
            objectType,
            ImmutableArray<SourceParameterSymbol>.Empty,
            property,
            this,
            iteratorMethod.ContainingNamespace,
            s_emptyLocations,
            s_emptySyntax,
            isStatic: false,
            methodKind: MethodKind.PropertyGet,
            declaredAccessibility: Accessibility.Private);

        var enumerator = compilation.GetSpecialType(SpecialType.System_Collections_IEnumerator);
        var interfaceGetter = enumerator
            .GetMembers("Current")
            .OfType<IPropertySymbol>()
            .Single()
            .GetMethod;

        if (interfaceGetter is not null)
            getter.SetExplicitInterfaceImplementations(ImmutableArray.Create(interfaceGetter));

        property.SetAccessors(getter, null);
        return property;
    }

    private SourceMethodSymbol CreateDisposeMethod(Compilation compilation, SourceMethodSymbol iteratorMethod)
    {
        var method = new SourceMethodSymbol(
            "Dispose",
            compilation.GetSpecialType(SpecialType.System_Void),
            ImmutableArray<SourceParameterSymbol>.Empty,
            this,
            this,
            iteratorMethod.ContainingNamespace,
            s_emptyLocations,
            s_emptySyntax,
            isStatic: false,
            methodKind: MethodKind.Ordinary,
            declaredAccessibility: Accessibility.Public);

        return method;
    }

    private SourceMethodSymbol CreateResetMethod(Compilation compilation, SourceMethodSymbol iteratorMethod)
    {
        var method = new SourceMethodSymbol(
            "Reset",
            compilation.GetSpecialType(SpecialType.System_Void),
            ImmutableArray<SourceParameterSymbol>.Empty,
            this,
            this,
            iteratorMethod.ContainingNamespace,
            s_emptyLocations,
            s_emptySyntax,
            isStatic: false,
            methodKind: MethodKind.Ordinary,
            declaredAccessibility: Accessibility.Public);

        return method;
    }

    private SourceMethodSymbol CreateGetEnumeratorMethod(
        Compilation compilation,
        SourceMethodSymbol iteratorMethod,
        ITypeSymbol elementType)
    {
        var enumerator = (INamedTypeSymbol)compilation
            .GetSpecialType(SpecialType.System_Collections_Generic_IEnumerator_T)
            .Construct(elementType);

        var method = new SourceMethodSymbol(
            "GetEnumerator",
            enumerator,
            ImmutableArray<SourceParameterSymbol>.Empty,
            this,
            this,
            iteratorMethod.ContainingNamespace,
            s_emptyLocations,
            s_emptySyntax,
            isStatic: false,
            methodKind: MethodKind.Ordinary,
            declaredAccessibility: Accessibility.Public);

        return method;
    }

    private SourceMethodSymbol CreateNonGenericGetEnumeratorMethod(
        Compilation compilation,
        SourceMethodSymbol iteratorMethod)
    {
        var method = new SourceMethodSymbol(
            "System.Collections.IEnumerable.GetEnumerator",
            compilation.GetSpecialType(SpecialType.System_Collections_IEnumerator),
            ImmutableArray<SourceParameterSymbol>.Empty,
            this,
            this,
            iteratorMethod.ContainingNamespace,
            s_emptyLocations,
            s_emptySyntax,
            isStatic: false,
            methodKind: MethodKind.ExplicitInterfaceImplementation,
            declaredAccessibility: Accessibility.Private);

        var enumerable = compilation.GetSpecialType(SpecialType.System_Collections_IEnumerable);
        var interfaceMethod = enumerable
            .GetMembers("GetEnumerator")
            .OfType<IMethodSymbol>()
            .SingleOrDefault();

        if (interfaceMethod is not null)
            method.SetExplicitInterfaceImplementations(ImmutableArray.Create(interfaceMethod));

        return method;
    }

    private static ImmutableArray<INamedTypeSymbol> CreateInterfaceList(
        Compilation compilation,
        IteratorMethodKind iteratorKind,
        ITypeSymbol elementType)
    {
        var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();

        var enumerator = compilation.GetSpecialType(SpecialType.System_Collections_IEnumerator);
        builder.Add(enumerator);

        var enumeratorGeneric = (INamedTypeSymbol)compilation
            .GetSpecialType(SpecialType.System_Collections_Generic_IEnumerator_T)
            .Construct(elementType);
        builder.Add(enumeratorGeneric);

        if (iteratorKind == IteratorMethodKind.Enumerable)
        {
            var enumerable = compilation.GetSpecialType(SpecialType.System_Collections_IEnumerable);
            builder.Add(enumerable);

            var enumerableGeneric = (INamedTypeSymbol)compilation
                .GetSpecialType(SpecialType.System_Collections_Generic_IEnumerable_T)
                .Construct(elementType);
            builder.Add(enumerableGeneric);
        }

        return builder.ToImmutable();
    }
}
