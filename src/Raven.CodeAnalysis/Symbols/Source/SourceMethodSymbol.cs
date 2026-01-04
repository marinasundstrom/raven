using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Globalization;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceMethodSymbol : SourceSymbol, IMethodSymbol
{
    private IEnumerable<SourceParameterSymbol> _parameters;
    private ITypeSymbol _returnType;
    private ImmutableArray<ITypeParameterSymbol> _typeParameters = ImmutableArray<ITypeParameterSymbol>.Empty;
    private ImmutableArray<ITypeSymbol> _typeArguments = ImmutableArray<ITypeSymbol>.Empty;
    private bool _isOverride;
    private bool _isVirtual;
    private bool _isSealed;
    private bool _isAbstract;
    private bool _declaredInExtension;
    private ImmutableArray<AttributeData> _lazyReturnTypeAttributes;
    private bool? _lazyIsExtensionMethod;
    private ImmutableArray<AttributeData> _lazyAugmentedAttributes;
    private IteratorMethodKind _iteratorKind;
    private ITypeSymbol? _iteratorElementType;
    private bool _isIterator;
    private SynthesizedIteratorTypeSymbol? _iteratorStateMachine;
    private bool _containsAwait;
    private SynthesizedAsyncStateMachineTypeSymbol? _asyncStateMachine;
    private bool _hasAsyncReturnTypeError;
    private bool _requiresAsyncReturnTypeInference;
    private bool _asyncReturnTypeInferenceComplete;

    private bool IsAutoPropertyAccessor
        => MethodKind is MethodKind.PropertyGet or MethodKind.PropertySet
           && ContainingSymbol is SourcePropertySymbol { IsAutoProperty: true };

    public SourceMethodSymbol(
        string name,
        ITypeSymbol returnType,
        ImmutableArray<SourceParameterSymbol> parameters,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        bool isStatic = true,
        MethodKind methodKind = MethodKind.Ordinary,
        bool isAsync = false,
        bool isVirtual = false,
        bool isOverride = false,
        bool isSealed = false,
        bool isAbstract = false,
        Accessibility declaredAccessibility = Accessibility.NotApplicable)
            : base(SymbolKind.Method, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences, declaredAccessibility)
    {
        _returnType = returnType;
        _parameters = parameters;

        IsStatic = isStatic;
        IsAsync = isAsync;

        MethodKind = methodKind;

        _isOverride = isOverride;
        _isAbstract = isAbstract;
        _isVirtual = isVirtual || isOverride || isAbstract;
        _isSealed = isSealed;
    }

    /// <summary>
    /// Metadata name as it appears in CLI metadata:
    /// - For non-generic methods, just <see cref="Name"/>.
    /// - For generic methods, <c>Name``arity</c> (double-backtick + arity).
    /// 
    /// We do NOT include parameter types or containing type here; that belongs
    /// in higher-level identity/display helpers (e.g. symbol keys).
    /// </summary>
    public override string MetadataName
    {
        get
        {
            var name = Name;

            // Generic method arity is encoded with ``N (double backtick),
            // e.g. "M``2" for a method with 2 type parameters.
            if (!_typeParameters.IsDefaultOrEmpty && _typeParameters.Length > 0)
            {
                name += "``" + _typeParameters.Length.ToString(CultureInfo.InvariantCulture);
            }

            return name;
        }
    }

    public ITypeSymbol ReturnType => _returnType;

    public ImmutableArray<IParameterSymbol> Parameters => _parameters.OfType<IParameterSymbol>().ToImmutableArray();

    public ImmutableArray<AttributeData> GetReturnTypeAttributes()
    {
        if (_lazyReturnTypeAttributes.IsDefault)
            _lazyReturnTypeAttributes = ComputeReturnTypeAttributes();

        return _lazyReturnTypeAttributes;
    }

    public bool IsConstructor => MethodKind is MethodKind.Constructor or MethodKind.NamedConstructor;

    public bool IsNamedConstructor => MethodKind is MethodKind.NamedConstructor;

    public override bool IsStatic { get; }

    public MethodKind MethodKind { get; }

    public IMethodSymbol? OriginalDefinition => this;

    public bool IsAbstract => _isAbstract;

    public bool IsAsync { get; }

    public bool ContainsAwait => _containsAwait;

    internal bool HasAsyncReturnTypeError => _hasAsyncReturnTypeError;
    internal bool RequiresAsyncReturnTypeInference => _requiresAsyncReturnTypeInference;
    internal bool AsyncReturnTypeInferenceComplete => _asyncReturnTypeInferenceComplete;
    internal bool ShouldDeferAsyncReturnDiagnostics =>
        _requiresAsyncReturnTypeInference && !_asyncReturnTypeInferenceComplete;

    public bool IsCheckedBuiltin { get; }

    public bool IsDefinition { get; }

    public bool IsExtensionMethod => _lazyIsExtensionMethod ??= ComputeIsExtensionMethod();

    public bool IsExtern { get; }

    public bool IsGenericMethod => !_typeParameters.IsDefaultOrEmpty && _typeParameters.Length > 0;

    public bool IsOverride => _isOverride;

    public bool IsReadOnly { get; }

    public bool IsSealed => _isSealed;

    public bool IsVirtual => _isVirtual;

    public ISymbol? AssociatedSymbol => ContainingSymbol?.Kind == SymbolKind.Property ? ContainingSymbol : null;

    public IMethodSymbol? OverriddenMethod { get; private set; }

    public ImmutableArray<IMethodSymbol> ExplicitInterfaceImplementations { get; private set; } = ImmutableArray<IMethodSymbol>.Empty;

    public ImmutableArray<ITypeParameterSymbol> TypeParameters => _typeParameters;

    public ImmutableArray<ITypeSymbol> TypeArguments => _typeArguments;

    public IMethodSymbol? ConstructedFrom => this;

    public bool IsIterator => _isIterator;

    public IteratorMethodKind IteratorKind => _iteratorKind;

    public ITypeSymbol? IteratorElementType => _iteratorElementType;

    public SynthesizedIteratorTypeSymbol? IteratorStateMachine => _iteratorStateMachine;

    public SynthesizedAsyncStateMachineTypeSymbol? AsyncStateMachine => _asyncStateMachine;

    public void SetParameters(IEnumerable<SourceParameterSymbol> parameters) => _parameters = parameters;

    internal void SetOverriddenMethod(IMethodSymbol overriddenMethod) => OverriddenMethod = overriddenMethod;

    internal void SetExplicitInterfaceImplementations(ImmutableArray<IMethodSymbol> implementations) => ExplicitInterfaceImplementations = implementations;

    public BoundObjectCreationExpression? ConstructorInitializer { get; private set; }

    public bool HasConstructorInitializerSyntax { get; private set; }

    internal void MarkConstructorInitializerSyntax() => HasConstructorInitializerSyntax = true;

    internal void SetConstructorInitializer(BoundObjectCreationExpression? initializer) => ConstructorInitializer = initializer;

    internal void SetTypeParameters(IEnumerable<ITypeParameterSymbol> typeParameters)
    {
        _typeParameters = typeParameters.ToImmutableArray();
        _typeArguments = _typeParameters.IsDefaultOrEmpty
            ? ImmutableArray<ITypeSymbol>.Empty
            : _typeParameters.Select(static tp => (ITypeSymbol)tp).ToImmutableArray();
    }

    internal void SetReturnType(ITypeSymbol returnType) => _returnType = returnType;

    internal void UpdateModifiers(bool isVirtual, bool isOverride, bool isSealed, bool isAbstract)
    {
        _isOverride = isOverride;
        _isAbstract = isAbstract;
        _isVirtual = isVirtual || isOverride || isAbstract;
        _isSealed = isSealed;
    }

    internal void MarkIterator(IteratorMethodKind kind, ITypeSymbol elementType)
    {
        if (kind == IteratorMethodKind.None)
            return;

        _isIterator = true;
        _iteratorKind = kind;
        _iteratorElementType = elementType;
    }

    internal void SetContainsAwait(bool containsAwait)
    {
        _containsAwait = containsAwait;
    }

    internal void MarkDeclaredInExtension()
    {
        _declaredInExtension = true;
        _lazyIsExtensionMethod = true;
    }

    public override ImmutableArray<AttributeData> GetAttributes()
    {
        if (!_declaredInExtension && !IsAutoPropertyAccessor)
            return base.GetAttributes();

        if (_lazyAugmentedAttributes.IsDefault)
        {
            var baseAttributes = base.GetAttributes();
            var builder = ImmutableArray.CreateBuilder<AttributeData>();

            if (IsAutoPropertyAccessor)
            {
                var compilerGenerated = CreateCompilerGeneratedAttribute();
                if (compilerGenerated is not null)
                    builder.Add(compilerGenerated);
            }

            if (_declaredInExtension)
            {
                var extensionAttribute = CreateExtensionAttributeData();
                if (extensionAttribute is not null)
                    builder.Add(extensionAttribute);
            }

            _lazyAugmentedAttributes = builder.Count == 0
                ? baseAttributes
                : baseAttributes.AddRange(builder.ToImmutable());
        }

        return _lazyAugmentedAttributes;
    }

    internal void MarkAsyncReturnTypeError()
    {
        _hasAsyncReturnTypeError = true;
    }

    internal void RequireAsyncReturnTypeInference()
    {
        _requiresAsyncReturnTypeInference = true;
        _asyncReturnTypeInferenceComplete = false;
    }

    internal void CompleteAsyncReturnTypeInference()
    {
        if (!_requiresAsyncReturnTypeInference)
            return;

        _asyncReturnTypeInferenceComplete = true;
    }

    internal void SetIteratorStateMachine(SynthesizedIteratorTypeSymbol stateMachine)
    {
        if (stateMachine is null)
            throw new ArgumentNullException(nameof(stateMachine));

        if (_iteratorStateMachine is not null && !ReferenceEquals(_iteratorStateMachine, stateMachine))
            throw new InvalidOperationException("Iterator state machine already assigned.");

        _iteratorStateMachine = stateMachine;
    }

    internal void SetAsyncStateMachine(SynthesizedAsyncStateMachineTypeSymbol stateMachine)
    {
        if (stateMachine is null)
            throw new ArgumentNullException(nameof(stateMachine));

        if (_asyncStateMachine is not null && !ReferenceEquals(_asyncStateMachine, stateMachine))
            throw new InvalidOperationException("Async state machine already assigned.");

        _asyncStateMachine = stateMachine;
    }

    public IMethodSymbol Construct(params ITypeSymbol[] typeArguments)
    {
        if (typeArguments is null)
            throw new ArgumentNullException(nameof(typeArguments));

        return new ConstructedMethodSymbol(this, typeArguments.ToImmutableArray());
    }

    private ImmutableArray<AttributeData> ComputeReturnTypeAttributes()
    {
        if (DeclaringSyntaxReferences.IsDefaultOrEmpty)
            return ImmutableArray<AttributeData>.Empty;

        var compilation = GetDeclaringCompilation();
        if (compilation is null)
            return ImmutableArray<AttributeData>.Empty;

        var builder = ImmutableArray.CreateBuilder<AttributeData>();
        var seenAttributes = new Dictionary<AttributeTargets, HashSet<INamedTypeSymbol>>();
        const AttributeTargets defaultTarget = AttributeTargets.ReturnValue;

        foreach (var syntaxReference in DeclaringSyntaxReferences)
        {
            var syntax = syntaxReference.GetSyntax();
            var returnClause = syntax switch
            {
                MethodDeclarationSyntax method => method.ReturnType,
                FunctionStatementSyntax function => function.ReturnType,
                ParenthesizedLambdaExpressionSyntax lambda => lambda.ReturnType,
                _ => null
            };

            if (returnClause is null || returnClause.AttributeLists.Count == 0)
                continue;

            var semanticModel = compilation.GetSemanticModel(returnClause.SyntaxTree);

            foreach (var attribute in returnClause.AttributeLists.SelectMany(static list => list.Attributes))
            {
                var data = semanticModel.BindAttribute(attribute);
                if (data is null)
                    continue;

                if (AttributeUsageHelper.TryValidateAttribute(
                        compilation,
                        semanticModel,
                        this,
                        attribute,
                        data,
                        defaultTarget,
                        seenAttributes))
                {
                    builder.Add(data);
                }
            }
        }

        return builder.ToImmutable();
    }

    private bool ComputeIsExtensionMethod()
    {
        if (_declaredInExtension)
            return true;

        if (!IsStatic)
            return false;

        if (Parameters.IsDefaultOrEmpty || Parameters.Length == 0)
            return false;

        foreach (var syntaxReference in DeclaringSyntaxReferences)
        {
            if (syntaxReference.GetSyntax() is not MethodDeclarationSyntax method)
                continue;

            foreach (var attribute in method.AttributeLists.SelectMany(static list => list.Attributes))
            {
                if (IsExtensionAttributeName(attribute.Name))
                    return true;
            }
        }

        return false;
    }

    private static bool IsExtensionAttributeName(TypeSyntax name)
    {
        return name switch
        {
            IdentifierNameSyntax identifier => IsExtensionIdentifier(identifier.Identifier.Text),
            QualifiedNameSyntax qualified => IsExtensionAttributeName(qualified.Right),
            AliasQualifiedNameSyntax alias => IsExtensionIdentifier(alias.Name.Identifier.Text),
            GenericNameSyntax generic => IsExtensionIdentifier(generic.Identifier.Text),
            _ => false,
        };

        static bool IsExtensionIdentifier(string identifier)
            => identifier is "Extension" or "ExtensionAttribute";
    }

    private AttributeData? CreateExtensionAttributeData()
    {
        var compilation = GetDeclaringCompilation();
        if (compilation is null)
            return null;

        var attributeType = compilation.GetTypeByMetadataName("System.Runtime.CompilerServices.ExtensionAttribute");
        if (attributeType is null)
            return null;

        var constructor = attributeType.Constructors.FirstOrDefault(c => !c.IsStatic && c.Parameters.Length == 0);
        if (constructor is null)
            return null;

        var syntaxReference = DeclaringSyntaxReferences.FirstOrDefault();
        if (syntaxReference is null)
            return null;

        return new AttributeData(
            attributeType,
            constructor,
            ImmutableArray<TypedConstant>.Empty,
            ImmutableArray<KeyValuePair<string, TypedConstant>>.Empty,
            syntaxReference);
    }
}
