using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceMethodSymbol : SourceSymbol, MethodSymbol
{
    internal override bool IsTerminalRuntimeCall => RuntimeFailureContract.IsTerminal(this, GetDeclaringCompilation()?.Options);

    private ImmutableArray<SourceParameterSymbol> _parameters = ImmutableArray<SourceParameterSymbol>.Empty;
    private TypeSymbol _returnType;
    private bool _isStatic;
    private ImmutableArray<TypeParameterSymbol> _typeParameters = ImmutableArray<TypeParameterSymbol>.Empty;
    private ImmutableArray<TypeSymbol> _typeArguments = ImmutableArray<TypeSymbol>.Empty;
    private ImmutableArray<Symbol> _capturedVariables = ImmutableArray<Symbol>.Empty;
    private bool _isOverride;
    private bool _isVirtual;
    private bool _isSealed;
    private bool _isAbstract;
    private bool _declaredInExtension;
    private ImmutableArray<AttributeData> _lazyReturnTypeAttributes;
    private bool? _lazyIsExtensionMethod;
    private bool? _lazyIsPrimaryConstructorSymbol;
    private TypeDeclarationSyntax? _lazyDeclaringTypeSyntax;
    private bool _lazyDeclaringTypeSyntaxComputed;
    private ImmutableArray<AttributeData> _lazyAugmentedAttributes;
    private IteratorMethodKind _iteratorKind;
    private TypeSymbol? _iteratorElementType;
    private bool _isIterator;
    private SynthesizedIteratorTypeSymbol? _iteratorStateMachine;
    private bool _containsAwait;
    private SynthesizedAsyncStateMachineTypeSymbol? _asyncStateMachine;
    private bool _hasAsyncReturnTypeError;
    private bool _requiresAsyncReturnTypeInference;
    private bool _asyncReturnTypeInferenceComplete;
    private bool _setsRequiredMembers;
    private bool _isRecordCopyConstructor;
    private SourceNamedTypeSymbol? _closureFrameType;
    private bool? _lazyIsUnsafe;
    private bool _isPartialDefinition;
    private bool _isPartialImplementation;
    private bool _isSignatureSkeleton;
    private int _signatureVersion;

    private bool IsAutoPropertyAccessor
        => MethodKind is MethodKind.PropertyGet or MethodKind.PropertySet or MethodKind.InitOnly
           && ContainingSymbol is SourcePropertySymbol { IsAutoProperty: true };

    public SourceMethodSymbol(
        string name,
        TypeSymbol returnType,
        ImmutableArray<SourceParameterSymbol> parameters,
        Symbol containingSymbol,
        NamedTypeSymbol? containingType,
        NamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        bool isStatic = true,
        MethodKind methodKind = MethodKind.Ordinary,
        bool isAsync = false,
        bool isVirtual = false,
        bool isOverride = false,
        bool isSealed = false,
        bool isAbstract = false,
        bool isExtern = false,
        Accessibility declaredAccessibility = Accessibility.NotApplicable)
            : base(SymbolKind.Method, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences, declaredAccessibility)
    {
        _returnType = returnType;
        _parameters = parameters;

        _isStatic = isStatic;
        IsAsync = isAsync;

        MethodKind = methodKind;

        _isOverride = isOverride;
        _isAbstract = isAbstract;
        _isVirtual = isVirtual || isOverride || isAbstract;
        _isSealed = isSealed;
        IsExtern = isExtern;
    }

    /// <summary>
    /// Metadata name as it appears in CLI metadata. Generic method arity is
    /// stored in the method signature rather than encoded into this name;
    /// double-backtick notation belongs to documentation IDs.
    /// </summary>
    public override string MetadataName => Name;

    public TypeSymbol? DeclaredFunctionType { get; private set; }
    internal void SetDeclaredFunctionType(TypeSymbol type) => DeclaredFunctionType = type;

    public TypeSymbol ReturnType => _returnType;

    public ImmutableArray<ParameterSymbol> Parameters => _parameters.Cast<ParameterSymbol>().ToImmutableArray();

    public ImmutableArray<AttributeData> GetReturnTypeAttributes()
    {
        if (_lazyReturnTypeAttributes.IsDefault)
            _lazyReturnTypeAttributes = ComputeReturnTypeAttributes();

        return _lazyReturnTypeAttributes;
    }

    public bool IsConstructor => MethodKind is MethodKind.Constructor;

    public override bool IsStatic => _isStatic;

    public MethodKind MethodKind { get; }

    public MethodSymbol? OriginalDefinition => this;

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
    internal bool IsPrimaryConstructorSymbol => _lazyIsPrimaryConstructorSymbol ??= ComputeIsPrimaryConstructorSymbol();

    public bool IsExtern { get; }

    public bool IsUnsafe => _lazyIsUnsafe ??= ComputeIsUnsafe();

    public bool IsGenericMethod => !_typeParameters.IsDefaultOrEmpty && _typeParameters.Length > 0;

    public bool IsOverride => _isOverride;

    public bool IsReadOnly
    {
        get
        {
            if (IsStatic || ContainingType?.IsValueType != true ||
                MethodKind is MethodKind.Constructor or MethodKind.StaticConstructor or MethodKind.InitOnly)
                return false;
            if (ContainingType.IsReadOnly)
                return true;
            if (MethodKind == MethodKind.PropertyGet && IsAutoPropertyAccessor)
                return true;
            if (!IsSynthesizedValueMember)
                return false;
            return Name switch
            {
                "Equals" or "GetHashCode" => true,
                "Deconstruct" => ContainingType.GetMembers().OfType<PropertySymbol>()
                    .Where(property => Parameters.Any(parameter => parameter.Name == property.Name))
                    .All(property => property.GetMethod?.IsReadOnly == true),
                "ToString" when ContainingType is SourceNamedTypeSymbol { IsRecord: true } =>
                    ContainingType.GetMembers("PrintMembers").OfType<MethodSymbol>().FirstOrDefault()?.IsReadOnly == true,
                "PrintMembers" => ContainingType.GetMembers().OfType<PropertySymbol>()
                    .Where(property => !property.IsStatic && property.DeclaredAccessibility == Accessibility.Public &&
                        property.Parameters.IsEmpty && property.GetMethod?.DeclaredAccessibility == Accessibility.Public)
                    .All(property => property.GetMethod!.IsReadOnly),
                "ToString" when ContainingType is SourceUnionCaseTypeSymbol => true,
                _ => false
            };
        }
    }

    private bool IsSynthesizedValueMember => ContainingType switch
    {
        SourceUnionCaseTypeSymbol => true,
        SourceUnionSymbol => DeclaringSyntaxReferences.All(reference =>
            reference.GetSyntax() is UnionDeclarationSyntax or CaseDeclarationSyntax),
        SourceNamedTypeSymbol { IsRecord: true } => DeclaringSyntaxReferences.IsDefaultOrEmpty,
        _ => false
    };

    public bool IsFinal => _isSealed;

    public bool IsVirtual => _isVirtual;

    public Symbol? AssociatedSymbol => ContainingSymbol?.Kind is SymbolKind.Property or SymbolKind.Event ? ContainingSymbol : null;

    public MethodSymbol? OverriddenMethod { get; private set; }

    public ImmutableArray<MethodSymbol> ExplicitInterfaceImplementations { get; private set; } = ImmutableArray<MethodSymbol>.Empty;

    public ImmutableArray<TypeParameterSymbol> TypeParameters => _typeParameters;

    public ImmutableArray<TypeSymbol> TypeArguments => _typeArguments;

    public ImmutableArray<Symbol> CapturedVariables => _capturedVariables;
    internal SourceNamedTypeSymbol? ClosureFrameType => _closureFrameType;

    public bool HasCaptures => !_capturedVariables.IsDefaultOrEmpty && _capturedVariables.Length > 0;

    public MethodSymbol? ConstructedFrom => this;

    public bool IsIterator => _isIterator;

    public IteratorMethodKind IteratorKind => _iteratorKind;

    public TypeSymbol? IteratorElementType => _iteratorElementType;

    public SynthesizedIteratorTypeSymbol? IteratorStateMachine => _iteratorStateMachine;

    public SynthesizedAsyncStateMachineTypeSymbol? AsyncStateMachine => _asyncStateMachine;
    internal bool IsPartialMember => _isPartialDefinition || _isPartialImplementation;
    internal bool HasPartialDefinition => _isPartialDefinition;
    internal bool HasPartialImplementation => _isPartialImplementation;
    internal bool IsSignatureSkeleton => _isSignatureSkeleton;
    internal int SignatureVersion => _signatureVersion;

    internal void MarkSignatureSkeleton() => _isSignatureSkeleton = true;

    internal void MarkSignatureBindingComplete() => _isSignatureSkeleton = false;

    public void SetParameters(IEnumerable<SourceParameterSymbol> parameters)
    {
        _parameters = parameters.ToImmutableArray();
        _signatureVersion++;
    }

    internal void SetOverriddenMethod(MethodSymbol overriddenMethod) => OverriddenMethod = overriddenMethod;

    internal void SetExplicitInterfaceImplementations(ImmutableArray<MethodSymbol> implementations) => ExplicitInterfaceImplementations = implementations;

    public BoundObjectCreationExpression? ConstructorInitializer { get; private set; }

    public bool HasConstructorInitializerSyntax { get; private set; }

    internal bool DelegatesToSameTypeConstructor => ConstructorInitializer is { } initializer &&
        SymbolEqualityComparer.Default.Equals(initializer.Constructor.ContainingType, ContainingType);
    public bool SetsRequiredMembers
    {
        get => _setsRequiredMembers || _isRecordCopyConstructor && HasRequiredRecordMembers();
        set => _setsRequiredMembers = value;
    }

    internal void MarkRecordCopyConstructor() => _isRecordCopyConstructor = true;

    private bool HasRequiredRecordMembers()
    {
        // Record synthesis precedes completion of body properties. Inspect the
        // completed members when the constructor contract is queried.
        for (var current = ContainingType; current is not null; current = current.BaseType)
        {
            if (current.GetMembers().Any(member => member is FieldSymbol { IsRequired: true } or PropertySymbol { IsRequired: true }))
                return true;
        }

        return false;
    }

    internal void MarkConstructorInitializerSyntax() => HasConstructorInitializerSyntax = true;

    internal void SetConstructorInitializer(BoundObjectCreationExpression? initializer) => ConstructorInitializer = initializer;

    internal void SetTypeParameters(IEnumerable<TypeParameterSymbol> typeParameters)
    {
        _typeParameters = typeParameters.ToImmutableArray();
        _typeArguments = _typeParameters.IsDefaultOrEmpty
            ? ImmutableArray<TypeSymbol>.Empty
            : _typeParameters.Select(static tp => (TypeSymbol)tp).ToImmutableArray();
        _signatureVersion++;
    }

    internal void SetIsStatic(bool isStatic)
    {
        _isStatic = isStatic;
    }

    internal void SetCapturedVariables(IEnumerable<Symbol> capturedVariables)
    {
        _capturedVariables = capturedVariables.ToImmutableArray();
    }

    internal void SetClosureFrameType(SourceNamedTypeSymbol? closureFrameType)
    {
        _closureFrameType = closureFrameType;
    }

    internal void SetReturnType(TypeSymbol returnType)
    {
        _returnType = returnType;
        _signatureVersion++;
    }

    internal void UpdateModifiers(bool isVirtual, bool isOverride, bool isSealed, bool isAbstract)
    {
        _isOverride = isOverride;
        _isAbstract = isAbstract;
        _isVirtual = isVirtual || isOverride || isAbstract;
        _isSealed = isSealed;
    }

    internal void MarkIterator(IteratorMethodKind kind, TypeSymbol elementType)
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

    internal TypeDeclarationSyntax? GetDeclaringTypeSyntax()
    {
        if (_lazyDeclaringTypeSyntaxComputed)
            return _lazyDeclaringTypeSyntax;

        _lazyDeclaringTypeSyntaxComputed = true;

        foreach (var syntaxReference in DeclaringSyntaxReferences)
        {
            switch (syntaxReference.GetSyntax())
            {
                case TypeDeclarationSyntax typeDeclaration:
                    _lazyDeclaringTypeSyntax = typeDeclaration;
                    return _lazyDeclaringTypeSyntax;
                case MemberDeclarationSyntax memberDeclaration when memberDeclaration.Parent is TypeDeclarationSyntax parentType:
                    _lazyDeclaringTypeSyntax = parentType;
                    return _lazyDeclaringTypeSyntax;
            }
        }

        return null;
    }

    internal void MarkAsPartialDefinition()
    {
        _isPartialDefinition = true;
    }

    internal void MarkAsPartialImplementation()
    {
        _isPartialImplementation = true;
    }

    private bool ComputeIsUnsafe()
    {
        if (DeclaringSyntaxReferences.IsDefaultOrEmpty)
            return false;

        foreach (var syntaxRef in DeclaringSyntaxReferences)
        {
            var syntax = syntaxRef.GetSyntax();
            var modifiers = syntax switch
            {
                MethodDeclarationSyntax methodDeclaration => methodDeclaration.Modifiers,
                ConstructorDeclarationSyntax constructorDeclaration => constructorDeclaration.Modifiers,
                ParameterlessConstructorDeclarationSyntax initDeclaration => initDeclaration.Modifiers,
                InitializerBlockDeclarationSyntax initBlockDeclaration => initBlockDeclaration.Modifiers,
                FinallyDeclarationSyntax finalDeclaration => finalDeclaration.Modifiers,
                OperatorDeclarationSyntax operatorDeclaration => operatorDeclaration.Modifiers,
                ConversionOperatorDeclarationSyntax conversionDeclaration => conversionDeclaration.Modifiers,
                FunctionStatementSyntax functionStatement => functionStatement.Modifiers,
                AccessorDeclarationSyntax accessorDeclaration => accessorDeclaration.Modifiers,
                _ => SyntaxTokenList.Empty
            };

            if (modifiers.Any(m => m.Kind == SyntaxKind.UnsafeKeyword))
                return true;

            if (syntax is TypeDeclarationSyntax { ParameterList: not null } typeDecl &&
                MethodKind == MethodKind.Constructor &&
                typeDecl.Members
                    .OfType<InitializerBlockDeclarationSyntax>()
                    .Any(b => b.Modifiers.Any(m => m.Kind == SyntaxKind.UnsafeKeyword)))
            {
                return true;
            }
        }

        return false;
    }

    public override ImmutableArray<AttributeData> GetAttributes()
    {
        var addSetsRequiredMembers = ShouldAddSetsRequiredMembersAttribute();
        var addCompilerGenerated = IsAutoPropertyAccessor || IsSynthesizedValueMember;
        var addReadOnly = IsReadOnly && ContainingType?.IsReadOnly != true;

        if (!_declaredInExtension && !addCompilerGenerated && !addReadOnly && !addSetsRequiredMembers)
            return base.GetAttributes();

        if (_lazyAugmentedAttributes.IsDefault)
        {
            var baseAttributes = base.GetAttributes();
            var builder = ImmutableArray.CreateBuilder<AttributeData>();

            if (addCompilerGenerated)
            {
                var compilerGenerated = CreateCompilerGeneratedAttribute();
                if (compilerGenerated is not null)
                    builder.Add(compilerGenerated);
            }

            if (addReadOnly && !baseAttributes.Any(attribute => attribute.AttributeClass.ToFullyQualifiedMetadataName() ==
                "System.Runtime.CompilerServices.IsReadOnlyAttribute"))
            {
                var readOnly = CreateIsReadOnlyAttribute();
                if (readOnly is not null)
                    builder.Add(readOnly);
            }

            if (_declaredInExtension && IsExtensionMethod)
            {
                var extensionAttribute = CreateExtensionAttributeData();
                if (extensionAttribute is not null)
                    builder.Add(extensionAttribute);
            }

            if (addSetsRequiredMembers)
            {
                var setsRequiredMembers = CreateSetsRequiredMembersAttributeData();
                if (setsRequiredMembers is not null)
                    builder.Add(setsRequiredMembers);
            }

            _lazyAugmentedAttributes = builder.Count == 0
                ? baseAttributes
                : baseAttributes.AddRange(builder.ToImmutable());
        }

        return _lazyAugmentedAttributes;
    }

    private bool ShouldAddSetsRequiredMembersAttribute()
    {
        // Explicitly marked (e.g. by binders for normal constructors)
        if (SetsRequiredMembers)
            return true;

        // Records: the primary constructor is synthesized and does not go through
        // TypeMemberBinder.BindConstructorSymbol. For now we conservatively add the
        // metadata to constructors on record types that contain at least one required
        // member. Later steps will implement full initialization semantics and checks.
        if (MethodKind is not MethodKind.Constructor)
            return false;

        var containingType = ContainingType;
        if (containingType is null)
            return false;

        if (!IsRecordType(containingType))
            return false;

        return HasRequiredMembers(containingType);

        static bool HasRequiredMembers(NamedTypeSymbol type)
        {
            foreach (var member in type.GetMembers())
            {
                if (member is SourceFieldSymbol field && field.IsRequired)
                    return true;
                if (member is SourcePropertySymbol prop && prop.IsRequired)
                    return true;
            }

            return false;
        }

        static bool IsRecordType(NamedTypeSymbol type)
        {
            // Prefer a direct API if the symbol model provides it.
            var isRecordProperty = type.GetType().GetProperty("IsRecord");
            if (isRecordProperty is not null && isRecordProperty.PropertyType == typeof(bool))
            {
                try
                {
                    return (bool)isRecordProperty.GetValue(type)!;
                }
                catch
                {
                    // ignore and fall back
                }
            }

            foreach (var r in type.DeclaringSyntaxReferences)
            {
                var syntax = r.GetSyntax();
                if (syntax is RecordDeclarationSyntax)
                    return true;

                if (syntax is ClassDeclarationSyntax cd && cd.Modifiers.Any(x => x.Kind == SyntaxKind.RecordKeyword))
                    return true;
            }

            return false;
        }
    }

    private AttributeData? CreateSetsRequiredMembersAttributeData()
    {
        var compilation = GetDeclaringCompilation();
        if (compilation is null)
            return null;

        var attributeType = compilation.GetTypeByMetadataName("System.Diagnostics.CodeAnalysis.SetsRequiredMembersAttribute");
        if (attributeType is null)
            return null;

        var constructor = attributeType.Constructors.FirstOrDefault(c => !c.IsStatic && c.Parameters.Length == 0);
        if (constructor is null)
            return null;

        var syntaxReference = DeclaringSyntaxReferences.FirstOrDefault();
        return new AttributeData(
            attributeType,
            constructor,
            ImmutableArray<TypedConstant>.Empty,
            ImmutableArray<KeyValuePair<string, TypedConstant>>.Empty,
            syntaxReference);
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

    public MethodSymbol Construct(params TypeSymbol[] typeArguments)
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
        var seenAttributes = new Dictionary<AttributeTargets, HashSet<NamedTypeSymbol>>();
        const AttributeTargets defaultTarget = AttributeTargets.ReturnValue;

        foreach (var syntaxReference in DeclaringSyntaxReferences)
        {
            if (!TryGetCurrentDeclaringSyntax(compilation, syntaxReference, out var syntax, out var semanticModel))
                continue;

            var declarationReturnTargetLists = syntax switch
            {
                MethodDeclarationSyntax method => method.AttributeLists
                    .Where(static list => string.Equals(list.Target?.Identifier.ValueText, "return", StringComparison.OrdinalIgnoreCase)),
                FunctionStatementSyntax function => function.AttributeLists
                    .Where(static list => string.Equals(list.Target?.Identifier.ValueText, "return", StringComparison.OrdinalIgnoreCase)),
                _ => Enumerable.Empty<AttributeListSyntax>()
            };

            var returnClause = syntax switch
            {
                MethodDeclarationSyntax method => method.ReturnType,
                FunctionStatementSyntax function => function.ReturnType,
                ParenthesizedFunctionExpressionSyntax lambda => lambda.ReturnType,
                _ => null
            };

            IEnumerable<AttributeSyntax> returnAttributes = declarationReturnTargetLists.SelectMany(static list => list.Attributes);
            if (returnClause is not null && returnClause.AttributeLists.Count > 0)
                returnAttributes = returnAttributes.Concat(returnClause.AttributeLists.SelectMany(static list => list.Attributes));

            foreach (var attribute in returnAttributes)
            {
                if (attribute.IsMacroAttribute())
                    continue;

                var binderNode = (SyntaxNode?)attribute.Parent ?? attribute;
                var binder = semanticModel.GetBinder(binderNode);
                var attributeBinder = binder as AttributeBinder ?? new AttributeBinder(this, binder);
                var boundAttribute = attributeBinder.BindAttribute(attribute);
                var data = AttributeDataFactory.Create(boundAttribute, attribute);
                if (data is null)
                    continue;

                if (AttributeUsageHelper.TryValidateAttribute(
                        compilation,
                        attributeBinder,
                        this,
                        attribute,
                        data,
                        defaultTarget,
                        seenAttributes,
                        out var appliedTarget) &&
                    appliedTarget == AttributeTargets.ReturnValue)
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

        var hasReceiverParameter = !Parameters.IsDefaultOrEmpty && Parameters.Length > 0;

        foreach (var syntaxReference in DeclaringSyntaxReferences)
        {
            if (syntaxReference.GetSyntax() is not MethodDeclarationSyntax method)
                continue;

            hasReceiverParameter |= method.ParameterList.Parameters.Count > 0;

            foreach (var attribute in method.AttributeLists.SelectMany(static list => list.Attributes))
            {
                if (attribute.IsMacroAttribute())
                    continue;

                if (IsExtensionAttributeName(attribute.Name))
                    return hasReceiverParameter;
            }
        }

        return false;
    }

    private bool ComputeIsPrimaryConstructorSymbol()
    {
        if (MethodKind != MethodKind.Constructor || IsStatic)
            return false;

        return GetDeclaringTypeSyntax()?.ParameterList is not null;
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

    internal void MarkSetsRequiredMembers()
    {
        SetsRequiredMembers = true;
    }
}
