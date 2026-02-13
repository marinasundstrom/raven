using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal partial class TypeMemberBinder : Binder
{
    private readonly INamedTypeSymbol _containingType;
    private readonly TypeSyntax? _extensionReceiverTypeSyntax;
    private ITypeSymbol? _extensionReceiverType;
    private bool _extensionReceiverTypeComputed;

    private static readonly SymbolDisplayFormat TypeNameDiagnosticFormat =
        SymbolDisplayFormat.MinimallyQualifiedFormat.WithGenericsOptions(
            SymbolDisplayGenericsOptions.IncludeTypeParameters |
            SymbolDisplayGenericsOptions.IncludeVariance);

    public TypeMemberBinder(Binder parent, INamedTypeSymbol containingType, TypeSyntax? extensionReceiverTypeSyntax = null)
        : base(parent, parent.Diagnostics)
    {
        _containingType = containingType;
        _extensionReceiverTypeSyntax = extensionReceiverTypeSyntax;
    }

    public override INamedTypeSymbol ContainingSymbol => _containingType;

    public override ISymbol? LookupSymbol(string name)
    {
        var symbol = _containingType.GetMembers(name).FirstOrDefault();
        if (symbol is not null)
            return symbol;

        return base.LookupSymbol(name);
    }

    public override ITypeSymbol? LookupType(string name)
    {
        var typeParameter = _containingType.TypeParameters.FirstOrDefault(tp => tp.Name == name);
        if (typeParameter is not null)
            return typeParameter;

        return base.LookupType(name);
    }

    private bool IsExtensionContainer => _extensionReceiverTypeSyntax is not null && _containingType is SourceNamedTypeSymbol { IsExtensionDeclaration: true };

    private ImmutableArray<ITypeParameterSymbol> CreateExtensionTypeParameters(SourceMethodSymbol methodSymbol)
    {
        if (!IsExtensionContainer || _containingType.TypeParameters.IsDefaultOrEmpty)
            return ImmutableArray<ITypeParameterSymbol>.Empty;

        var builder = ImmutableArray.CreateBuilder<ITypeParameterSymbol>(_containingType.TypeParameters.Length);
        var receiverNamespace = CurrentNamespace!.AsSourceNamespace();
        var ordinal = 0;

        foreach (var typeParameter in _containingType.TypeParameters.OfType<SourceTypeParameterSymbol>())
        {
            builder.Add(new SourceTypeParameterSymbol(
                typeParameter.Name,
                methodSymbol,
                _containingType,
                receiverNamespace,
                typeParameter.Locations.ToArray(),
                typeParameter.DeclaringSyntaxReferences.ToArray(),
                ordinal++,
                typeParameter.ConstraintKind,
                typeParameter.ConstraintTypeReferences,
                typeParameter.Variance));
        }

        return builder.ToImmutable();
    }

    private ITypeSymbol GetExtensionReceiverType()
    {
        if (!_extensionReceiverTypeComputed)
        {
            _extensionReceiverType = _extensionReceiverTypeSyntax is null
                ? null
                : ResolveTypeSyntaxForSignature(this, _extensionReceiverTypeSyntax, RefKind.None);
            _extensionReceiverTypeComputed = true;
        }

        return _extensionReceiverType ?? Compilation.ErrorTypeSymbol;
    }

    private ITypeSymbol ResolveTypeSyntaxForSignature(
        Binder binder,
        TypeSyntax typeSyntax,
        RefKind refKindHint,
        Binder.TypeResolutionOptions? options = null)
    {
        var result = binder.BindTypeSyntax(typeSyntax, options);
        if (!result.Success)
            return binder.BindTypeSyntaxDirect(typeSyntax, options, refKindHint: null, allowLegacyFallback: true);

        return result.ResolvedType;
    }

    private ITypeSymbol ResolveParameterTypeSyntaxForSignature(
        Binder binder,
        TypeSyntax typeSyntax,
        RefKind refKind,
        Binder.TypeResolutionOptions? options = null)
    {
        var boundTypeSyntax = refKind.IsByRef() && typeSyntax is ByRefTypeSyntax byRefType
            ? byRefType.ElementType
            : typeSyntax;

        return ResolveTypeSyntaxForSignature(binder, boundTypeSyntax, RefKind.None, options);
    }

    private INamedTypeSymbol? ResolveNamedTypeSyntax(Binder binder, TypeSyntax typeSyntax)
    {
        var result = binder.BindTypeSyntax(typeSyntax);
        if (!result.Success)
            return binder.BindTypeSyntaxDirect(typeSyntax) as INamedTypeSymbol;

        return result.ResolvedType as INamedTypeSymbol;
    }

    public override ISymbol? BindDeclaredSymbol(SyntaxNode node)
    {
        return node switch
        {
            MethodDeclarationSyntax method => BindMethodSymbol(method),
            ConstructorDeclarationSyntax ctor => BindConstructorSymbol(ctor),
            NamedConstructorDeclarationSyntax namedCtor => BindConstructorSymbol(namedCtor),
            OperatorDeclarationSyntax opDecl => BindOperatorSymbol(opDecl),
            ConversionOperatorDeclarationSyntax conversionDecl => BindConversionOperatorSymbol(conversionDecl),
            PropertyDeclarationSyntax property => BindPropertySymbol(property),
            EventDeclarationSyntax @event => BindEventSymbol(@event),
            IndexerDeclarationSyntax indexer => BindIndexerSymbol(indexer),
            AccessorDeclarationSyntax accessor => BindAccessorSymbol(accessor),
            VariableDeclaratorSyntax variable => BindFieldSymbol(variable),
            DelegateDeclarationSyntax del => BindDelegateSymbol(del),
            _ => base.BindDeclaredSymbol(node)
        };
    }

    private ISymbol? BindFieldSymbol(VariableDeclaratorSyntax variable)
    {
        return _containingType.GetMembers()
            .OfType<IFieldSymbol>()
            .FirstOrDefault(f => f.Name == variable.Identifier.ValueText &&
                                 f.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == variable));
    }

    private ISymbol? BindDelegateSymbol(DelegateDeclarationSyntax del)
    {
        return _containingType.GetMembers()
            .OfType<INamedTypeSymbol>()
            .Where(x => x.TypeKind == TypeKind.Delegate)
            .FirstOrDefault(f => f.Name == del.Identifier.ValueText &&
                                 f.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == del));
    }

    private ISymbol? BindMethodSymbol(MethodDeclarationSyntax method)
    {
        var identifierToken = ResolveExplicitInterfaceIdentifier(method.Identifier, method.ExplicitInterfaceSpecifier);
        var name = identifierToken.Kind == SyntaxKind.SelfKeyword ? "Invoke" : identifierToken.ValueText;

        return _containingType.GetMembers()
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m => m.Name == name &&
                                 m.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == method));
    }

    private ISymbol? BindOperatorSymbol(OperatorDeclarationSyntax operatorDecl)
    {
        var parameterCount = operatorDecl.ParameterList.Parameters.Count;
        if (!OperatorFacts.TryGetUserDefinedOperatorInfo(operatorDecl.OperatorToken.Kind, parameterCount, out var operatorInfo))
            return null;

        var metadataName = operatorInfo.MetadataName;

        return _containingType.GetMembers()
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m => m.Name == metadataName &&
                                 m.MethodKind == MethodKind.UserDefinedOperator &&
                                 m.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == operatorDecl));
    }

    private ISymbol? BindConversionOperatorSymbol(ConversionOperatorDeclarationSyntax conversionDecl)
    {
        if (!OperatorFacts.TryGetConversionOperatorMetadataName(conversionDecl.ConversionKindKeyword.Kind, out var metadataName))
            return null;

        return _containingType.GetMembers()
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m => m.Name == metadataName &&
                                 m.MethodKind == MethodKind.Conversion &&
                                 m.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == conversionDecl));
    }

    private ISymbol? BindConstructorSymbol(BaseConstructorDeclarationSyntax ctor)
    {
        string name = ".ctor";
        if (ctor is NamedConstructorDeclarationSyntax namedCtor)
            name = namedCtor.Identifier.ValueText;

        var method = _containingType.GetMembers()
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m => m.Name == name &&
                                 m.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == ctor));

        return method;
    }

    private ISymbol? BindPropertySymbol(PropertyDeclarationSyntax property)
    {
        var identifierToken = ResolveExplicitInterfaceIdentifier(property.Identifier, property.ExplicitInterfaceSpecifier);
        return _containingType.GetMembers()
            .OfType<IPropertySymbol>()
            .FirstOrDefault(p => !p.IsIndexer &&
                                 p.Name == identifierToken.ValueText &&
                                 p.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == property));
    }

    private ISymbol? BindIndexerSymbol(IndexerDeclarationSyntax indexer)
    {
        return _containingType.GetMembers()
            .OfType<IPropertySymbol>()
            .FirstOrDefault(p => p.IsIndexer &&
                                 p.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == indexer));
    }

    private ISymbol? BindAccessorSymbol(AccessorDeclarationSyntax accessor)
    {
        return _containingType.GetMembers()
            .OfType<IPropertySymbol>()
            .SelectMany(p => new[] { p.GetMethod, p.SetMethod }.Where(m => m is not null))
            .Concat(_containingType.GetMembers()
                .OfType<IEventSymbol>()
                .SelectMany(e => new[] { e.AddMethod, e.RemoveMethod }.Where(m => m is not null)))
            .FirstOrDefault(m => m!.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == accessor));
    }

    private ISymbol? BindEventSymbol(EventDeclarationSyntax @event)
    {
        var identifierToken = ResolveExplicitInterfaceIdentifier(@event.Identifier, @event.ExplicitInterfaceSpecifier);
        return _containingType.GetMembers()
            .OfType<IEventSymbol>()
            .FirstOrDefault(e => e.Name == identifierToken.ValueText &&
                                 e.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == @event));
    }

    public void BindFieldDeclaration(FieldDeclarationSyntax fieldDecl)
    {
        var firstDeclaratorName = fieldDecl.Declaration.Declarators.Count > 0
            ? fieldDecl.Declaration.Declarators[0].Identifier.ValueText
            : "<field>";
        ReportPartialModifierNotSupported(fieldDecl.Modifiers, "field", firstDeclaratorName);

        var bindingKeyword = fieldDecl.Declaration.BindingKeyword;
        var isConstDeclaration = bindingKeyword.IsKind(SyntaxKind.ConstKeyword);
        var isStatic = fieldDecl.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword) || isConstDeclaration;
        var hasNewModifier = fieldDecl.Modifiers.Any(m => m.Kind == SyntaxKind.NewKeyword);
        var isRequired = fieldDecl.Modifiers.Any(m => m.IsKind(SyntaxKind.RequiredKeyword));
        var fieldAccessibility = AccessibilityUtilities.DetermineAccessibility(
            fieldDecl.Modifiers,
            AccessibilityUtilities.GetDefaultMemberAccessibility(_containingType));

        foreach (var decl in fieldDecl.Declaration.Declarators)
        {
            ITypeSymbol? fieldType = decl.TypeAnnotation is null
                ? null
                : ResolveTypeSyntaxForSignature(this, decl.TypeAnnotation.Type, RefKind.None);

            BoundExpression? initializer = null;
            object? constantValue = null;
            var constantValueComputed = false;

            if (decl.Initializer is not null)
            {
                var exprBinder = new BlockBinder(_containingType, this);

                var targetType = exprBinder.PushTargetType(fieldType);

                initializer = exprBinder.BindExpression(decl.Initializer.Value);

                targetType.Dispose();

                foreach (var diag in exprBinder.Diagnostics.AsEnumerable())
                    _diagnostics.Report(diag);

                if (decl.TypeAnnotation is null && initializer?.Type is { } inferred)
                    fieldType = TypeSymbolNormalization.NormalizeForInference(inferred);

                fieldType ??= Compilation.GetSpecialType(SpecialType.System_Object);

                if (isConstDeclaration && initializer is not BoundErrorExpression)
                {
                    if (!ConstantValueEvaluator.TryEvaluate(decl.Initializer.Value, out var evaluated))
                    {
                        _diagnostics.ReportConstFieldMustBeConstant(decl.Identifier.ValueText, decl.Initializer.Value.GetLocation());
                    }
                    else if (!ConstantValueEvaluator.TryConvert(fieldType, evaluated, out constantValue))
                    {
                        var display = fieldType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
                        _diagnostics.ReportConstFieldCannotConvert(decl.Identifier.ValueText, display, decl.Initializer.Value.GetLocation());
                    }
                    else
                    {
                        constantValueComputed = true;
                        initializer = null;
                    }
                }
            }
            else
            {
                if (isConstDeclaration)
                    _diagnostics.ReportConstFieldRequiresInitializer(decl.Identifier.ValueText, decl.Identifier.GetLocation());
            }

            fieldType ??= Compilation.GetSpecialType(SpecialType.System_Object);

            var isConst = isConstDeclaration && constantValueComputed;
            var initializerForSymbol = isConst ? null : initializer;
            var constantValueForSymbol = isConst ? constantValue : null;
            var isMutable = bindingKeyword.Kind == SyntaxKind.VarKeyword;

            // Required fields must be mutable (assignable from an object initializer or equivalent init semantics).
            if (isRequired && !isMutable)
            {
                _diagnostics.ReportRequiredFieldMustBeMutable(decl.Identifier.ValueText, decl.Identifier.GetLocation());
            }

            var fieldTypeLocation = decl.TypeAnnotation?.Type.GetLocation() ?? decl.Identifier.GetLocation();
            ValidateTypeAccessibility(
                fieldType,
                fieldAccessibility,
                "field",
                GetMemberDisplayName(decl.Identifier.ValueText),
                $"field '{decl.Identifier.ValueText}'",
                fieldTypeLocation);

            ReportInstanceMemberInStaticTypeIfNeeded(
                isStatic,
                decl.Identifier.ValueText,
                decl.Identifier.GetLocation());

            if (!IsExtensionContainer)
            {
                var hiddenMember = FindHidingFieldCandidate(decl.Identifier.ValueText, isStatic, fieldType);
                ReportMemberHidingIfNeeded(hiddenMember, decl.Identifier.ValueText, hasNewModifier, decl.Identifier.GetLocation());
            }

            var f = new SourceFieldSymbol(
                decl.Identifier.ValueText,
                fieldType,
                isStatic: isStatic,
                isMutable: isMutable,
                isConst: isConst,
                constantValue: constantValueForSymbol,
                _containingType,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [decl.GetLocation()],
                [decl.GetReference(), fieldDecl.GetReference()],
                initializerForSymbol,
                declaredAccessibility: fieldAccessibility
            );

            if (isRequired)
                f.MarkAsRequired();

            return;
        }
    }

    public MethodBinder BindMethodDeclaration(MethodDeclarationSyntax methodDecl)
    {
        var explicitInterfaceSpecifier = methodDecl.ExplicitInterfaceSpecifier;
        var identifierToken = ResolveExplicitInterfaceIdentifier(methodDecl.Identifier, explicitInterfaceSpecifier);
        var name = identifierToken.Kind == SyntaxKind.SelfKeyword ? "Invoke" : identifierToken.ValueText;
        ReportPartialModifierNotSupported(methodDecl.Modifiers, "method", name);
        INamedTypeSymbol? explicitInterfaceType = null;
        IMethodSymbol? explicitInterfaceMember = null;

        var metadataName = name;
        var displayName = name;
        var isExtensionContainer = IsExtensionContainer;

        if (explicitInterfaceSpecifier is not null)
        {
            var resolved = ResolveNamedTypeSyntax(this, explicitInterfaceSpecifier.Name);
            if (resolved is INamedTypeSymbol interfaceType && interfaceType.TypeKind == TypeKind.Interface)
            {
                explicitInterfaceType = interfaceType;
                var interfaceMetadataName = GetInterfaceMetadataName(interfaceType);
                metadataName = $"{interfaceMetadataName}.{name}";
                displayName = $"{interfaceType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat)}.{name}";

                if (!ImplementsInterface(interfaceType))
                {
                    _diagnostics.ReportContainingTypeDoesNotImplementInterface(
                        _containingType.Name,
                        interfaceType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        explicitInterfaceSpecifier.Name.GetLocation());
                }
            }
            else
            {
                _diagnostics.ReportExplicitInterfaceSpecifierMustBeInterface(explicitInterfaceSpecifier.Name.GetLocation());
            }
        }

        var paramInfos = new List<(string name, TypeSyntax typeSyntax, RefKind refKind, ParameterSyntax syntax, bool isMutable)>();
        foreach (var p in methodDecl.ParameterList.Parameters)
        {
            var typeSyntax = p.TypeAnnotation!.Type;
            var refKindTokenKind = p.RefKindKeyword?.Kind;
            var refKind = typeSyntax is ByRefTypeSyntax
                ? refKindTokenKind switch
                {
                    SyntaxKind.OutKeyword => RefKind.Out,
                    SyntaxKind.InKeyword => RefKind.In,
                    SyntaxKind.RefKeyword => RefKind.Ref,
                    _ => RefKind.Ref,
                }
                : refKindTokenKind switch
                {
                    SyntaxKind.OutKeyword => RefKind.Out,
                    SyntaxKind.InKeyword => RefKind.In,
                    SyntaxKind.RefKeyword => RefKind.Ref,
                    _ => RefKind.None,
                };

            var isMutable = p.BindingKeyword?.Kind == SyntaxKind.VarKeyword;
            paramInfos.Add((p.Identifier.ValueText, typeSyntax, refKind, p, isMutable));
        }

        var modifiers = methodDecl.Modifiers;
        var hasStaticModifier = modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);
        var isStatic = hasStaticModifier;
        var isAsync = modifiers.Any(m => m.Kind == SyntaxKind.AsyncKeyword);
        var isVirtual = modifiers.Any(m => m.Kind == SyntaxKind.VirtualKeyword);
        var isOverride = modifiers.Any(m => m.Kind == SyntaxKind.OverrideKeyword);
        var isSealed = modifiers.Any(m => m.Kind is SyntaxKind.SealedKeyword or SyntaxKind.FinalKeyword);
        var isAbstract = modifiers.Any(m => m.Kind == SyntaxKind.AbstractKeyword);
        var isExtern = modifiers.Any(m => m.Kind == SyntaxKind.ExternKeyword);
        var hasNewModifier = modifiers.Any(m => m.Kind == SyntaxKind.NewKeyword);
        var defaultAccessibility = AccessibilityUtilities.GetDefaultMemberAccessibility(_containingType);
        var methodAccessibility = AccessibilityUtilities.DetermineAccessibility(modifiers, defaultAccessibility);

        if (isExtensionContainer)
        {
            isStatic = true;
            isVirtual = false;
            isOverride = false;
            isSealed = false;
            isAbstract = false;
            methodAccessibility = modifiers.Any(m => m.Kind == SyntaxKind.InternalKeyword)
                ? Accessibility.Internal
                : Accessibility.Public;
        }

        if (explicitInterfaceType is not null)
        {
            isStatic = false;
            isVirtual = false;
            isOverride = false;
            isSealed = false;
            isAbstract = false;
            methodAccessibility = Accessibility.Private;
        }

        ReportInstanceMemberInStaticTypeIfNeeded(
            isStatic,
            displayName,
            identifierToken.GetLocation());

        ValidateInheritanceModifiers(
            ref isAbstract,
            ref isVirtual,
            ref isOverride,
            ref isSealed,
            isStatic,
            displayName,
            identifierToken.GetLocation());

        if (isSealed && !isOverride)
        {
            _diagnostics.ReportSealedMemberMustOverride(name, identifierToken.GetLocation());
            isSealed = false;
        }

        if (isAbstract && (methodDecl.Body is not null || methodDecl.ExpressionBody is not null))
        {
            _diagnostics.ReportAbstractMemberCannotHaveBody(name, identifierToken.GetLocation());
        }

        if (isExtern && !isStatic)
        {
            _diagnostics.ReportExternMemberMustBeStatic(name, identifierToken.GetLocation());
            isStatic = true;
        }

        if (isExtern && (methodDecl.Body is not null || methodDecl.ExpressionBody is not null))
        {
            _diagnostics.ReportExternMemberCannotHaveBody(name, identifierToken.GetLocation());
        }

        ValidateAbstractMemberInNonAbstractType(
            isAbstract,
            GetMemberDisplayName(displayName),
            identifierToken.GetLocation());

        if (isStatic && (isVirtual || isOverride))
        {
            if (isVirtual)
                _diagnostics.ReportStaticMemberCannotBeVirtualOrOverride(name, "virtual", identifierToken.GetLocation());
            if (isOverride)
                _diagnostics.ReportStaticMemberCannotBeVirtualOrOverride(name, "override", identifierToken.GetLocation());

            isVirtual = false;
            isOverride = false;
            isSealed = false;
            isAbstract = false;
        }

        if (isVirtual && !isOverride && _containingType.IsSealed)
        {
            _diagnostics.ReportVirtualMemberInSealedType(name, _containingType.Name, identifierToken.GetLocation());
            isVirtual = false;
        }

        IMethodSymbol? overriddenMethod = null;

        var methodKind = explicitInterfaceType is not null ? MethodKind.ExplicitInterfaceImplementation : MethodKind.Ordinary;

        var defaultReturnType = isAsync
            ? Compilation.GetSpecialType(SpecialType.System_Threading_Tasks_Task)
            : Compilation.GetSpecialType(SpecialType.System_Unit);

        var methodSymbol = new SourceMethodSymbol(
            metadataName,
            defaultReturnType,
            ImmutableArray<SourceParameterSymbol>.Empty,
            _containingType,
            _containingType,
            CurrentNamespace!.AsSourceNamespace(),
            [methodDecl.GetLocation()],
            [methodDecl.GetReference()],
            isStatic: isStatic,
            methodKind: methodKind,
            isAsync: isAsync,
            isVirtual: isVirtual,
            isOverride: isOverride,
            isSealed: isSealed,
            isAbstract: isAbstract,
            isExtern: isExtern,
            declaredAccessibility: methodAccessibility);

        var isExtensionMember = isExtensionContainer && !hasStaticModifier;

        if (isExtensionMember)
            methodSymbol.MarkDeclaredInExtension();

        if (isAsync && methodDecl.ReturnType is null)
            methodSymbol.RequireAsyncReturnTypeInference();

        // Bind method-declared type parameters first (e.g. <B>)
        InitializeMethodTypeParameters(methodSymbol, methodDecl.TypeParameterList);

        // If we are inside an extension container, the lowering pipeline should already have
        // projected the extension container type parameters onto the method as method-owned
        // type parameters. We still need a substitution map so we can bind the receiver type
        // syntax (e.g. IEnumerable<T>) against the *method's* type parameter identity.
        IReadOnlyDictionary<string, ITypeSymbol>? extensionTypeParameterSubstitutions = null;

        if (IsExtensionContainer && !_containingType.TypeParameters.IsDefaultOrEmpty)
        {
            var methodTypeParameters = methodSymbol.TypeParameters;

            // Expected invariant: extension type params are the first method type params.
            if (!methodTypeParameters.IsDefaultOrEmpty &&
                methodTypeParameters.Length >= _containingType.TypeParameters.Length)
            {
                var map = new Dictionary<string, ITypeSymbol>(StringComparer.Ordinal);
                for (int i = 0; i < _containingType.TypeParameters.Length; i++)
                    map[_containingType.TypeParameters[i].Name] = methodTypeParameters[i];

                extensionTypeParameterSubstitutions = map;
            }
            else
            {
                // Fallback: only if method doesn't already have the lowered extension TPs
                var loweredExtensionTypeParameters = CreateExtensionTypeParameters(methodSymbol);
                if (!loweredExtensionTypeParameters.IsDefaultOrEmpty)
                {
                    var existing = methodSymbol.TypeParameters;
                    var merged = ImmutableArray.CreateBuilder<ITypeParameterSymbol>(
                        loweredExtensionTypeParameters.Length + existing.Length);

                    merged.AddRange(loweredExtensionTypeParameters);
                    merged.AddRange(existing);

                    methodSymbol.SetTypeParameters(merged.ToImmutable());

                    var map = new Dictionary<string, ITypeSymbol>(StringComparer.Ordinal);
                    var count = Math.Min(_containingType.TypeParameters.Length, loweredExtensionTypeParameters.Length);
                    for (int i = 0; i < count; i++)
                        map[_containingType.TypeParameters[i].Name] = loweredExtensionTypeParameters[i];

                    extensionTypeParameterSubstitutions = map;
                }
            }
        }

        var hasInvalidAsyncReturnType = false;

        var methodBinder = new MethodBinder(methodSymbol, this);
        methodBinder.EnsureTypeParameterConstraintTypesResolved(methodSymbol.TypeParameters);

        var returnType = methodDecl.ReturnType is null
            ? defaultReturnType
            : ResolveTypeSyntaxForSignature(methodBinder, methodDecl.ReturnType.Type, RefKind.None);

        if (isAsync && methodDecl.ReturnType is { } annotatedReturn && !IsValidAsyncReturnType(returnType))
        {
            var display = returnType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
            _diagnostics.ReportAsyncReturnTypeMustBeTaskLike(display, annotatedReturn.Type.GetLocation());
            returnType = Compilation.GetSpecialType(SpecialType.System_Threading_Tasks_Task);
            hasInvalidAsyncReturnType = true;
        }

        var resolvedParamInfos = new List<(string name, ITypeSymbol type, RefKind refKind, ParameterSyntax syntax, bool isMutable)>();
        foreach (var (paramName, typeSyntax, refKind, syntax, isMutable) in paramInfos)
        {
            var resolvedType = ResolveParameterTypeSyntaxForSignature(methodBinder, typeSyntax, refKind);
            resolvedParamInfos.Add((paramName, resolvedType, refKind, syntax, isMutable));
        }

        ITypeSymbol? receiverType = null;
        if (isExtensionMember && _extensionReceiverTypeSyntax is not null)
        {
            receiverType = ResolveTypeSyntaxForSignature(
                methodBinder,
                _extensionReceiverTypeSyntax,
                RefKind.None,
                extensionTypeParameterSubstitutions is null
                    ? null
                    : new Binder.TypeResolutionOptions
                    {
                        TypeParameterSubstitutions = extensionTypeParameterSubstitutions,
                        SubstitutionPrecedence = Binder.SubstitutionPrecedence.OptionsWin
                    });
        }

        var signatureParameters = resolvedParamInfos.Select(p => (p.type, p.refKind)).ToList();
        if (receiverType is not null)
            signatureParameters.Insert(0, (receiverType, RefKind.None));
        var signatureArray = signatureParameters.ToArray();

        ValidateTypeAccessibility(
            returnType,
            methodAccessibility,
            GetMethodKindDisplay(methodKind),
            GetMemberDisplayName(displayName),
            "return",
            methodDecl.ReturnType?.Type.GetLocation() ?? methodDecl.Identifier.GetLocation());

        if (receiverType is not null && _extensionReceiverTypeSyntax is not null)
        {
            ValidateTypeAccessibility(
                receiverType,
                methodAccessibility,
                GetMethodKindDisplay(methodKind),
                GetMemberDisplayName(displayName),
                "receiver",
                _extensionReceiverTypeSyntax.GetLocation());
        }

        foreach (var (paramName, paramType, _, syntax, _) in resolvedParamInfos)
        {
            ValidateTypeAccessibility(
                paramType,
                methodAccessibility,
                GetMethodKindDisplay(methodKind),
                GetMemberDisplayName(displayName),
                $"parameter '{paramName}'",
                syntax.TypeAnnotation!.Type.GetLocation());
        }

        if (explicitInterfaceType is not null)
        {
            explicitInterfaceMember = FindExplicitInterfaceImplementation(
                explicitInterfaceType,
                name,
                returnType,
                signatureArray);

            if (explicitInterfaceMember is null)
            {
                _diagnostics.ReportExplicitInterfaceMemberNotFound(
                    explicitInterfaceType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    name,
                    identifierToken.GetLocation());
            }
        }

        if (explicitInterfaceType is null && isOverride)
        {
            var candidate = FindOverrideCandidate(name, signatureArray);

            var name2 = CreateSignature(name, returnType, signatureArray);

            if (candidate is null || !candidate.IsVirtual)
            {
                _diagnostics.ReportOverrideMemberNotFound(methodSymbol.ToDisplayString(), "method", identifierToken.GetLocation());
                isOverride = false;
                isVirtual = false;
                isSealed = false;
            }
            else if (candidate.IsFinal)
            {
                _diagnostics.ReportCannotOverrideSealedMember(name, candidate.Name, identifierToken.GetLocation());
                isOverride = false;
                isVirtual = false;
                isSealed = false;
            }
            else
            {
                overriddenMethod = candidate;
                isVirtual = true;
            }
        }

        methodSymbol.UpdateModifiers(isVirtual, isOverride, isSealed, isAbstract);

        if (explicitInterfaceType is null && !isOverride && !isExtensionContainer)
        {
            var hiddenMember = FindHidingMethodCandidate(name, isStatic, signatureArray);
            ReportMemberHidingIfNeeded(hiddenMember, name, hasNewModifier, identifierToken.GetLocation());
        }

        CheckForDuplicateSignature(metadataName, displayName, signatureArray, identifierToken.GetLocation(), methodDecl);

        if (overriddenMethod is not null)
            methodSymbol.SetOverriddenMethod(overriddenMethod);

        if (explicitInterfaceMember is not null)
            methodSymbol.SetExplicitInterfaceImplementations(ImmutableArray.Create(explicitInterfaceMember));

        var parameters = new List<SourceParameterSymbol>();
        var seenOptionalParameter = false;

        if (isExtensionMember && receiverType is not null && _extensionReceiverTypeSyntax is not null)
        {
            var receiverNamespace = CurrentNamespace!.AsSourceNamespace();
            var selfParameter = new SourceParameterSymbol(
                "self",
                receiverType,
                methodSymbol,
                _containingType,
                receiverNamespace,
                [_extensionReceiverTypeSyntax.GetLocation()],
                [_extensionReceiverTypeSyntax.GetReference()]);
            parameters.Add(selfParameter);
        }

        foreach (var (paramName, paramType, refKind, syntax, isMutable) in resolvedParamInfos)
        {
            var defaultResult = ProcessParameterDefault(
                syntax,
                paramType,
                paramName,
                _diagnostics,
                ref seenOptionalParameter);
            var pSymbol = new SourceParameterSymbol(
                paramName,
                paramType,
                methodSymbol,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [syntax.GetLocation()],
                [syntax.GetReference()],
                refKind,
                defaultResult.HasExplicitDefaultValue,
                defaultResult.ExplicitDefaultValue,
                isMutable
            );
            parameters.Add(pSymbol);
        }

        methodSymbol.SetReturnType(returnType);
        methodSymbol.SetParameters(parameters);

        if (hasInvalidAsyncReturnType)
            methodSymbol.MarkAsyncReturnTypeError();
        return methodBinder;
    }

    private string CreateSignature(
        string name,
        ITypeSymbol returnType,
        (ITypeSymbol type, RefKind refKind)[] signatureArray)
    {
        static string RefPrefix(RefKind refKind) => refKind switch
        {
            RefKind.None => string.Empty,
            RefKind.Ref => "ref ",
            RefKind.Out => "out ",
            RefKind.In => "in ",
            RefKind.RefReadOnly => "ref readonly ",
            RefKind.RefReadOnlyParameter => "ref readonly ",
            _ => string.Empty
        };

        var paramStrings = signatureArray.Select(p =>
            RefPrefix(p.refKind) +
            p.type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat));

        var paramsString = string.Join(", ", paramStrings);

        var returnString = returnType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);

        return $"{name}({paramsString}) -> {returnString}";
    }

    public MethodBinder BindOperatorDeclaration(OperatorDeclarationSyntax operatorDecl)
    {
        var operatorText = OperatorFacts.GetDisplayText(operatorDecl.OperatorToken.Kind);
        ReportPartialModifierNotSupported(operatorDecl.Modifiers, "operator", operatorText);
        var defaultAccessibility = AccessibilityUtilities.GetDefaultMemberAccessibility(_containingType);
        var operatorAccessibility = AccessibilityUtilities.DetermineAccessibility(operatorDecl.Modifiers, defaultAccessibility);
        var hasStaticModifier = operatorDecl.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);

        if (_containingType.TypeKind is not TypeKind.Class and not TypeKind.Struct)
            _diagnostics.ReportOperatorDeclarationMustBeInClassOrStruct(operatorText, operatorDecl.OperatorKeyword.GetLocation());

        if (!hasStaticModifier)
            _diagnostics.ReportOperatorMustBeStatic(operatorText, operatorDecl.OperatorKeyword.GetLocation());

        if (operatorAccessibility != Accessibility.Public)
        {
            _diagnostics.ReportOperatorMustBePublic(operatorText, operatorDecl.OperatorKeyword.GetLocation());
            operatorAccessibility = Accessibility.Public;
        }

        var parameterCount = operatorDecl.ParameterList.Parameters.Count;

        if (!OperatorFacts.TryGetUserDefinedOperatorInfo(operatorDecl.OperatorToken.Kind, parameterCount, out var operatorInfo))
        {
            var expectedParameters = OperatorFacts.GetExpectedParameterCountDescription(operatorDecl.OperatorToken.Kind);
            _diagnostics.ReportOperatorParameterCountInvalid(operatorText, expectedParameters, operatorDecl.ParameterList.GetLocation());

            operatorInfo = new UserDefinedOperatorInfo(
                operatorText,
                parameterCount == 1 ? OperatorArity.Unary : OperatorArity.Binary);
        }

        var displayName = $"operator {operatorText}";
        var defaultReturnType = Compilation.GetSpecialType(SpecialType.System_Unit);

        var operatorSymbol = new SourceMethodSymbol(
            operatorInfo.MetadataName,
            defaultReturnType,
            ImmutableArray<SourceParameterSymbol>.Empty,
            _containingType,
            _containingType,
            CurrentNamespace!.AsSourceNamespace(),
            [operatorDecl.GetLocation()],
            [operatorDecl.GetReference()],
            isStatic: true,
            methodKind: MethodKind.UserDefinedOperator,
            declaredAccessibility: operatorAccessibility);

        InitializeMethodTypeParameters(operatorSymbol, typeParameterList: null);

        var operatorBinder = new MethodBinder(operatorSymbol, this);

        var returnType = operatorDecl.ReturnType is null
            ? defaultReturnType
            : ResolveTypeSyntaxForSignature(operatorBinder, operatorDecl.ReturnType.Type, RefKind.None);

        var resolvedParamInfos = new List<(string name, ITypeSymbol type, RefKind refKind, ParameterSyntax syntax, bool isMutable)>();
        foreach (var p in operatorDecl.ParameterList.Parameters)
        {
            var typeSyntax = p.TypeAnnotation!.Type;
            var refKindTokenKind = p.RefKindKeyword?.Kind;
            var refKind = typeSyntax is ByRefTypeSyntax
                ? refKindTokenKind switch
                {
                    SyntaxKind.OutKeyword => RefKind.Out,
                    SyntaxKind.InKeyword => RefKind.In,
                    SyntaxKind.RefKeyword => RefKind.Ref,
                    _ => RefKind.Ref,
                }
                : refKindTokenKind switch
                {
                    SyntaxKind.OutKeyword => RefKind.Out,
                    SyntaxKind.InKeyword => RefKind.In,
                    SyntaxKind.RefKeyword => RefKind.Ref,
                    _ => RefKind.None,
                };

            var pType = ResolveParameterTypeSyntaxForSignature(operatorBinder, typeSyntax, refKind);
            var isMutable = p.BindingKeyword?.Kind == SyntaxKind.VarKeyword;
            resolvedParamInfos.Add((p.Identifier.ValueText, pType, refKind, p, isMutable));
        }

        ValidateTypeAccessibility(
            returnType,
            operatorAccessibility,
            "operator",
            GetMemberDisplayName(displayName),
            "return",
            operatorDecl.ReturnType?.Type.GetLocation() ?? operatorDecl.OperatorToken.GetLocation());

        foreach (var (paramName, paramType, _, syntax, _) in resolvedParamInfos)
        {
            ValidateTypeAccessibility(
                paramType,
                operatorAccessibility,
                "operator",
                GetMemberDisplayName(displayName),
                $"parameter '{paramName}'",
                syntax.TypeAnnotation!.Type.GetLocation());
        }

        var signatureArray = resolvedParamInfos.Select(p => (p.type, p.refKind)).ToArray();
        CheckForDuplicateSignature(operatorInfo.MetadataName, displayName, signatureArray, operatorDecl.OperatorToken.GetLocation(), operatorDecl);

        var parameters = new List<SourceParameterSymbol>();
        var seenOptionalParameter = false;
        foreach (var (paramName, paramType, refKind, syntax, isMutable) in resolvedParamInfos)
        {
            var defaultResult = ProcessParameterDefault(
                syntax,
                paramType,
                paramName,
                _diagnostics,
                ref seenOptionalParameter);
            var pSymbol = new SourceParameterSymbol(
                paramName,
                paramType,
                operatorSymbol,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [syntax.GetLocation()],
                [syntax.GetReference()],
                refKind,
                defaultResult.HasExplicitDefaultValue,
                defaultResult.ExplicitDefaultValue,
                isMutable
            );
            parameters.Add(pSymbol);
        }

        operatorSymbol.SetReturnType(returnType);
        operatorSymbol.SetParameters(parameters);

        return operatorBinder;
    }

    public MethodBinder BindConversionOperatorDeclaration(ConversionOperatorDeclarationSyntax conversionDecl)
    {
        var operatorText = OperatorFacts.GetConversionOperatorDisplayText(conversionDecl.ConversionKindKeyword.Kind);
        ReportPartialModifierNotSupported(conversionDecl.Modifiers, "operator", operatorText);
        var defaultAccessibility = AccessibilityUtilities.GetDefaultMemberAccessibility(_containingType);
        var operatorAccessibility = AccessibilityUtilities.DetermineAccessibility(conversionDecl.Modifiers, defaultAccessibility);
        var hasStaticModifier = conversionDecl.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);

        if (_containingType.TypeKind is not TypeKind.Class and not TypeKind.Struct)
            _diagnostics.ReportOperatorDeclarationMustBeInClassOrStruct(operatorText, conversionDecl.ConversionKindKeyword.GetLocation());

        if (!hasStaticModifier)
            _diagnostics.ReportOperatorMustBeStatic(operatorText, conversionDecl.ConversionKindKeyword.GetLocation());

        if (operatorAccessibility != Accessibility.Public)
        {
            _diagnostics.ReportOperatorMustBePublic(operatorText, conversionDecl.ConversionKindKeyword.GetLocation());
            operatorAccessibility = Accessibility.Public;
        }

        var parameterCount = conversionDecl.ParameterList.Parameters.Count;

        if (parameterCount != 1)
            _diagnostics.ReportOperatorParameterCountInvalid(operatorText, "1", conversionDecl.ParameterList.GetLocation());

        var defaultReturnType = Compilation.GetSpecialType(SpecialType.System_Unit);
        var metadataName = OperatorFacts.TryGetConversionOperatorMetadataName(conversionDecl.ConversionKindKeyword.Kind, out var resolvedName)
            ? resolvedName
            : operatorText;

        var conversionSymbol = new SourceMethodSymbol(
            metadataName,
            defaultReturnType,
            ImmutableArray<SourceParameterSymbol>.Empty,
            _containingType,
            _containingType,
            CurrentNamespace!.AsSourceNamespace(),
            [conversionDecl.GetLocation()],
            [conversionDecl.GetReference()],
            isStatic: true,
            methodKind: MethodKind.Conversion,
            declaredAccessibility: operatorAccessibility);

        var extensionTypeParameters = CreateExtensionTypeParameters(conversionSymbol);
        if (!extensionTypeParameters.IsDefaultOrEmpty)
            conversionSymbol.SetTypeParameters(extensionTypeParameters);

        var conversionBinder = new MethodBinder(conversionSymbol, this);

        var returnType = conversionDecl.ReturnType is null
            ? defaultReturnType
            : ResolveTypeSyntaxForSignature(conversionBinder, conversionDecl.ReturnType.Type, RefKind.None);

        var resolvedParamInfos = new List<(string name, ITypeSymbol type, RefKind refKind, ParameterSyntax syntax, bool isMutable)>();
        foreach (var p in conversionDecl.ParameterList.Parameters)
        {
            var typeSyntax = p.TypeAnnotation!.Type;
            var refKindTokenKind = p.RefKindKeyword?.Kind;
            var refKind = typeSyntax is ByRefTypeSyntax
                ? refKindTokenKind switch
                {
                    SyntaxKind.OutKeyword => RefKind.Out,
                    SyntaxKind.InKeyword => RefKind.In,
                    SyntaxKind.RefKeyword => RefKind.Ref,
                    _ => RefKind.Ref,
                }
                : refKindTokenKind switch
                {
                    SyntaxKind.OutKeyword => RefKind.Out,
                    SyntaxKind.InKeyword => RefKind.In,
                    SyntaxKind.RefKeyword => RefKind.Ref,
                    _ => RefKind.None,
                };

            var pType = ResolveParameterTypeSyntaxForSignature(conversionBinder, typeSyntax, refKind);
            var isMutable = p.BindingKeyword?.Kind == SyntaxKind.VarKeyword;
            resolvedParamInfos.Add((p.Identifier.ValueText, pType, refKind, p, isMutable));
        }

        ValidateTypeAccessibility(
            returnType,
            operatorAccessibility,
            "operator",
            GetMemberDisplayName(operatorText),
            "return",
            conversionDecl.ReturnType?.Type.GetLocation() ?? conversionDecl.ConversionKindKeyword.GetLocation());

        foreach (var (paramName, paramType, _, syntax, _) in resolvedParamInfos)
        {
            ValidateTypeAccessibility(
                paramType,
                operatorAccessibility,
                "operator",
                GetMemberDisplayName(operatorText),
                $"parameter '{paramName}'",
                syntax.TypeAnnotation?.Type.GetLocation() ?? syntax.GetLocation());
        }

        var signatureArray = resolvedParamInfos.Select(p => (p.type, p.refKind)).ToArray();
        CheckForDuplicateSignature(metadataName, operatorText, signatureArray, conversionDecl.ConversionKindKeyword.GetLocation(), conversionDecl);

        var parameters = new List<SourceParameterSymbol>();
        var seenOptionalParameter = false;
        foreach (var (paramName, paramType, refKind, syntax, isMutable) in resolvedParamInfos)
        {
            var defaultResult = ProcessParameterDefault(
                syntax,
                paramType,
                paramName,
                _diagnostics,
                ref seenOptionalParameter);
            var pSymbol = new SourceParameterSymbol(
                paramName,
                paramType,
                conversionSymbol,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [syntax.GetLocation()],
                [syntax.GetReference()],
                refKind,
                defaultResult.HasExplicitDefaultValue,
                defaultResult.ExplicitDefaultValue,
                isMutable
            );
            parameters.Add(pSymbol);
        }

        conversionSymbol.SetReturnType(returnType);
        conversionSymbol.SetParameters(parameters);

        return conversionBinder;
    }

    private bool ImplementsInterface(INamedTypeSymbol interfaceType)
    {
        if (_containingType.Interfaces.Contains(interfaceType, SymbolEqualityComparer.Default))
            return true;

        return _containingType.AllInterfaces.Contains(interfaceType, SymbolEqualityComparer.Default);
    }

    private IMethodSymbol? FindExplicitInterfaceImplementation(
        INamedTypeSymbol interfaceType,
        string methodName,
        ITypeSymbol returnType,
        (ITypeSymbol type, RefKind refKind)[] parameters)
    {
        foreach (var member in interfaceType.GetMembers(methodName).OfType<IMethodSymbol>())
        {
            if (member.IsStatic)
                continue;

            if (!ReturnTypesMatch(returnType, member.ReturnType))
                continue;

            if (!SignaturesMatch(member, parameters))
                continue;

            return member;
        }

        return null;
    }

    private IPropertySymbol? FindExplicitInterfacePropertyImplementation(
        INamedTypeSymbol interfaceType,
        string propertyName,
        ITypeSymbol propertyType,
        bool isIndexer,
        (ITypeSymbol type, RefKind refKind)[] parameters)
    {
        foreach (var property in interfaceType.GetMembers(propertyName).OfType<IPropertySymbol>())
        {
            if (property.IsIndexer != isIndexer)
                continue;

            var existingType = StripNullableReference(property.Type);
            var newType = StripNullableReference(propertyType);

            if (!SymbolEqualityComparer.Default.Equals(existingType, newType))
                continue;

            if (isIndexer && !IndexerParametersMatch(property, parameters))
                continue;

            return property;
        }

        return null;
    }

    private IEventSymbol? FindExplicitInterfaceEventImplementation(
        INamedTypeSymbol interfaceType,
        string eventName,
        ITypeSymbol eventType)
    {
        foreach (var @event in interfaceType.GetMembers(eventName).OfType<IEventSymbol>())
        {
            var existingType = StripNullableReference(@event.Type);
            var newType = StripNullableReference(eventType);

            if (!SymbolEqualityComparer.Default.Equals(existingType, newType))
                continue;

            return @event;
        }

        return null;
    }

    private static string GetInterfaceMetadataName(INamedTypeSymbol interfaceType)
    {
        var metadataName = interfaceType.ToFullyQualifiedMetadataName();
        if (!string.IsNullOrEmpty(metadataName) && metadataName[0] == '.')
            return metadataName[1..];

        return metadataName;
    }

    private static bool ReturnTypesMatch(ITypeSymbol candidateReturnType, ITypeSymbol interfaceReturnType)
    {
        if (SymbolEqualityComparer.Default.Equals(candidateReturnType, interfaceReturnType))
            return true;

        if (candidateReturnType.SpecialType == SpecialType.System_Unit && interfaceReturnType.SpecialType == SpecialType.System_Void)
            return true;

        if (candidateReturnType.SpecialType == SpecialType.System_Void && interfaceReturnType.SpecialType == SpecialType.System_Unit)
            return true;

        return false;
    }

    public MethodBinder BindConstructorDeclaration(ConstructorDeclarationSyntax ctorDecl)
    {
        ReportPartialModifierNotSupported(ctorDecl.Modifiers, "constructor", _containingType.Name);
        var isStatic = ctorDecl.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);
        var defaultAccessibility = AccessibilityUtilities.GetDefaultMemberAccessibility(_containingType);
        var ctorAccessibility = AccessibilityUtilities.DetermineAccessibility(ctorDecl.Modifiers, defaultAccessibility);
        if (isStatic)
            ctorAccessibility = Accessibility.Private;

        ReportInstanceMemberInStaticTypeIfNeeded(
            isStatic,
            "init",
            ctorDecl.GetLocation());

        var paramInfos = new List<(string name, ITypeSymbol type, RefKind refKind, ParameterSyntax syntax, bool isMutable)>();
        foreach (var p in ctorDecl.ParameterList.Parameters)
        {
            var typeSyntax = p.TypeAnnotation!.Type;
            var refKindTokenKind = p.RefKindKeyword?.Kind;
            var refKind = typeSyntax is ByRefTypeSyntax
                ? refKindTokenKind switch
                {
                    SyntaxKind.OutKeyword => RefKind.Out,
                    SyntaxKind.InKeyword => RefKind.In,
                    SyntaxKind.RefKeyword => RefKind.Ref,
                    _ => RefKind.Ref,
                }
                : refKindTokenKind switch
                {
                    SyntaxKind.OutKeyword => RefKind.Out,
                    SyntaxKind.InKeyword => RefKind.In,
                    SyntaxKind.RefKeyword => RefKind.Ref,
                    _ => RefKind.None,
                };

            var pType = ResolveParameterTypeSyntaxForSignature(this, typeSyntax, refKind);
            var isMutable = p.BindingKeyword?.Kind == SyntaxKind.VarKeyword;
            paramInfos.Add((p.Identifier.ValueText, pType, refKind, p, isMutable));
        }

        foreach (var (paramName, paramType, _, syntax, _) in paramInfos)
        {
            ValidateTypeAccessibility(
                paramType,
                ctorAccessibility,
                "constructor",
                GetMemberDisplayName(".ctor"),
                $"parameter '{paramName}'",
                syntax.TypeAnnotation!.Type.GetLocation());
        }

        CheckForDuplicateSignature(".ctor", _containingType.Name, paramInfos.Select(p => (p.type, p.refKind)).ToArray(), ctorDecl.GetLocation(), ctorDecl);

        var ctorSymbol = new SourceMethodSymbol(
            ".ctor",
            Compilation.GetSpecialType(SpecialType.System_Unit),
            ImmutableArray<SourceParameterSymbol>.Empty,
            _containingType,
            _containingType,
            CurrentNamespace!.AsSourceNamespace(),
            [ctorDecl.GetLocation()],
            [ctorDecl.GetReference()],
            isStatic: isStatic,
            methodKind: MethodKind.Constructor,
            declaredAccessibility: ctorAccessibility);

        var parameters = new List<SourceParameterSymbol>();
        var seenOptionalParameter = false;
        foreach (var (paramName, paramType, refKind, syntax, isMutable) in paramInfos)
        {
            var defaultResult = ProcessParameterDefault(
                syntax,
                paramType,
                paramName,
                _diagnostics,
                ref seenOptionalParameter);
            var pSymbol = new SourceParameterSymbol(
                paramName,
                paramType,
                ctorSymbol,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [syntax.GetLocation()],
                [syntax.GetReference()],
                refKind,
                defaultResult.HasExplicitDefaultValue,
                defaultResult.ExplicitDefaultValue,
                isMutable
            );
            parameters.Add(pSymbol);
        }

        ctorSymbol.SetParameters(parameters);

        var methodBinder = new MethodBinder(ctorSymbol, this);

        if (ctorDecl.Initializer is { } initializerSyntax)
        {
            ctorSymbol.MarkConstructorInitializerSyntax();

            if (isStatic)
            {
                _diagnostics.ReportConstructorInitializerNotAllowedOnStaticConstructor(initializerSyntax.Keyword.GetLocation());
            }
            else
            {
                var initializerBinder = new ConstructorInitializerBinder(ctorSymbol, methodBinder);
                var boundInitializer = initializerBinder.Bind(initializerSyntax);

                foreach (var diagnostic in initializerBinder.Diagnostics.AsEnumerable())
                    _diagnostics.Report(diagnostic);

                ctorSymbol.SetConstructorInitializer(boundInitializer);
            }
        }

        return methodBinder;
    }

    public MethodBinder BindNamedConstructorDeclaration(NamedConstructorDeclarationSyntax ctorDecl)
    {
        ReportPartialModifierNotSupported(ctorDecl.Modifiers, "constructor", ctorDecl.Identifier.ValueText);
        var defaultAccessibility = AccessibilityUtilities.GetDefaultMemberAccessibility(_containingType);
        var ctorAccessibility = AccessibilityUtilities.DetermineAccessibility(ctorDecl.Modifiers, defaultAccessibility);

        ReportInstanceMemberInStaticTypeIfNeeded(
            false,
            ctorDecl.Identifier.ValueText,
            ctorDecl.Identifier.GetLocation());

        var paramInfos = new List<(string name, ITypeSymbol type, RefKind refKind, ParameterSyntax syntax, bool isMutable)>();
        foreach (var p in ctorDecl.ParameterList.Parameters)
        {
            var typeSyntax = p.TypeAnnotation!.Type;
            var refKindTokenKind = p.RefKindKeyword?.Kind;
            var refKind = typeSyntax is ByRefTypeSyntax
                ? refKindTokenKind switch
                {
                    SyntaxKind.OutKeyword => RefKind.Out,
                    SyntaxKind.InKeyword => RefKind.In,
                    SyntaxKind.RefKeyword => RefKind.Ref,
                    _ => RefKind.Ref,
                }
                : refKindTokenKind switch
                {
                    SyntaxKind.OutKeyword => RefKind.Out,
                    SyntaxKind.InKeyword => RefKind.In,
                    SyntaxKind.RefKeyword => RefKind.Ref,
                    _ => RefKind.None,
                };

            var pType = ResolveParameterTypeSyntaxForSignature(this, typeSyntax, refKind);
            var isMutable = p.BindingKeyword?.Kind == SyntaxKind.VarKeyword;
            paramInfos.Add((p.Identifier.ValueText, pType, refKind, p, isMutable));
        }

        foreach (var (paramName, paramType, _, syntax, _) in paramInfos)
        {
            ValidateTypeAccessibility(
                paramType,
                ctorAccessibility,
                "constructor",
                GetMemberDisplayName(ctorDecl.Identifier.ValueText),
                $"parameter '{paramName}'",
                syntax.TypeAnnotation!.Type.GetLocation());
        }

        CheckForDuplicateSignature(ctorDecl.Identifier.ValueText, ctorDecl.Identifier.ValueText, paramInfos.Select(p => (p.type, p.refKind)).ToArray(), ctorDecl.Identifier.GetLocation(), ctorDecl);

        var ctorSymbol = new SourceMethodSymbol(
            ctorDecl.Identifier.ValueText,
            _containingType,
            ImmutableArray<SourceParameterSymbol>.Empty,
            _containingType,
            _containingType,
            CurrentNamespace!.AsSourceNamespace(),
            [ctorDecl.GetLocation()],
            [ctorDecl.GetReference()],
            isStatic: true,
            methodKind: MethodKind.NamedConstructor,
            declaredAccessibility: ctorAccessibility);

        var parameters = new List<SourceParameterSymbol>();
        var seenOptionalParameter = false;
        foreach (var (paramName, paramType, refKind, syntax, isMutable) in paramInfos)
        {
            var defaultResult = ProcessParameterDefault(
                syntax,
                paramType,
                paramName,
                _diagnostics,
                ref seenOptionalParameter);
            var pSymbol = new SourceParameterSymbol(
                paramName,
                paramType,
                ctorSymbol,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [syntax.GetLocation()],
                [syntax.GetReference()],
                refKind,
                defaultResult.HasExplicitDefaultValue,
                defaultResult.ExplicitDefaultValue,
                isMutable
            );
            parameters.Add(pSymbol);
        }

        ctorSymbol.SetParameters(parameters);
        return new MethodBinder(ctorSymbol, this);
    }

    public DelegateDeclarationBinder BindDelegateDeclaration(DelegateDeclarationSyntax delegateDecl)
    {
        ReportPartialModifierNotSupported(delegateDecl.Modifiers, "delegate", delegateDecl.Identifier.ValueText);
        var declared = BindDelegateSymbol(delegateDecl);
        var delegateSymbol = declared as SourceNamedTypeSymbol;

        if (delegateSymbol is null)
        {
            var delegateAccessibility = AccessibilityUtilities.DetermineAccessibility(
                delegateDecl.Modifiers,
                AccessibilityUtilities.GetDefaultTypeAccessibility(_containingType));

            // If the delegate symbol wasn't registered (or we're binding against metadata),
            // fall back to a synthesized container so we can keep producing diagnostics.
            delegateSymbol = new SourceNamedTypeSymbol(
                delegateDecl.Identifier.ValueText,
                Compilation.GetSpecialType(SpecialType.System_MulticastDelegate)!,
                TypeKind.Delegate,
                _containingType,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [delegateDecl.GetLocation()],
                [delegateDecl.GetReference()],
                isSealed: true,
                isAbstract: true,
                isStatic: false,
                declaredAccessibility: delegateAccessibility);
        }

        var binder = new DelegateDeclarationBinder(this, delegateSymbol, delegateDecl);
        binder.EnsureTypeParameterConstraintTypesResolved(delegateSymbol.TypeParameters);
        EnsureDelegateMembers(delegateSymbol, delegateDecl, binder);
        return binder;
    }

    private void EnsureDelegateMembers(SourceNamedTypeSymbol delegateSymbol, DelegateDeclarationSyntax delegateDecl, Binder binder)
    {
        var unitType = Compilation.GetSpecialType(SpecialType.System_Unit);
        var intPtrType = Compilation.GetSpecialType(SpecialType.System_IntPtr);
        var objectType = Compilation.GetSpecialType(SpecialType.System_Object);

        void RegisterMember(SourceNamedTypeSymbol owner, ISymbol member)
        {
            if (!owner.GetMembers().Any(m => SymbolEqualityComparer.Default.Equals(m, member)))
                owner.AddMember(member);
        }

        static RefKind GetRefKind(ParameterSyntax parameter)
        {
            var typeSyntax = parameter.TypeAnnotation!.Type;
            var refKindTokenKind = parameter.RefKindKeyword?.Kind;
            return typeSyntax is ByRefTypeSyntax
                ? refKindTokenKind switch
                {
                    SyntaxKind.OutKeyword => RefKind.Out,
                    SyntaxKind.InKeyword => RefKind.In,
                    SyntaxKind.RefKeyword => RefKind.Ref,
                    _ => RefKind.Ref,
                }
                : refKindTokenKind switch
                {
                    SyntaxKind.OutKeyword => RefKind.Out,
                    SyntaxKind.InKeyword => RefKind.In,
                    SyntaxKind.RefKeyword => RefKind.Ref,
                    _ => RefKind.None,
                };
        }

        // .ctor(object, IntPtr)
        var ctor = new SourceMethodSymbol(
            ".ctor",
            unitType!,
            ImmutableArray<SourceParameterSymbol>.Empty,
            delegateSymbol,
            delegateSymbol,
            CurrentNamespace.AsSourceNamespace(),
            new[] { delegateDecl.GetLocation() },
            Array.Empty<SyntaxReference>(),
            isStatic: false,
            methodKind: MethodKind.Constructor,
            declaredAccessibility: Accessibility.Public);

        var ctorParams = ImmutableArray.Create(
                   new SourceParameterSymbol(
                       "object",
                       objectType!,
                       ctor,
                       delegateSymbol,
                       CurrentNamespace.AsSourceNamespace(),
                       new[] { delegateDecl.GetLocation() },
                       Array.Empty<SyntaxReference>(),
                       RefKind.None),
                   new SourceParameterSymbol(
                       "method",
                       intPtrType!,
                       ctor,
                       delegateSymbol,
                       CurrentNamespace.AsSourceNamespace(),
                       new[] { delegateDecl.GetLocation() },
                       Array.Empty<SyntaxReference>(),
                       RefKind.None));

        ctor.SetParameters(ctorParams);
        RegisterMember(delegateSymbol, ctor);

        // Invoke
        var returnType = delegateDecl.ReturnType is null
            ? unitType!
            : ResolveTypeSyntaxForSignature(binder, delegateDecl.ReturnType.Type, RefKind.None);

        var invoke = new SourceMethodSymbol(
            "Invoke",
            returnType,
            ImmutableArray<SourceParameterSymbol>.Empty,
            delegateSymbol,
            delegateSymbol,
            CurrentNamespace.AsSourceNamespace(),
            new[] { delegateDecl.GetLocation() },
            Array.Empty<SyntaxReference>(),
            isStatic: false,
            methodKind: MethodKind.Ordinary,
            declaredAccessibility: Accessibility.Public);

        var invokeParams = ImmutableArray.CreateBuilder<SourceParameterSymbol>(delegateDecl.ParameterList.Parameters.Count);
        foreach (var p in delegateDecl.ParameterList.Parameters)
        {
            var refKind = GetRefKind(p);
            var typeSyntax = p.TypeAnnotation!.Type;
            var pType = ResolveParameterTypeSyntaxForSignature(binder, typeSyntax, refKind);

            invokeParams.Add(new SourceParameterSymbol(
                p.Identifier.ValueText,
                pType,
                invoke,
                delegateSymbol,
                CurrentNamespace.AsSourceNamespace(),
                new[] { p.GetLocation() },
                new[] { p.GetReference() },
                refKind));
        }

        invoke.SetParameters(invokeParams.ToImmutable());
        RegisterMember(delegateSymbol, invoke);
    }

    private void CheckForDuplicateSignature(string searchName, string displayName, (ITypeSymbol type, RefKind refKind)[] parameters, Location location, SyntaxNode? currentDeclaration)
    {
        foreach (var method in _containingType.GetMembers(searchName).OfType<IMethodSymbol>())
        {
            if (currentDeclaration is not null && method.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == currentDeclaration))
                continue;

            if (SignaturesMatch(method, parameters))
            {
                _diagnostics.ReportTypeAlreadyDefinesMember(_containingType.Name, displayName, location);
                break;
            }
        }
    }

    private static bool SignaturesMatch(IMethodSymbol existing, (ITypeSymbol type, RefKind refKind)[] parameters)
    {
        if (existing.Parameters.Length != parameters.Length)
            return false;

        for (int i = 0; i < parameters.Length; i++)
        {
            var existingParam = existing.Parameters[i];
            var newParam = parameters[i];

            if (existingParam.RefKind != newParam.refKind)
                return false;

            var existingType = StripNullableReference(existingParam.Type);
            var newType = StripNullableReference(newParam.type);

            if (!SymbolEqualityComparer.Default.Equals(existingType, newType))
                return false;
        }

        return true;
    }

    private static ITypeSymbol StripNullableReference(ITypeSymbol type)
    {
        if (type is NullableTypeSymbol nt && !nt.UnderlyingType.IsValueType)
            return StripNullableReference(nt.UnderlyingType);

        return type;
    }

    public Dictionary<AccessorDeclarationSyntax, MethodBinder> BindEventDeclaration(EventDeclarationSyntax eventDecl)
    {
        ReportPartialModifierNotSupported(eventDecl.Modifiers, "event", eventDecl.Identifier.ValueText);
        var eventType = ResolveTypeSyntaxForSignature(this, eventDecl.Type.Type, RefKind.None);
        var modifiers = eventDecl.Modifiers;
        var hasStaticModifier = modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);
        var isStatic = hasStaticModifier;
        var isAbstract = modifiers.Any(m => m.Kind == SyntaxKind.AbstractKeyword);
        var isVirtual = modifiers.Any(m => m.Kind == SyntaxKind.VirtualKeyword);
        var isOverride = modifiers.Any(m => m.Kind == SyntaxKind.OverrideKeyword);
        var isSealed = modifiers.Any(m => m.Kind is SyntaxKind.SealedKeyword or SyntaxKind.FinalKeyword);
        var hasNewModifier = modifiers.Any(m => m.Kind == SyntaxKind.NewKeyword);
        var defaultAccessibility = AccessibilityUtilities.GetDefaultMemberAccessibility(_containingType);
        var eventAccessibility = AccessibilityUtilities.DetermineAccessibility(modifiers, defaultAccessibility);
        var explicitInterfaceSpecifier = eventDecl.ExplicitInterfaceSpecifier;
        var identifierToken = ResolveExplicitInterfaceIdentifier(eventDecl.Identifier, explicitInterfaceSpecifier);
        var eventName = identifierToken.Text;
        var metadataName = eventName;
        INamedTypeSymbol? explicitInterfaceType = null;
        string? explicitInterfaceMetadataName = null;
        IEventSymbol? explicitInterfaceEvent = null;

        var isExtensionContainer = IsExtensionContainer;
        var isExtensionMember = isExtensionContainer && !hasStaticModifier;

        if (isExtensionContainer)
        {
            isAbstract = false;
            isVirtual = false;
            isOverride = false;
            isSealed = false;
            eventAccessibility = modifiers.Any(m => m.Kind == SyntaxKind.InternalKeyword)
                ? Accessibility.Internal
                : Accessibility.Public;

            if (isExtensionMember)
                isStatic = false;
        }

        if (explicitInterfaceSpecifier is not null)
        {
            var resolved = ResolveNamedTypeSyntax(this, explicitInterfaceSpecifier.Name);
            if (resolved is INamedTypeSymbol interfaceType && interfaceType.TypeKind == TypeKind.Interface)
            {
                explicitInterfaceType = interfaceType;
                var interfaceMetadataName = GetInterfaceMetadataName(interfaceType);
                metadataName = $"{interfaceMetadataName}.{eventName}";
                explicitInterfaceMetadataName = interfaceMetadataName;
                eventAccessibility = Accessibility.Private;

                if (!ImplementsInterface(interfaceType))
                {
                    _diagnostics.ReportContainingTypeDoesNotImplementInterface(
                        _containingType.Name,
                        interfaceType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        explicitInterfaceSpecifier.Name.GetLocation());
                }

                explicitInterfaceEvent = FindExplicitInterfaceEventImplementation(interfaceType, eventName, eventType);

                if (explicitInterfaceEvent is null)
                {
                    _diagnostics.ReportExplicitInterfaceMemberNotFound(
                        interfaceType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        eventName,
                        identifierToken.GetLocation());
                }
            }
            else
            {
                _diagnostics.ReportExplicitInterfaceSpecifierMustBeInterface(explicitInterfaceSpecifier.Name.GetLocation());
            }
        }

        if (explicitInterfaceType is not null)
        {
            isStatic = false;
            isVirtual = false;
            isOverride = false;
            isSealed = false;
            isAbstract = false;
        }

        ReportInstanceMemberInStaticTypeIfNeeded(
            isStatic,
            eventName,
            identifierToken.GetLocation());

        ValidateInheritanceModifiers(
            ref isAbstract,
            ref isVirtual,
            ref isOverride,
            ref isSealed,
            isStatic,
            eventName,
            identifierToken.GetLocation());

        ValidateAbstractMemberInNonAbstractType(
            isAbstract,
            GetMemberDisplayName(eventName),
            identifierToken.GetLocation());

        if (isSealed && !isOverride)
        {
            _diagnostics.ReportSealedMemberMustOverride(eventName, identifierToken.GetLocation());
            isSealed = false;
        }

        if (isStatic && (isVirtual || isOverride))
        {
            if (isVirtual)
                _diagnostics.ReportStaticMemberCannotBeVirtualOrOverride(eventName, "virtual", identifierToken.GetLocation());
            if (isOverride)
                _diagnostics.ReportStaticMemberCannotBeVirtualOrOverride(eventName, "override", identifierToken.GetLocation());

            isVirtual = false;
            isOverride = false;
            isSealed = false;
        }

        if (isVirtual && !isOverride && _containingType.IsSealed)
        {
            _diagnostics.ReportVirtualMemberInSealedType(eventName, _containingType.Name, identifierToken.GetLocation());
            isVirtual = false;
        }

        ValidateTypeAccessibility(
            eventType,
            eventAccessibility,
            "event",
            GetMemberDisplayName(eventName),
            "event",
            eventDecl.Type.Type.GetLocation());

        ITypeSymbol? receiverType = null;
        if (isExtensionMember)
            receiverType = GetExtensionReceiverType();

        if (receiverType is not null && _extensionReceiverTypeSyntax is not null)
        {
            ValidateTypeAccessibility(
                receiverType,
                eventAccessibility,
                "event",
                GetMemberDisplayName(eventName),
                "receiver",
                _extensionReceiverTypeSyntax.GetLocation());
        }

        var eventSymbol = new SourceEventSymbol(
            eventName,
            eventType,
            _containingType,
            _containingType,
            CurrentNamespace!.AsSourceNamespace(),
            [eventDecl.GetLocation()],
            [eventDecl.GetReference()],
            isStatic: isStatic,
            metadataName: metadataName,
            declaredAccessibility: eventAccessibility);

        if (isExtensionMember)
            eventSymbol.MarkDeclaredInExtension(receiverType);

        if (!isExtensionContainer &&
            _containingType.TypeKind != TypeKind.Interface)
        {
            var isAutoEvent = eventDecl.AccessorList is null ||
                eventDecl.AccessorList.Accessors.All(a => a.Body is null && a.ExpressionBody is null);

            if (isAutoEvent)
            {
                var backingField = new SourceFieldSymbol(
                    $"<{eventSymbol.Name}>k__BackingField",
                    eventType,
                    isStatic: isStatic,
                    isMutable: true,
                    isConst: false,
                    constantValue: null,
                    _containingType,
                    _containingType,
                    CurrentNamespace!.AsSourceNamespace(),
                    [eventDecl.GetLocation()],
                    [eventDecl.GetReference()],
                    declaredAccessibility: Accessibility.Private);

                eventSymbol.SetBackingField(backingField);
            }
        }

        IMethodSymbol? overriddenAdder = null;
        IMethodSymbol? overriddenRemover = null;

        if (isOverride)
        {
            var candidate = FindEventOverrideCandidate(eventName, eventType, isStatic);
            bool overrideValid = true;

            if (candidate is null)
            {
                _diagnostics.ReportOverrideMemberNotFound(eventName, "event", identifierToken.GetLocation());
                overrideValid = false;
            }
            else
            {
                if (candidate.AddMethod is null || !candidate.AddMethod.IsVirtual)
                {
                    _diagnostics.ReportOverrideMemberNotFound(eventName, "event", identifierToken.GetLocation());
                    overrideValid = false;
                }
                else if (candidate.AddMethod.IsFinal)
                {
                    _diagnostics.ReportCannotOverrideSealedMember(eventName, candidate.Name, identifierToken.GetLocation());
                    overrideValid = false;
                }
                else
                {
                    overriddenAdder = candidate.AddMethod;
                }

                if (overrideValid)
                {
                    if (candidate.RemoveMethod is null || !candidate.RemoveMethod.IsVirtual)
                    {
                        _diagnostics.ReportOverrideMemberNotFound(eventName, "event", identifierToken.GetLocation());
                        overrideValid = false;
                    }
                    else if (candidate.RemoveMethod.IsFinal)
                    {
                        _diagnostics.ReportCannotOverrideSealedMember(eventName, candidate.Name, identifierToken.GetLocation());
                        overrideValid = false;
                    }
                    else
                    {
                        overriddenRemover = candidate.RemoveMethod;
                    }
                }
            }

            if (!overrideValid)
            {
                isOverride = false;
                isVirtual = false;
                isSealed = false;
            }
            else
            {
                isVirtual = true;
            }
        }

        if (explicitInterfaceType is null && !isOverride && !isExtensionContainer)
        {
            var hiddenMember = FindEventOverrideCandidate(eventName, eventType, isStatic);
            ReportMemberHidingIfNeeded(hiddenMember, eventName, hasNewModifier, identifierToken.GetLocation());
        }

        var binders = new Dictionary<AccessorDeclarationSyntax, MethodBinder>();
        SourceMethodSymbol? addMethod = null;
        SourceMethodSymbol? removeMethod = null;
        var explicitAccessorPrefix = explicitInterfaceMetadataName is not null
            ? explicitInterfaceMetadataName + "."
            : string.Empty;

        if (eventDecl.AccessorList is not null)
        {
            foreach (var accessor in eventDecl.AccessorList.Accessors)
            {
                bool isAdd = accessor.Kind == SyntaxKind.AddAccessorDeclaration;
                bool isRemove = accessor.Kind == SyntaxKind.RemoveAccessorDeclaration;

                if (!isAdd && !isRemove)
                    continue;

                var name = explicitAccessorPrefix + (isAdd ? "add_" : "remove_") + eventSymbol.Name;
                var accessorOverride = isOverride && (isAdd ? overriddenAdder is not null : overriddenRemover is not null);
                var accessorVirtual = accessorOverride || isVirtual;
                var accessorSealed = accessorOverride && isSealed;

                if (isAbstract && (accessor.Body is not null || accessor.ExpressionBody is not null))
                {
                    _diagnostics.ReportAbstractMemberCannotHaveBody(name, identifierToken.GetLocation());
                }

                var methodSymbol = new SourceMethodSymbol(
                    name,
                    Compilation.GetSpecialType(SpecialType.System_Unit),
                    ImmutableArray<SourceParameterSymbol>.Empty,
                    eventSymbol,
                    _containingType,
                    CurrentNamespace!.AsSourceNamespace(),
                    [accessor.GetLocation()],
                    [accessor.GetReference()],
                    isStatic: isStatic || isExtensionContainer,
                    methodKind: isAdd ? MethodKind.EventAdd : MethodKind.EventRemove,
                    isAsync: false,
                    isVirtual: accessorVirtual,
                    isOverride: accessorOverride,
                    isSealed: accessorSealed,
                    isAbstract: isAbstract,
                    declaredAccessibility: eventAccessibility);

                if (isExtensionMember)
                    methodSymbol.MarkDeclaredInExtension();

                if (isExtensionMember)
                {
                    var extensionTypeParameters = CreateExtensionTypeParameters(methodSymbol);
                    if (!extensionTypeParameters.IsDefaultOrEmpty)
                        methodSymbol.SetTypeParameters(extensionTypeParameters);
                }

                MethodBinder? binder = null;
                ITypeSymbol? receiverTypeForAccessor = receiverType;
                if (isExtensionMember && _extensionReceiverTypeSyntax is not null)
                {
                    binder = new MethodBinder(methodSymbol, this);
                    receiverTypeForAccessor = ResolveTypeSyntaxForSignature(binder, _extensionReceiverTypeSyntax, RefKind.None);
                }

                var parameters = new List<SourceParameterSymbol>();
                if (isExtensionMember && receiverTypeForAccessor is not null && _extensionReceiverTypeSyntax is not null)
                {
                    var receiverNamespace = CurrentNamespace!.AsSourceNamespace();
                    var selfParameter = new SourceParameterSymbol(
                        "self",
                        receiverTypeForAccessor,
                        methodSymbol,
                        _containingType,
                        receiverNamespace,
                        [_extensionReceiverTypeSyntax.GetLocation()],
                        [_extensionReceiverTypeSyntax.GetReference()]);
                    parameters.Add(selfParameter);
                }

                parameters.Add(new SourceParameterSymbol(
                    "value",
                    eventType,
                    methodSymbol,
                    _containingType,
                    CurrentNamespace!.AsSourceNamespace(),
                    [accessor.GetLocation()],
                    [accessor.GetReference()]));

                methodSymbol.SetParameters(parameters);

                if (explicitInterfaceType is not null && explicitInterfaceEvent is not null)
                {
                    var interfaceAccessor = isAdd
                        ? explicitInterfaceEvent.AddMethod
                        : explicitInterfaceEvent.RemoveMethod;

                    if (interfaceAccessor is not null)
                    {
                        methodSymbol.SetExplicitInterfaceImplementations(ImmutableArray.Create(interfaceAccessor));
                    }
                    else
                    {
                        _diagnostics.ReportExplicitInterfaceMemberNotFound(
                            explicitInterfaceType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            eventName,
                            accessor.Keyword.GetLocation());
                    }
                }

                if (accessorOverride)
                {
                    var overriddenMethod = isAdd ? overriddenAdder : overriddenRemover;
                    if (overriddenMethod is not null)
                        methodSymbol.SetOverriddenMethod(overriddenMethod);
                }

                binder ??= new MethodBinder(methodSymbol, this);
                binders[accessor] = binder;

                if (isAdd)
                    addMethod = methodSymbol;
                else
                    removeMethod = methodSymbol;
            }
        }
        else
        {
            var addName = explicitAccessorPrefix + "add_" + eventSymbol.Name;
            var removeName = explicitAccessorPrefix + "remove_" + eventSymbol.Name;
            var accessorVirtual = isOverride || isVirtual;
            var accessorSealed = isOverride && isSealed;

            var addMethodSymbol = new SourceMethodSymbol(
                addName,
                Compilation.GetSpecialType(SpecialType.System_Unit),
                ImmutableArray<SourceParameterSymbol>.Empty,
                eventSymbol,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [eventDecl.GetLocation()],
                [eventDecl.GetReference()],
                isStatic: isStatic || isExtensionContainer,
                methodKind: MethodKind.EventAdd,
                isAsync: false,
                isVirtual: accessorVirtual,
                isOverride: isOverride,
                isSealed: accessorSealed,
                isAbstract: isAbstract,
                declaredAccessibility: eventAccessibility);

            var removeMethodSymbol = new SourceMethodSymbol(
                removeName,
                Compilation.GetSpecialType(SpecialType.System_Unit),
                ImmutableArray<SourceParameterSymbol>.Empty,
                eventSymbol,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [eventDecl.GetLocation()],
                [eventDecl.GetReference()],
                isStatic: isStatic || isExtensionContainer,
                methodKind: MethodKind.EventRemove,
                isAsync: false,
                isVirtual: accessorVirtual,
                isOverride: isOverride,
                isSealed: accessorSealed,
                isAbstract: isAbstract,
                declaredAccessibility: eventAccessibility);

            if (isExtensionMember)
            {
                addMethodSymbol.MarkDeclaredInExtension();
                removeMethodSymbol.MarkDeclaredInExtension();
            }

            var parameters = new List<SourceParameterSymbol>();
            if (isExtensionMember && receiverType is not null && _extensionReceiverTypeSyntax is not null)
            {
                var receiverNamespace = CurrentNamespace!.AsSourceNamespace();
                var selfParameter = new SourceParameterSymbol(
                    "self",
                    receiverType,
                    addMethodSymbol,
                    _containingType,
                    receiverNamespace,
                    [_extensionReceiverTypeSyntax.GetLocation()],
                    [_extensionReceiverTypeSyntax.GetReference()]);
                parameters.Add(selfParameter);
            }

            parameters.Add(new SourceParameterSymbol(
                "value",
                eventType,
                addMethodSymbol,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [eventDecl.GetLocation()],
                [eventDecl.GetReference()]));
            addMethodSymbol.SetParameters(parameters);

            var removeParameters = new List<SourceParameterSymbol>();
            if (isExtensionMember && receiverType is not null && _extensionReceiverTypeSyntax is not null)
            {
                var receiverNamespace = CurrentNamespace!.AsSourceNamespace();
                var selfParameter = new SourceParameterSymbol(
                    "self",
                    receiverType,
                    removeMethodSymbol,
                    _containingType,
                    receiverNamespace,
                    [_extensionReceiverTypeSyntax.GetLocation()],
                    [_extensionReceiverTypeSyntax.GetReference()]);
                removeParameters.Add(selfParameter);
            }

            removeParameters.Add(new SourceParameterSymbol(
                "value",
                eventType,
                removeMethodSymbol,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [eventDecl.GetLocation()],
                [eventDecl.GetReference()]));
            removeMethodSymbol.SetParameters(removeParameters);

            if (explicitInterfaceType is not null && explicitInterfaceEvent is not null)
            {
                if (explicitInterfaceEvent.AddMethod is not null)
                    addMethodSymbol.SetExplicitInterfaceImplementations(ImmutableArray.Create(explicitInterfaceEvent.AddMethod));
                if (explicitInterfaceEvent.RemoveMethod is not null)
                    removeMethodSymbol.SetExplicitInterfaceImplementations(ImmutableArray.Create(explicitInterfaceEvent.RemoveMethod));
            }

            if (isOverride)
            {
                if (overriddenAdder is not null)
                    addMethodSymbol.SetOverriddenMethod(overriddenAdder);
                if (overriddenRemover is not null)
                    removeMethodSymbol.SetOverriddenMethod(overriddenRemover);
            }

            addMethod = addMethodSymbol;
            removeMethod = removeMethodSymbol;
        }

        eventSymbol.SetAccessors(addMethod, removeMethod);

        return binders;
    }

    public Dictionary<AccessorDeclarationSyntax, MethodBinder> BindIndexerDeclaration(IndexerDeclarationSyntax indexerDecl)
    {
        ReportPartialModifierNotSupported(indexerDecl.Modifiers, "indexer", "Item");
        var propertyType = ResolveTypeSyntaxForSignature(this, indexerDecl.Type.Type, RefKind.None);
        var modifiers = indexerDecl.Modifiers;
        var hasStaticModifier = modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);
        var isStatic = hasStaticModifier; var isAbstract = modifiers.Any(m => m.Kind == SyntaxKind.AbstractKeyword);
        var isVirtual = modifiers.Any(m => m.Kind == SyntaxKind.VirtualKeyword);
        var isOverride = modifiers.Any(m => m.Kind == SyntaxKind.OverrideKeyword);
        var isSealed = modifiers.Any(m => m.Kind is SyntaxKind.SealedKeyword or SyntaxKind.FinalKeyword);
        var hasNewModifier = modifiers.Any(m => m.Kind == SyntaxKind.NewKeyword);
        var defaultAccessibility = AccessibilityUtilities.GetDefaultMemberAccessibility(_containingType);
        var indexerAccessibility = AccessibilityUtilities.DetermineAccessibility(modifiers, defaultAccessibility);
        var explicitInterfaceSpecifier = indexerDecl.ExplicitInterfaceSpecifier;
        var identifierToken = ResolveExplicitInterfaceIdentifier(indexerDecl.Identifier, explicitInterfaceSpecifier);

        var isExtensionContainer = IsExtensionContainer;
        var isExtensionMember = isExtensionContainer && !hasStaticModifier;

        ITypeSymbol? receiverType = null;
        if (isExtensionMember)
            receiverType = GetExtensionReceiverType();

        if (isExtensionContainer)
        {
            isAbstract = false;
            isVirtual = false;
            isOverride = false;
            isSealed = false;
            indexerAccessibility = modifiers.Any(m => m.Kind == SyntaxKind.InternalKeyword)
                ? Accessibility.Internal
                : Accessibility.Public;

            if (isExtensionMember)
                isStatic = false;
        }

        var indexerParametersBuilder = new List<(ParameterSyntax Syntax, ITypeSymbol Type, RefKind RefKind, bool IsMutable, bool HasDefaultValue, object? DefaultValue)>();
        var seenOptionalParameter = false;
        foreach (var p in indexerDecl.ParameterList.Parameters)
        {
            var typeSyntax = p.TypeAnnotation!.Type;
            var refKindTokenKind = p.RefKindKeyword?.Kind;
            var isByRefSyntax = typeSyntax is ByRefTypeSyntax;
            var refKind = isByRefSyntax
                ? refKindTokenKind switch
                {
                    SyntaxKind.OutKeyword => RefKind.Out,
                    SyntaxKind.InKeyword => RefKind.In,
                    SyntaxKind.RefKeyword => RefKind.Ref,
                    _ => RefKind.Ref,
                }
                : refKindTokenKind switch
                {
                    SyntaxKind.OutKeyword => RefKind.Out,
                    SyntaxKind.InKeyword => RefKind.In,
                    SyntaxKind.RefKeyword => RefKind.Ref,
                    _ => RefKind.None,
                };

            var type = ResolveParameterTypeSyntaxForSignature(this, typeSyntax, refKind);

            var defaultResult = ProcessParameterDefault(
                p,
                type,
                p.Identifier.ValueText,
                _diagnostics,
                ref seenOptionalParameter);

            var isMutable = p.BindingKeyword?.Kind == SyntaxKind.VarKeyword;

            indexerParametersBuilder.Add((p, type, refKind, isMutable, defaultResult.HasExplicitDefaultValue, defaultResult.ExplicitDefaultValue));
        }

        var indexerParameters = indexerParametersBuilder.ToArray();

        var overrideParameters = indexerParameters.Select(p => (p.Type, p.RefKind)).ToArray();
        var metadataName = "Item";
        INamedTypeSymbol? explicitInterfaceType = null;
        string? explicitInterfaceMetadataName = null;
        IPropertySymbol? explicitInterfaceProperty = null;

        if (explicitInterfaceSpecifier is not null)
        {
            var resolved = ResolveNamedTypeSyntax(this, explicitInterfaceSpecifier.Name);
            if (resolved is INamedTypeSymbol interfaceType && interfaceType.TypeKind == TypeKind.Interface)
            {
                explicitInterfaceType = interfaceType;
                var interfaceMetadataName = GetInterfaceMetadataName(interfaceType);
                metadataName = $"{interfaceMetadataName}.Item";
                explicitInterfaceMetadataName = interfaceMetadataName;
                indexerAccessibility = Accessibility.Private;

                if (!ImplementsInterface(interfaceType))
                {
                    _diagnostics.ReportContainingTypeDoesNotImplementInterface(
                        _containingType.Name,
                        interfaceType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        explicitInterfaceSpecifier.Name.GetLocation());
                }

                explicitInterfaceProperty = FindExplicitInterfacePropertyImplementation(
                    interfaceType,
                    "Item",
                    propertyType,
                    isIndexer: true,
                    parameters: overrideParameters);

                if (explicitInterfaceProperty is null)
                {
                    _diagnostics.ReportExplicitInterfaceMemberNotFound(
                        interfaceType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        "Item",
                        identifierToken.GetLocation());
                }
            }
            else
            {
                _diagnostics.ReportExplicitInterfaceSpecifierMustBeInterface(explicitInterfaceSpecifier.Name.GetLocation());
            }
        }

        if (explicitInterfaceType is not null)
        {
            isStatic = false;
            isVirtual = false;
            isOverride = false;
            isSealed = false;
            isAbstract = false;
        }

        ReportInstanceMemberInStaticTypeIfNeeded(
            isStatic,
            "Item",
            identifierToken.GetLocation());

        ValidateInheritanceModifiers(
            ref isAbstract,
            ref isVirtual,
            ref isOverride,
            ref isSealed,
            isStatic,
            "Item",
            identifierToken.GetLocation());

        ValidateAbstractMemberInNonAbstractType(
            isAbstract,
            GetMemberDisplayName("Item"),
            identifierToken.GetLocation());

        if (isSealed && !isOverride)
        {
            _diagnostics.ReportSealedMemberMustOverride("Item", identifierToken.GetLocation());
            isSealed = false;
        }

        if (isStatic && (isVirtual || isOverride))
        {
            if (isVirtual)
                _diagnostics.ReportStaticMemberCannotBeVirtualOrOverride("Item", "virtual", identifierToken.GetLocation());
            if (isOverride)
                _diagnostics.ReportStaticMemberCannotBeVirtualOrOverride("Item", "override", identifierToken.GetLocation());

            isVirtual = false;
            isOverride = false;
            isSealed = false;
        }

        if (isVirtual && !isOverride && _containingType.IsSealed)
        {
            _diagnostics.ReportVirtualMemberInSealedType("Item", _containingType.Name, identifierToken.GetLocation());
            isVirtual = false;
        }

        ValidateTypeAccessibility(
            propertyType,
            indexerAccessibility,
            "indexer",
            GetMemberDisplayName("Item"),
            "indexer",
            indexerDecl.Type.Type.GetLocation());

        if (receiverType is not null && _extensionReceiverTypeSyntax is not null)
        {
            ValidateTypeAccessibility(
                receiverType,
                indexerAccessibility,
                "indexer",
                GetMemberDisplayName("Item"),
                "receiver",
                _extensionReceiverTypeSyntax.GetLocation());
        }

        foreach (var parameter in indexerParameters)
        {
            ValidateTypeAccessibility(
                parameter.Type,
                indexerAccessibility,
                "indexer",
                GetMemberDisplayName("Item"),
                $"parameter '{parameter.Syntax.Identifier.ValueText}'",
                parameter.Syntax.TypeAnnotation!.Type.GetLocation());
        }

        var propertySymbol = new SourcePropertySymbol(
            "Item",
            propertyType,
            _containingType,
            _containingType,
            CurrentNamespace!.AsSourceNamespace(),
            [indexerDecl.GetLocation()],
            [indexerDecl.GetReference()],
            isIndexer: true,
            isStatic: isStatic,
            metadataName: metadataName,
            declaredAccessibility: indexerAccessibility);

        var binders = new Dictionary<AccessorDeclarationSyntax, MethodBinder>();

        var hasGetter = indexerDecl.AccessorList?.Accessors.Any(a => a.Kind == SyntaxKind.GetAccessorDeclaration) ?? false;
        var hasSetter = indexerDecl.AccessorList?.Accessors.Any(a => a.Kind == SyntaxKind.SetAccessorDeclaration) ?? false;

        IMethodSymbol? overriddenGetter = null;
        IMethodSymbol? overriddenSetter = null;

        if (isOverride)
        {
            var candidate = FindPropertyOverrideCandidate("Item", propertyType, isStatic, isIndexer: true, overrideParameters);
            bool overrideValid = true;

            if (candidate is null)
            {
                _diagnostics.ReportOverrideMemberNotFound("Item", "indexer", identifierToken.GetLocation());
                overrideValid = false;
            }
            else
            {
                if (hasGetter)
                {
                    if (candidate.GetMethod is null || !candidate.GetMethod.IsVirtual)
                    {
                        _diagnostics.ReportOverrideMemberNotFound("Item", "indexer", identifierToken.GetLocation());
                        overrideValid = false;
                    }
                    else if (candidate.GetMethod.IsFinal)
                    {
                        _diagnostics.ReportCannotOverrideSealedMember("Item", candidate.Name, identifierToken.GetLocation());
                        overrideValid = false;
                    }
                    else
                    {
                        overriddenGetter = candidate.GetMethod;
                    }
                }

                if (overrideValid && hasSetter)
                {
                    if (candidate.SetMethod is null || !candidate.SetMethod.IsVirtual)
                    {
                        _diagnostics.ReportOverrideMemberNotFound("Item", "indexer", identifierToken.GetLocation());
                        overrideValid = false;
                    }
                    else if (candidate.SetMethod.IsFinal)
                    {
                        _diagnostics.ReportCannotOverrideSealedMember("Item", candidate.Name, identifierToken.GetLocation());
                        overrideValid = false;
                    }
                    else
                    {
                        overriddenSetter = candidate.SetMethod;
                    }
                }
            }

            if (!overrideValid)
            {
                isOverride = false;
                isVirtual = false;
                isSealed = false;
            }
            else
            {
                isVirtual = true;
            }
        }

        if (explicitInterfaceType is null && !isOverride && !isExtensionContainer)
        {
            var hiddenMember = FindPropertyOverrideCandidate("Item", propertyType, isStatic, isIndexer: true, overrideParameters);
            ReportMemberHidingIfNeeded(hiddenMember, "Item", hasNewModifier, identifierToken.GetLocation());
        }

        SourceMethodSymbol? getMethod = null;
        SourceMethodSymbol? setMethod = null;
        var explicitIndexerAccessorPrefix = explicitInterfaceMetadataName is not null
            ? explicitInterfaceMetadataName + "."
            : string.Empty;

        if (isAbstract && indexerDecl.ExpressionBody is not null)
        {
            _diagnostics.ReportAbstractMemberCannotHaveBody("Item", identifierToken.GetLocation());
        }

        if (indexerDecl.AccessorList is not null)
        {
            foreach (var accessor in indexerDecl.AccessorList.Accessors)
            {
                bool isGet = accessor.Kind == SyntaxKind.GetAccessorDeclaration;
                var returnType = isGet ? propertyType : Compilation.GetSpecialType(SpecialType.System_Unit);
                var name = explicitIndexerAccessorPrefix + (isGet ? "get_" : "set_") + propertySymbol.Name;

                var accessorOverride = isOverride && (isGet ? overriddenGetter is not null : overriddenSetter is not null);
                var accessorVirtual = accessorOverride || isVirtual;
                var accessorSealed = accessorOverride && isSealed;

                var isAsync = accessor.Modifiers.Any(m => m.Kind == SyntaxKind.AsyncKeyword);

                var propertyTypeSyntax = indexerDecl.Type?.Type;
                var requiresAsyncReturnTypeDiagnostic = isGet && isAsync && propertyTypeSyntax is not null &&
                    (!IsValidAsyncReturnType(propertyType) || propertyType.TypeKind == TypeKind.Error);

                if (requiresAsyncReturnTypeDiagnostic)
                {
                    var display = propertyType.TypeKind == TypeKind.Error
                        ? propertyTypeSyntax.ToString()
                        : propertyType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
                    _diagnostics.ReportAsyncReturnTypeMustBeTaskLike(display, propertyTypeSyntax.GetLocation());
                }


                if (isAbstract && (accessor.Body is not null || accessor.ExpressionBody is not null))
                {
                    _diagnostics.ReportAbstractMemberCannotHaveBody(name, identifierToken.GetLocation());
                }

                var methodSymbol = new SourceMethodSymbol(
                    name,
                    returnType,
                    ImmutableArray<SourceParameterSymbol>.Empty,
                    propertySymbol,
                    _containingType,
                    CurrentNamespace!.AsSourceNamespace(),
                    [accessor.GetLocation()],
                    [accessor.GetReference()],
                    isStatic: isStatic,
                    methodKind: isGet ? MethodKind.PropertyGet : MethodKind.PropertySet,
                    isAsync: isAsync,
                    isVirtual: accessorVirtual,
                    isOverride: accessorOverride,
                    isSealed: accessorSealed,
                    isAbstract: isAbstract,
                    declaredAccessibility: indexerAccessibility);

                var parameters = new List<SourceParameterSymbol>();
                foreach (var param in indexerParameters)
                {
                    parameters.Add(new SourceParameterSymbol(
                        param.Syntax.Identifier.ValueText,
                        param.Type,
                        methodSymbol,
                        _containingType,
                        CurrentNamespace!.AsSourceNamespace(),
                        [param.Syntax.GetLocation()],
                        [param.Syntax.GetReference()],
                        param.RefKind,
                        param.HasDefaultValue,
                        param.DefaultValue,
                        param.IsMutable));
                }
                if (!isGet)
                {
                    parameters.Add(new SourceParameterSymbol(
                        "value",
                        propertyType,
                        methodSymbol,
                        _containingType,
                        CurrentNamespace!.AsSourceNamespace(),
                        [accessor.GetLocation()],
                        [accessor.GetReference()]));
                }
                methodSymbol.SetParameters(parameters);

                if (explicitInterfaceType is not null && explicitInterfaceProperty is not null)
                {
                    var interfaceAccessor = isGet
                        ? explicitInterfaceProperty.GetMethod
                        : explicitInterfaceProperty.SetMethod;

                    if (interfaceAccessor is not null)
                    {
                        methodSymbol.SetExplicitInterfaceImplementations(ImmutableArray.Create(interfaceAccessor));
                    }
                    else
                    {
                        _diagnostics.ReportExplicitInterfaceMemberNotFound(
                            explicitInterfaceType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            "Item",
                            accessor.Keyword.GetLocation());
                    }
                }

                if (accessorOverride)
                {
                    var overriddenMethod = isGet ? overriddenGetter : overriddenSetter;
                    if (overriddenMethod is not null)
                        methodSymbol.SetOverriddenMethod(overriddenMethod);
                }

                var binder = new MethodBinder(methodSymbol, this);
                binders[accessor] = binder;

                if (isGet)
                    getMethod = methodSymbol;
                else
                    setMethod = methodSymbol;
            }
        }
        else if (indexerDecl.ExpressionBody is not null)
        {
            var name = explicitIndexerAccessorPrefix + "get_" + propertySymbol.Name;
            var accessorOverride = isOverride && overriddenGetter is not null;
            var accessorVirtual = accessorOverride || isVirtual;
            var accessorSealed = accessorOverride && isSealed;

            var methodSymbol = new SourceMethodSymbol(
                name,
                propertyType,
                ImmutableArray<SourceParameterSymbol>.Empty,
                propertySymbol,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [indexerDecl.GetLocation()],
                [indexerDecl.GetReference()],
                isStatic: isStatic || isExtensionContainer,
                methodKind: MethodKind.PropertyGet,
                isAsync: false,
                isVirtual: accessorVirtual,
                isOverride: accessorOverride,
                isSealed: accessorSealed,
                isAbstract: isAbstract,
                declaredAccessibility: indexerAccessibility);

            if (isExtensionMember)
                methodSymbol.MarkDeclaredInExtension();

            if (isExtensionMember)
            {
                var extensionTypeParameters = CreateExtensionTypeParameters(methodSymbol);
                if (!extensionTypeParameters.IsDefaultOrEmpty)
                    methodSymbol.SetTypeParameters(extensionTypeParameters);
            }

            MethodBinder? binder = null;
            ITypeSymbol? receiverTypeForAccessor = receiverType;
            if (isExtensionMember && _extensionReceiverTypeSyntax is not null)
            {
                binder = new MethodBinder(methodSymbol, this);
                receiverTypeForAccessor = ResolveTypeSyntaxForSignature(binder, _extensionReceiverTypeSyntax, RefKind.None);
            }

            var parameters = new List<SourceParameterSymbol>();
            if (isExtensionMember && receiverTypeForAccessor is not null && _extensionReceiverTypeSyntax is not null)
            {
                var receiverNamespace = CurrentNamespace!.AsSourceNamespace();
                var selfParameter = new SourceParameterSymbol(
                    "self",
                    receiverTypeForAccessor,
                    methodSymbol,
                    _containingType,
                    receiverNamespace,
                    [_extensionReceiverTypeSyntax.GetLocation()],
                    [_extensionReceiverTypeSyntax.GetReference()]);
                parameters.Add(selfParameter);
            }
        }

        propertySymbol.SetAccessors(getMethod, setMethod);

        return binders;
    }

    private IPropertySymbol? FindPropertyOverrideCandidate(string name, ITypeSymbol propertyType, bool isStatic, bool isIndexer, (ITypeSymbol type, RefKind refKind)[] parameters)
    {
        for (var baseType = _containingType.BaseType; baseType is not null; baseType = baseType.BaseType)
        {
            foreach (var property in baseType.GetMembers(name).OfType<IPropertySymbol>())
            {
                if (property.IsIndexer != isIndexer)
                    continue;

                if (property.IsStatic != isStatic)
                    continue;

                var existingType = StripNullableReference(property.Type);
                var newType = StripNullableReference(propertyType);

                if (!SymbolEqualityComparer.Default.Equals(existingType, newType))
                    continue;

                if (isIndexer && !IndexerParametersMatch(property, parameters))
                    continue;

                return property;
            }
        }

        return null;
    }

    private IEventSymbol? FindEventOverrideCandidate(string name, ITypeSymbol eventType, bool isStatic)
    {
        for (var baseType = _containingType.BaseType; baseType is not null; baseType = baseType.BaseType)
        {
            foreach (var @event in baseType.GetMembers(name).OfType<IEventSymbol>())
            {
                if (@event.IsStatic != isStatic)
                    continue;

                var existingType = StripNullableReference(@event.Type);
                var newType = StripNullableReference(eventType);

                if (!SymbolEqualityComparer.Default.Equals(existingType, newType))
                    continue;

                return @event;
            }
        }

        return null;
    }

    private static bool IndexerParametersMatch(IPropertySymbol property, (ITypeSymbol type, RefKind refKind)[] parameters)
    {
        IMethodSymbol? accessor = property.GetMethod ?? property.SetMethod;
        if (accessor is null)
            return false;

        var accessorParameters = accessor.Parameters;
        var compareLength = accessor.MethodKind == MethodKind.PropertySet
            ? accessorParameters.Length - 1
            : accessorParameters.Length;

        if (compareLength != parameters.Length)
            return false;

        for (int i = 0; i < parameters.Length; i++)
        {
            var accessorParam = accessorParameters[i];
            var expected = parameters[i];

            if (accessorParam.RefKind != expected.refKind)
                return false;

            var accessorType = StripNullableReference(accessorParam.Type);
            var expectedType = StripNullableReference(expected.type);

            if (!SymbolEqualityComparer.Default.Equals(accessorType, expectedType))
                return false;
        }

        return true;
    }

    private IMethodSymbol? FindOverrideCandidate(string name, (ITypeSymbol type, RefKind refKind)[] parameters)
    {
        for (var baseType = _containingType.BaseType; baseType is not null; baseType = baseType.BaseType)
        {
            foreach (var method in baseType.GetMembers(name).OfType<IMethodSymbol>())
            {
                if (SignaturesMatch(method, parameters))
                    return method;
            }
        }

        return null;
    }

    private IMethodSymbol? FindHidingMethodCandidate(string name, bool isStatic, (ITypeSymbol type, RefKind refKind)[] parameters)
    {
        for (var baseType = _containingType.BaseType; baseType is not null; baseType = baseType.BaseType)
        {
            foreach (var method in baseType.GetMembers(name).OfType<IMethodSymbol>())
            {
                if (method.IsStatic != isStatic)
                    continue;

                if (SignaturesMatch(method, parameters))
                    return method;
            }
        }

        return null;
    }

    private IFieldSymbol? FindHidingFieldCandidate(string name, bool isStatic, ITypeSymbol fieldType)
    {
        for (var baseType = _containingType.BaseType; baseType is not null; baseType = baseType.BaseType)
        {
            foreach (var field in baseType.GetMembers(name).OfType<IFieldSymbol>())
            {
                if (field.IsStatic != isStatic)
                    continue;

                var existingType = StripNullableReference(field.Type);
                var newType = StripNullableReference(fieldType);

                if (!SymbolEqualityComparer.Default.Equals(existingType, newType))
                    continue;

                return field;
            }
        }

        return null;
    }

    private void ReportMemberHidingIfNeeded(ISymbol? hiddenMember, string memberName, bool hasNewModifier, Location location)
    {
        if (hiddenMember is null || hasNewModifier)
            return;

        _diagnostics.ReportMemberHidesInheritedMember(
            GetMemberDisplayName(memberName),
            GetInheritedMemberDisplayName(hiddenMember),
            location);
    }

    private static string GetInheritedMemberDisplayName(ISymbol member)
    {
        if (member.ContainingType is null)
            return member.Name;

        return $"{member.ContainingType.ToDisplayStringKeywordAware(TypeNameDiagnosticFormat)}.{member.Name}";
    }

    private static SyntaxToken ResolveExplicitInterfaceIdentifier(
        SyntaxToken identifier,
        ExplicitInterfaceSpecifierSyntax? explicitInterfaceSpecifier)
    {
        if (identifier.Kind == SyntaxKind.None && explicitInterfaceSpecifier is not null)
            return explicitInterfaceSpecifier.Identifier;

        return identifier;
    }

    internal readonly struct ParameterDefaultEvaluationResult
    {
        public ParameterDefaultEvaluationResult(
            bool hasDefaultSyntax,
            bool success,
            object? value,
            ParameterDefaultEvaluationFailure failure)
        {
            HasDefaultSyntax = hasDefaultSyntax;
            Success = success;
            Value = value;
            Failure = failure;
        }

        public bool HasDefaultSyntax { get; }

        public bool Success { get; }

        public object? Value { get; }

        public ParameterDefaultEvaluationFailure Failure { get; }
    }

    internal enum ParameterDefaultEvaluationFailure
    {
        None,
        NotConstant,
        NotConvertible,
    }

    internal readonly struct ParameterDefaultProcessingResult
    {
        public static readonly ParameterDefaultProcessingResult None = new(false, null);

        public ParameterDefaultProcessingResult(bool hasExplicitDefaultValue, object? explicitDefaultValue)
        {
            HasExplicitDefaultValue = hasExplicitDefaultValue;
            ExplicitDefaultValue = explicitDefaultValue;
        }

        public bool HasExplicitDefaultValue { get; }

        public object? ExplicitDefaultValue { get; }
    }

    internal static ParameterDefaultProcessingResult ProcessParameterDefault(
        ParameterSyntax parameterSyntax,
        ITypeSymbol parameterType,
        string parameterName,
        DiagnosticBag diagnostics,
        ref bool seenOptionalParameter)
    {
        var evaluation = EvaluateParameterDefaultValue(parameterSyntax, parameterType);

        if (!evaluation.HasDefaultSyntax)
        {
            if (seenOptionalParameter)
            {
                diagnostics.ReportOptionalParameterMustBeTrailing(parameterName, parameterSyntax.Identifier.GetLocation());
            }

            return ParameterDefaultProcessingResult.None;
        }

        seenOptionalParameter = true;

        if (!evaluation.Success)
        {
            var defaultLocation = parameterSyntax.DefaultValue!.Value.GetLocation();

            switch (evaluation.Failure)
            {
                case ParameterDefaultEvaluationFailure.NotConstant:
                    diagnostics.ReportParameterDefaultValueMustBeConstant(parameterName, defaultLocation);
                    break;
                case ParameterDefaultEvaluationFailure.NotConvertible:
                    if (parameterType.TypeKind != TypeKind.Error)
                    {
                        var parameterTypeDisplay = parameterType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
                        diagnostics.ReportParameterDefaultValueCannotConvert(parameterName, parameterTypeDisplay, defaultLocation);
                    }
                    break;
            }

            return ParameterDefaultProcessingResult.None;
        }

        return new ParameterDefaultProcessingResult(true, evaluation.Value);
    }

    private string GetMemberDisplayName(string memberName)
    {
        return _containingType is null
            ? memberName
            : $"{_containingType.ToDisplayStringKeywordAware(TypeNameDiagnosticFormat)}.{memberName}";
    }

    private static string GetMethodKindDisplay(MethodKind methodKind)
    {
        return methodKind switch
        {
            MethodKind.Constructor or MethodKind.NamedConstructor => "constructor",
            MethodKind.UserDefinedOperator => "operator",
            _ => "method",
        };
    }

    private void ValidateTypeAccessibility(
        ITypeSymbol type,
        Accessibility memberAccessibility,
        string memberKind,
        string memberName,
        string typeRole,
        Location location)
    {
        if (type.TypeKind == TypeKind.Error)
            return;

        var effectiveMemberAccessibility = AccessibilityUtilities.GetEffectiveAccessibility(memberAccessibility, _containingType);

        if (AccessibilityUtilities.IsTypeLessAccessibleThan(type, effectiveMemberAccessibility))
        {
            var typeDisplay = type.ToDisplayStringKeywordAware(TypeNameDiagnosticFormat);
            _diagnostics.ReportTypeIsLessAccessibleThanMember(typeRole, typeDisplay, memberKind, memberName, location);
        }
    }

    internal static ParameterDefaultEvaluationResult EvaluateParameterDefaultValue(
        ParameterSyntax parameterSyntax,
        ITypeSymbol parameterType)
    {
        if (parameterSyntax.DefaultValue is null)
            return new ParameterDefaultEvaluationResult(false, success: false, value: null, ParameterDefaultEvaluationFailure.None);

        if (!ConstantValueEvaluator.TryEvaluate(parameterSyntax.DefaultValue.Value, out var rawValue))
            return new ParameterDefaultEvaluationResult(true, success: false, value: null, ParameterDefaultEvaluationFailure.NotConstant);

        if (!ConstantValueEvaluator.TryConvert(parameterType, rawValue, out var defaultValue))
            return new ParameterDefaultEvaluationResult(true, success: false, value: null, ParameterDefaultEvaluationFailure.NotConvertible);

        return new ParameterDefaultEvaluationResult(true, success: true, defaultValue, ParameterDefaultEvaluationFailure.None);
    }

    private static VarianceKind GetDeclaredVariance(TypeParameterSyntax parameter)
    {
        return parameter.VarianceKeyword?.Kind switch
        {
            SyntaxKind.OutKeyword => VarianceKind.Out,
            SyntaxKind.InKeyword => VarianceKind.In,
            _ => VarianceKind.None,
        };
    }

    private void ValidateAbstractMemberInNonAbstractType(
        bool isAbstract,
        string memberDisplayName,
        Location location)
    {
        if (!isAbstract)
            return;

        // Interfaces are special (members may be abstract-like by default).
        if (_containingType.TypeKind == TypeKind.Interface)
            return;

        // If you only want to enforce the “class must be abstract” rule:
        if (_containingType.TypeKind == TypeKind.Class && !_containingType.IsAbstract)
        {
            _diagnostics.ReportAbstractMemberInNonAbstractType(
                memberDisplayName,
                _containingType.ToDisplayStringKeywordAware(TypeNameDiagnosticFormat),
                location);
        }

        // Optional stricter rule (C#-like): abstract members not allowed in structs.
        // else if (_containingType.TypeKind == TypeKind.Struct)
        // {
        //     _diagnostics.ReportAbstractMemberNotAllowedInStruct(memberDisplayName, location);
        // }
    }

    private void ReportInstanceMemberInStaticTypeIfNeeded(
        bool isStaticMember,
        string memberName,
        Location location)
    {
        if (!_containingType.IsStatic || isStaticMember)
            return;

        _diagnostics.ReportStaticClassCannotContainInstanceMember(
            _containingType.ToDisplayStringKeywordAware(TypeNameDiagnosticFormat),
            memberName,
            location);
    }

    private void ReportPartialModifierNotSupported(
        SyntaxTokenList modifiers,
        string memberKind,
        string memberName)
    {
        foreach (var modifier in modifiers)
        {
            if (!modifier.IsKind(SyntaxKind.PartialKeyword))
                continue;

            _diagnostics.ReportModifierNotValidOnMember("partial", memberKind, memberName, modifier.GetLocation());
        }
    }

    private void ValidateInheritanceModifiers(
        ref bool isAbstract,
        ref bool isVirtual,
        ref bool isOverride,
        ref bool isSealed,
        bool isStatic,
        string memberName,
        Location location)
    {
        if (isVirtual && isOverride)
        {
            _diagnostics.ReportInvalidMemberModifierCombination(memberName, "virtual", "override", location);
            isVirtual = false;
        }

        if (isAbstract && isVirtual)
        {
            _diagnostics.ReportInvalidMemberModifierCombination(memberName, "abstract", "virtual", location);
            isVirtual = false;
        }

        if (isAbstract && isSealed)
        {
            _diagnostics.ReportInvalidMemberModifierCombination(memberName, "abstract", "sealed", location);
            isSealed = false;
        }

        if (isAbstract && isStatic)
        {
            _diagnostics.ReportInvalidMemberModifierCombination(memberName, "abstract", "static", location);
            isAbstract = false;
        }
    }

    private int AddExtensionContainerTypeParameters(
        SourceMethodSymbol methodSymbol,
        ImmutableArray<ITypeParameterSymbol>.Builder builder)
    {
        if (!IsExtensionContainer || _containingType.TypeParameters.IsDefaultOrEmpty)
            return 0;

        var receiverNamespace = CurrentNamespace!.AsSourceNamespace();
        var ordinal = 0;

        foreach (var tp in _containingType.TypeParameters.OfType<SourceTypeParameterSymbol>())
        {
            builder.Add(new SourceTypeParameterSymbol(
                tp.Name,
                methodSymbol,
                _containingType,
                receiverNamespace,
                tp.Locations.ToArray(),
                tp.DeclaringSyntaxReferences.ToArray(),
                ordinal++,
                tp.ConstraintKind,
                tp.ConstraintTypeReferences,
                tp.Variance));
        }

        return ordinal;
    }

    private void InitializeMethodTypeParameters(
        SourceMethodSymbol methodSymbol,
        TypeParameterListSyntax? typeParameterList)
    {
        var hasDeclared = typeParameterList is { Parameters.Count: > 0 };
        var hasExtension = IsExtensionContainer && !_containingType.TypeParameters.IsDefaultOrEmpty;

        if (!hasDeclared && !hasExtension)
            return;

        var builder = ImmutableArray.CreateBuilder<ITypeParameterSymbol>(
            (hasExtension ? _containingType.TypeParameters.Length : 0) +
            (hasDeclared ? typeParameterList!.Parameters.Count : 0));

        var ordinal = AddExtensionContainerTypeParameters(methodSymbol, builder);

        if (hasDeclared)
        {
            foreach (var tpSyntax in typeParameterList!.Parameters)
            {
                var (constraintKind, constraintTypeRefs) = TypeParameterConstraintAnalyzer.AnalyzeInline(tpSyntax);
                var variance = GetDeclaredVariance(tpSyntax);

                builder.Add(new SourceTypeParameterSymbol(
                    tpSyntax.Identifier.ValueText,
                    methodSymbol,
                    _containingType,
                    CurrentNamespace!.AsSourceNamespace(),
                    [tpSyntax.GetLocation()],
                    [tpSyntax.GetReference()],
                    ordinal++,
                    constraintKind,
                    constraintTypeRefs,
                    variance));
            }
        }

        methodSymbol.SetTypeParameters(builder);
    }
}
