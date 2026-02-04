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
    public Dictionary<SyntaxNode, Binder> BindPropertyDeclaration(PropertyDeclarationSyntax propertyDecl)
    {
        // NOTE: For extension properties, the container type parameters (e.g. <T, E>) are lowered onto the
        // accessor methods as *method-owned* type parameters. We must bind the declared property type (e.g. Option<T>)
        // against that lowered identity, otherwise the property type will leak Option<T> at use sites.
        var propertyTypeSyntax = propertyDecl.Type.Type;

        // The real declared type (used for accessor signatures / binding)
        var declaredPropertyType = ResolveType(propertyTypeSyntax);

        // The property symbol itself is a marker for extension containers.
        // It must be codegen-safe because the extension container type is not emitted as generic.
        var propertyType = declaredPropertyType;

        var modifiers = propertyDecl.Modifiers;
        var hasStaticModifier = modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);
        var isStatic = hasStaticModifier;
        var isAbstract = modifiers.Any(m => m.Kind == SyntaxKind.AbstractKeyword);
        var isVirtual = modifiers.Any(m => m.Kind == SyntaxKind.VirtualKeyword);
        var isOverride = modifiers.Any(m => m.Kind == SyntaxKind.OverrideKeyword);
        var isSealed = modifiers.Any(m => m.Kind is SyntaxKind.SealedKeyword or SyntaxKind.FinalKeyword);
        var hasNewModifier = modifiers.Any(m => m.Kind == SyntaxKind.NewKeyword);
        var isRequired = modifiers.Any(m => m.Kind == SyntaxKind.RequiredKeyword);
        var defaultAccessibility = AccessibilityUtilities.GetDefaultMemberAccessibility(_containingType);
        var propertyAccessibility = AccessibilityUtilities.DetermineAccessibility(modifiers, defaultAccessibility);
        var explicitInterfaceSpecifier = propertyDecl.ExplicitInterfaceSpecifier;
        var identifierToken = ResolveExplicitInterfaceIdentifier(propertyDecl.Identifier, explicitInterfaceSpecifier);
        var propertyName = identifierToken.Text;
        var metadataName = propertyName;
        INamedTypeSymbol? explicitInterfaceType = null;
        string? explicitInterfaceMetadataName = null;
        IPropertySymbol? explicitInterfaceProperty = null;

        var isExtensionContainer = IsExtensionContainer;
        var isExtensionMember = isExtensionContainer && !hasStaticModifier;

        if (isExtensionMember)
        {
            // Marker-only: real generic type parameters belong on accessor methods.
            propertyType = Compilation.GetSpecialType(SpecialType.System_Object);
        }

        if (isExtensionContainer)
        {
            isAbstract = false;
            isVirtual = false;
            isOverride = false;
            isSealed = false;
            propertyAccessibility = modifiers.Any(m => m.Kind == SyntaxKind.InternalKeyword)
                ? Accessibility.Internal
                : Accessibility.Public;

            if (isExtensionMember)
                isStatic = false;
        }

        if (explicitInterfaceSpecifier is not null)
        {
            var resolved = ResolveType(explicitInterfaceSpecifier.Name);
            if (resolved is INamedTypeSymbol interfaceType && interfaceType.TypeKind == TypeKind.Interface)
            {
                explicitInterfaceType = interfaceType;
                var interfaceMetadataName = GetInterfaceMetadataName(interfaceType);
                metadataName = $"{interfaceMetadataName}.{propertyName}";
                explicitInterfaceMetadataName = interfaceMetadataName;
                propertyAccessibility = Accessibility.Private;

                if (!ImplementsInterface(interfaceType))
                {
                    _diagnostics.ReportContainingTypeDoesNotImplementInterface(
                        _containingType.Name,
                        interfaceType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        explicitInterfaceSpecifier.Name.GetLocation());
                }

                explicitInterfaceProperty = FindExplicitInterfacePropertyImplementation(
                    interfaceType,
                    propertyName,
                    propertyType,
                    isIndexer: false,
                    parameters: Array.Empty<(ITypeSymbol type, RefKind refKind)>());

                if (explicitInterfaceProperty is null)
                {
                    _diagnostics.ReportExplicitInterfaceMemberNotFound(
                        interfaceType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        propertyName,
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
            propertyName,
            identifierToken.GetLocation());

        ValidateInheritanceModifiers(
            ref isAbstract,
            ref isVirtual,
            ref isOverride,
            ref isSealed,
            isStatic,
            propertyName,
            identifierToken.GetLocation());

        ValidateAbstractMemberInNonAbstractType(
            isAbstract,
            GetMemberDisplayName(propertyName),
            identifierToken.GetLocation());

        if (isSealed && !isOverride)
        {
            _diagnostics.ReportSealedMemberMustOverride(propertyName, identifierToken.GetLocation());
            isSealed = false;
        }

        if (isStatic && (isVirtual || isOverride))
        {
            if (isVirtual)
                _diagnostics.ReportStaticMemberCannotBeVirtualOrOverride(propertyName, "virtual", identifierToken.GetLocation());
            if (isOverride)
                _diagnostics.ReportStaticMemberCannotBeVirtualOrOverride(propertyName, "override", identifierToken.GetLocation());

            isVirtual = false;
            isOverride = false;
            isSealed = false;
        }

        if (isVirtual && !isOverride && _containingType.IsSealed)
        {
            _diagnostics.ReportVirtualMemberInSealedType(propertyName, _containingType.Name, identifierToken.GetLocation());
            isVirtual = false;
        }

        ITypeSymbol? receiverType = null;

        ValidateTypeAccessibility(
            isExtensionMember ? declaredPropertyType : propertyType,
            propertyAccessibility,
            "property",
            GetMemberDisplayName(propertyName),
            "property",
            propertyTypeSyntax.GetLocation());

        IPropertySymbol propertySymbol;
        SourcePropertySymbol? sourcePropertySymbol = null;

        if (isExtensionMember)
        {
            // No marker properties for extension properties; synthesize property symbol from accessors.
            propertySymbol = new SynthesizedExtensionPropertySymbol(
                _containingType,
                [propertyDecl.GetLocation()],
                [propertyDecl.GetReference()],
                name: propertyName);
        }
        else
        {
            sourcePropertySymbol = new SourcePropertySymbol(
                propertyName,
                propertyType,
                _containingType,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [propertyDecl.GetLocation()],
                [propertyDecl.GetReference()],
                isStatic: isStatic,
                metadataName: metadataName,
                declaredAccessibility: propertyAccessibility);

            propertySymbol = sourcePropertySymbol;
        }

        if (isRequired && sourcePropertySymbol is not null)
            sourcePropertySymbol.MarkAsRequired();

        // Bind property initializer (if any) with the property's declared type as target type.
        BoundExpression? initializer = null;
        if (propertyDecl.Initializer is not null)
        {
            var exprBinder = new BlockBinder(_containingType, this);
            var targetType = exprBinder.PushTargetType(propertyType);

            initializer = exprBinder.BindExpression(propertyDecl.Initializer.Value);

            targetType.Dispose();

            foreach (var diag in exprBinder.Diagnostics.AsEnumerable())
                _diagnostics.Report(diag);
        }

        if (!isExtensionContainer &&
            _containingType.TypeKind != TypeKind.Interface &&
            propertyDecl.AccessorList is { } accessorList &&
            accessorList.Accessors.All(a => a.Body is null && a.ExpressionBody is null))
        {
            var backingField = new SourceFieldSymbol(
                $"<{propertySymbol.Name}>k__BackingField",
                propertyType,
                isStatic: isStatic,
                isMutable: true,
                isConst: false,
                constantValue: null,
                _containingType,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [propertyDecl.GetLocation()],
                [propertyDecl.GetReference()],
                initializer: initializer,
                declaredAccessibility: Accessibility.Private);

            sourcePropertySymbol?.SetBackingField(backingField);
        }

        var hasExpressionBody = propertyDecl.ExpressionBody is not null;
        var hasGetter = propertyDecl.AccessorList?.Accessors.Any(a => a.Kind == SyntaxKind.GetAccessorDeclaration) ?? hasExpressionBody;
        var hasSetter = propertyDecl.AccessorList?.Accessors.Any(a =>
            a.Kind == SyntaxKind.SetAccessorDeclaration ||
            a.Kind == SyntaxKind.InitAccessorDeclaration) ?? false;

        IMethodSymbol? overriddenGetter = null;
        IMethodSymbol? overriddenSetter = null;

        if (isOverride)
        {
            var candidate = FindPropertyOverrideCandidate(propertyName, propertyType, isStatic, isIndexer: false, parameters: Array.Empty<(ITypeSymbol type, RefKind refKind)>());
            bool overrideValid = true;

            if (candidate is null)
            {
                _diagnostics.ReportOverrideMemberNotFound(propertyName, "property", identifierToken.GetLocation());
                overrideValid = false;
            }
            else
            {
                if (hasGetter)
                {
                    if (candidate.GetMethod is null || !candidate.GetMethod.IsVirtual)
                    {
                        _diagnostics.ReportOverrideMemberNotFound(propertyName, "property", identifierToken.GetLocation());
                        overrideValid = false;
                    }
                    else if (candidate.GetMethod.IsFinal)
                    {
                        _diagnostics.ReportCannotOverrideSealedMember(propertyName, candidate.Name, identifierToken.GetLocation());
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
                        _diagnostics.ReportOverrideMemberNotFound(propertyName, "property", identifierToken.GetLocation());
                        overrideValid = false;
                    }
                    else if (candidate.SetMethod.IsFinal)
                    {
                        _diagnostics.ReportCannotOverrideSealedMember(propertyName, candidate.Name, identifierToken.GetLocation());
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
            var hiddenMember = FindPropertyOverrideCandidate(propertyName, propertyType, isStatic, isIndexer: false, parameters: Array.Empty<(ITypeSymbol type, RefKind refKind)>());
            ReportMemberHidingIfNeeded(hiddenMember, propertyName, hasNewModifier, identifierToken.GetLocation());
        }

        var binders = new Dictionary<SyntaxNode, Binder>();

        SourceMethodSymbol? getMethod = null;
        SourceMethodSymbol? setMethod = null;


        IReadOnlyDictionary<string, ITypeSymbol>? BuildExtensionTypeParameterSubstitutions(ImmutableArray<ITypeParameterSymbol> accessorTypeParameters)
        {
            if (!IsExtensionContainer || _containingType.TypeParameters.IsDefaultOrEmpty)
                return null;

            if (accessorTypeParameters.IsDefaultOrEmpty || accessorTypeParameters.Length < _containingType.TypeParameters.Length)
                return null;

            var map = new Dictionary<string, ITypeSymbol>(StringComparer.Ordinal);
            for (int i = 0; i < _containingType.TypeParameters.Length; i++)
                map[_containingType.TypeParameters[i].Name] = accessorTypeParameters[i];

            return map;
        }

        void EnsureExtensionTypeParametersOnAccessor(SourceMethodSymbol accessor)
        {
            if (!IsExtensionContainer || _containingType.TypeParameters.IsDefaultOrEmpty)
                return;

            // Expected invariant: lowered extension type params are the first method type params.
            if (!accessor.TypeParameters.IsDefaultOrEmpty &&
                accessor.TypeParameters.Length >= _containingType.TypeParameters.Length)
                return;

            var lowered = CreateExtensionTypeParameters(accessor);
            if (lowered.IsDefaultOrEmpty)
                return;

            var existing = accessor.TypeParameters;
            var merged = ImmutableArray.CreateBuilder<ITypeParameterSymbol>(lowered.Length + existing.Length);
            merged.AddRange(lowered);
            merged.AddRange(existing);
            accessor.SetTypeParameters(merged.ToImmutable());
        }

        var explicitAccessorPrefix = explicitInterfaceMetadataName is not null
            ? explicitInterfaceMetadataName + "."
            : string.Empty;

        if (isAbstract && propertyDecl.ExpressionBody is not null)
        {
            _diagnostics.ReportAbstractMemberCannotHaveBody(propertyName, identifierToken.GetLocation());
        }


        if (propertyDecl.AccessorList is not null)
        {
            foreach (var accessor in propertyDecl.AccessorList.Accessors)
            {
                bool isGet = accessor.Kind == SyntaxKind.GetAccessorDeclaration;
                bool isSet = accessor.Kind == SyntaxKind.SetAccessorDeclaration;
                bool isInit = accessor.Kind == SyntaxKind.InitAccessorDeclaration;

                // Ignore non-get/non-set/non-init accessors.
                if (!isGet && !isSet && !isInit)
                    continue;

                static Accessibility? GetExplicitAccessorAccessibility(SyntaxTokenList mods)
                {
                    var hasPublic = mods.Any(m => m.Kind == SyntaxKind.PublicKeyword);
                    var hasPrivate = mods.Any(m => m.Kind == SyntaxKind.PrivateKeyword);
                    var hasProtected = mods.Any(m => m.Kind == SyntaxKind.ProtectedKeyword);
                    var hasInternal = mods.Any(m => m.Kind == SyntaxKind.InternalKeyword);

                    // No explicit accessor accessibility.
                    if (!hasPublic && !hasPrivate && !hasProtected && !hasInternal)
                        return null;

                    // Treat combinations similarly to C# where applicable.
                    if (hasProtected && hasInternal)
                        return Accessibility.ProtectedOrInternal;

                    if (hasPublic)
                        return Accessibility.Public;
                    if (hasPrivate)
                        return Accessibility.Private;
                    if (hasProtected)
                        return Accessibility.ProtectedAndProtected;
                    if (hasInternal)
                        return Accessibility.Internal;

                    return null;
                }

                // Accessor accessibility defaults to the property's accessibility unless explicitly overridden.
                // For explicit interface implementations we always force private.
                var explicitAccessorAccessibility = GetExplicitAccessorAccessibility(accessor.Modifiers);
                var accessorAccessibility = explicitInterfaceType is not null
                    ? Accessibility.Private
                    : explicitAccessorAccessibility ?? propertyAccessibility;

                var returnType = Compilation.GetSpecialType(SpecialType.System_Unit);
                var name = explicitAccessorPrefix + (isGet ? "get_" : "set_") + propertyName;

                var accessorOverride = isOverride && (isGet ? overriddenGetter is not null : overriddenSetter is not null);
                var accessorVirtual = accessorOverride || isVirtual;
                var accessorSealed = accessorOverride && isSealed;

                var isAsync = accessor.Modifiers.Any(m => m.Kind == SyntaxKind.AsyncKeyword);

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
                    isStatic: isStatic || isExtensionContainer,
                    methodKind: isGet ? MethodKind.PropertyGet : (isInit ? MethodKind.InitOnly : MethodKind.PropertySet),
                    isAsync: isAsync,
                    isVirtual: accessorVirtual,
                    isOverride: accessorOverride,
                    isSealed: accessorSealed,
                    isAbstract: isAbstract,
                    declaredAccessibility: accessorAccessibility);

                if (isExtensionMember)
                    methodSymbol.MarkDeclaredInExtension();

                if (isExtensionMember)
                {
                    var extensionTypeParameters = CreateExtensionTypeParameters(methodSymbol);
                    if (!extensionTypeParameters.IsDefaultOrEmpty)
                        methodSymbol.SetTypeParameters(extensionTypeParameters);
                }

                // Ensure lowered extension type parameters are present so we can bind Option<T> / Result<T,E>
                // against the accessor's method-owned type parameters.
                if (isExtensionMember)
                    EnsureExtensionTypeParametersOnAccessor(methodSymbol);

                ITypeSymbol propertyTypeForAccessor = declaredPropertyType;
                ITypeSymbol? receiverTypeForAccessor = receiverType;

                if (isExtensionMember && _extensionReceiverTypeSyntax is not null)
                {
                    var substitutions = BuildExtensionTypeParameterSubstitutions(methodSymbol.TypeParameters);
                    var options = substitutions is null
                        ? null
                        : new Binder.TypeResolutionOptions
                        {
                            TypeParameterSubstitutions = substitutions,
                            SubstitutionPrecedence = Binder.SubstitutionPrecedence.OptionsWin
                        };

                    var accessorBinder = new MethodBinder(methodSymbol, this);

                    var boundPropType = accessorBinder.BindType(propertyTypeSyntax, options);
                    propertyTypeForAccessor = boundPropType.ResolvedType;

                    var boundReceiver = accessorBinder.BindType(_extensionReceiverTypeSyntax, options);
                    receiverTypeForAccessor = boundReceiver.ResolvedType;
                }

                // Now that we have the correct accessor-bound property type, set the accessor return type.
                methodSymbol.SetReturnType(isGet
                    ? propertyTypeForAccessor
                    : Compilation.GetSpecialType(SpecialType.System_Unit));

                // Binder is still needed for the accessor body.
                MethodBinder binder = new MethodBinder(methodSymbol, this);

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

                if (!isGet)
                {
                    parameters.Add(new SourceParameterSymbol(
                        "value",
                        propertyTypeForAccessor,
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
                            propertyName,
                            accessor.Keyword.GetLocation());
                    }
                }

                if (accessorOverride)
                {
                    var overriddenMethod = isGet ? overriddenGetter : overriddenSetter;
                    if (overriddenMethod is not null)
                        methodSymbol.SetOverriddenMethod(overriddenMethod);
                }

                binders[accessor] = binder;

                if (isGet)
                    getMethod = methodSymbol;
                else
                    setMethod = methodSymbol;
            }
        }
        else if (propertyDecl.ExpressionBody is not null)
        {
            var name = explicitAccessorPrefix + "get_" + propertyName;

            var accessorAccessibility = explicitInterfaceType is not null
                ? Accessibility.Private
                : propertyAccessibility;

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
                [propertyDecl.GetLocation()],
                [propertyDecl.GetReference()],
                isStatic: isStatic || isExtensionContainer,
                methodKind: MethodKind.PropertyGet,
                isAsync: false,
                isVirtual: accessorVirtual,
                isOverride: accessorOverride,
                isSealed: accessorSealed,
                isAbstract: isAbstract,
                declaredAccessibility: accessorAccessibility);

            if (isExtensionMember)
                methodSymbol.MarkDeclaredInExtension();

            if (isExtensionMember)
            {
                var extensionTypeParameters = CreateExtensionTypeParameters(methodSymbol);
                if (!extensionTypeParameters.IsDefaultOrEmpty)
                    methodSymbol.SetTypeParameters(extensionTypeParameters);
            }

            if (isExtensionMember)
                EnsureExtensionTypeParametersOnAccessor(methodSymbol);

            ITypeSymbol propertyTypeForAccessor = propertyType; // keep propertyType codegen-safe
            ITypeSymbol? receiverTypeForAccessor = receiverType;

            var binder = new MethodBinder(methodSymbol, this);

            if (isExtensionMember && _extensionReceiverTypeSyntax is not null)
            {
                var substitutions = BuildExtensionTypeParameterSubstitutions(methodSymbol.TypeParameters);
                var options = substitutions is null
                    ? null
                    : new Binder.TypeResolutionOptions
                    {
                        TypeParameterSubstitutions = substitutions,
                        SubstitutionPrecedence = Binder.SubstitutionPrecedence.OptionsWin
                    };

                propertyTypeForAccessor = binder.BindType(propertyTypeSyntax, options).ResolvedType;
                receiverTypeForAccessor = binder.BindType(_extensionReceiverTypeSyntax, options).ResolvedType;
            }

            // Use the accessor-bound property type for the getter signature.
            methodSymbol.SetReturnType(propertyTypeForAccessor);

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

            methodSymbol.SetParameters(parameters);

            if (explicitInterfaceType is not null && explicitInterfaceProperty?.GetMethod is not null)
            {
                methodSymbol.SetExplicitInterfaceImplementations(ImmutableArray.Create(explicitInterfaceProperty.GetMethod));
            }
            else if (explicitInterfaceType is not null)
            {
                _diagnostics.ReportExplicitInterfaceMemberNotFound(
                    explicitInterfaceType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    propertyName,
                    propertyDecl.Identifier.GetLocation());
            }

            if (accessorOverride && overriddenGetter is not null)
                methodSymbol.SetOverriddenMethod(overriddenGetter);

            var expressionBodyBinder = new MethodBodyBinder(methodSymbol, binder);

            binders[propertyDecl.ExpressionBody!] = expressionBodyBinder;

            getMethod = methodSymbol;
        }

        if (isExtensionMember && _extensionReceiverTypeSyntax is not null)
        {
            // If receiverType was already bound earlier using lowered extension type parameter substitutions, keep it.
            if (receiverType is not null)
            {
                sourcePropertySymbol?.MarkDeclaredInExtension(receiverType);

                ValidateTypeAccessibility(
                    receiverType,
                    propertyAccessibility,
                    "property",
                    GetMemberDisplayName(propertyName),
                    "receiver",
                    _extensionReceiverTypeSyntax.GetLocation());

                goto __receiverDone;
            }

            // Extension properties are lowered to static accessor methods.
            // Bind the receiver type syntax against the accessor's method-owned type parameters.
            var accessorForReceiver = getMethod ?? setMethod;

            if (accessorForReceiver is not null && IsExtensionContainer && !_containingType.TypeParameters.IsDefaultOrEmpty)
            {
                // If lowering didn't project extension type parameters onto the accessor, synthesize them.
                if (accessorForReceiver.TypeParameters.IsDefaultOrEmpty ||
                    accessorForReceiver.TypeParameters.Length < _containingType.TypeParameters.Length)
                {
                    var loweredExtensionTypeParameters = CreateExtensionTypeParameters(accessorForReceiver);
                    if (!loweredExtensionTypeParameters.IsDefaultOrEmpty)
                    {
                        var merged = ImmutableArray.CreateBuilder<ITypeParameterSymbol>(
                            loweredExtensionTypeParameters.Length + accessorForReceiver.TypeParameters.Length);
                        merged.AddRange(loweredExtensionTypeParameters);
                        merged.AddRange(accessorForReceiver.TypeParameters);
                        accessorForReceiver.SetTypeParameters(merged.ToImmutable());
                    }
                }

                var substitutions = BuildExtensionTypeParameterSubstitutions(accessorForReceiver.TypeParameters);

                var accessorBinder = new MethodBinder(accessorForReceiver, this);
                var bound = accessorBinder.BindType(
                    _extensionReceiverTypeSyntax,
                    substitutions is null
                        ? null
                        : new Binder.TypeResolutionOptions
                        {
                            TypeParameterSubstitutions = substitutions,
                            SubstitutionPrecedence = Binder.SubstitutionPrecedence.OptionsWin
                        });

                receiverType = bound.ResolvedType;
            }
            else
            {
                // Fallback: bind without substitutions.
                receiverType = GetExtensionReceiverType();
            }

            sourcePropertySymbol?.MarkDeclaredInExtension(receiverType);

            ValidateTypeAccessibility(
                receiverType,
                propertyAccessibility,
                "property",
                GetMemberDisplayName(propertyName),
                "receiver",
                _extensionReceiverTypeSyntax.GetLocation());
        __receiverDone:
            ;
        }

        if (sourcePropertySymbol is not null)
        {
            sourcePropertySymbol.SetAccessors(getMethod, setMethod);
        }
        else if (propertySymbol is SynthesizedExtensionPropertySymbol synthesized)
        {
            synthesized.SetAccessors(getMethod, setMethod);
        }

        return binders;
    }
}
