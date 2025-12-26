using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal class TypeMemberBinder : Binder
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

    public new INamedTypeSymbol ContainingSymbol => _containingType;

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

    private ITypeSymbol GetExtensionReceiverType()
    {
        if (!_extensionReceiverTypeComputed)
        {
            _extensionReceiverType = _extensionReceiverTypeSyntax is null
                ? null
                : ResolveType(_extensionReceiverTypeSyntax);
            _extensionReceiverTypeComputed = true;
        }

        return _extensionReceiverType ?? Compilation.ErrorTypeSymbol;
    }

    public override ISymbol? BindDeclaredSymbol(SyntaxNode node)
    {
        return node switch
        {
            MethodDeclarationSyntax method => BindMethodSymbol(method),
            ConstructorDeclarationSyntax ctor => BindConstructorSymbol(ctor),
            NamedConstructorDeclarationSyntax namedCtor => BindConstructorSymbol(namedCtor),
            OperatorDeclarationSyntax opDecl => BindOperatorSymbol(opDecl),
            PropertyDeclarationSyntax property => BindPropertySymbol(property),
            IndexerDeclarationSyntax indexer => BindIndexerSymbol(indexer),
            AccessorDeclarationSyntax accessor => BindAccessorSymbol(accessor),
            VariableDeclaratorSyntax variable => BindFieldSymbol(variable),
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

    private ISymbol? BindConstructorSymbol(BaseConstructorDeclarationSyntax ctor)
    {
        string name = ".ctor";
        if (ctor is NamedConstructorDeclarationSyntax namedCtor)
            name = namedCtor.Identifier.ValueText;

        return _containingType.GetMembers()
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m => m.Name == name &&
                                 m.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == ctor));
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
            .FirstOrDefault(m => m!.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == accessor));
    }

    public void BindFieldDeclaration(FieldDeclarationSyntax fieldDecl)
    {
        var bindingKeyword = fieldDecl.Declaration.BindingKeyword;
        var isConstDeclaration = bindingKeyword.IsKind(SyntaxKind.ConstKeyword);
        var isStatic = fieldDecl.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword) || isConstDeclaration;
        var fieldAccessibility = AccessibilityUtilities.DetermineAccessibility(
            fieldDecl.Modifiers,
            AccessibilityUtilities.GetDefaultMemberAccessibility(_containingType));

        foreach (var decl in fieldDecl.Declaration.Declarators)
        {
            ITypeSymbol? fieldType = decl.TypeAnnotation is null
                ? null
                : ResolveType(decl.TypeAnnotation.Type);

            BoundExpression? initializer = null;
            object? constantValue = null;
            var constantValueComputed = false;

            if (decl.Initializer is not null)
            {
                var exprBinder = new BlockBinder(_containingType, this);
                initializer = exprBinder.BindExpression(decl.Initializer.Value);

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

            var fieldTypeLocation = decl.TypeAnnotation?.Type.GetLocation() ?? decl.Identifier.GetLocation();
            ValidateTypeAccessibility(
                fieldType,
                fieldAccessibility,
                "field",
                GetMemberDisplayName(decl.Identifier.ValueText),
                $"field '{decl.Identifier.ValueText}'",
                fieldTypeLocation);

            _ = new SourceFieldSymbol(
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
                [decl.GetReference()],
                initializerForSymbol,
                declaredAccessibility: fieldAccessibility
            );
        }
    }

    public MethodBinder BindMethodDeclaration(MethodDeclarationSyntax methodDecl)
    {
        var explicitInterfaceSpecifier = methodDecl.ExplicitInterfaceSpecifier;
        var identifierToken = ResolveExplicitInterfaceIdentifier(methodDecl.Identifier, explicitInterfaceSpecifier);
        var name = identifierToken.Kind == SyntaxKind.SelfKeyword ? "Invoke" : identifierToken.ValueText;
        INamedTypeSymbol? explicitInterfaceType = null;
        IMethodSymbol? explicitInterfaceMember = null;

        var metadataName = name;
        var displayName = name;
        var isExtensionContainer = IsExtensionContainer;

        if (explicitInterfaceSpecifier is not null)
        {
            var resolved = ResolveType(explicitInterfaceSpecifier.Name);
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
        var isSealed = modifiers.Any(m => m.Kind == SyntaxKind.SealedKeyword);
        var defaultAccessibility = AccessibilityUtilities.GetDefaultMemberAccessibility(_containingType);
        var methodAccessibility = AccessibilityUtilities.DetermineAccessibility(modifiers, defaultAccessibility);

        if (isExtensionContainer)
        {
            isStatic = true;
            isVirtual = false;
            isOverride = false;
            isSealed = false;
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
            methodAccessibility = Accessibility.Private;
        }

        if (isSealed && !isOverride)
        {
            _diagnostics.ReportSealedMemberMustOverride(name, identifierToken.GetLocation());
            isSealed = false;
        }

        if (isStatic && (isVirtual || isOverride))
        {
            if (isVirtual)
                _diagnostics.ReportStaticMemberCannotBeVirtualOrOverride(name, "virtual", identifierToken.GetLocation());
            if (isOverride)
                _diagnostics.ReportStaticMemberCannotBeVirtualOrOverride(name, "override", identifierToken.GetLocation());

            isVirtual = false;
            isOverride = false;
            isSealed = false;
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
            declaredAccessibility: methodAccessibility);

        var isExtensionMember = isExtensionContainer && !hasStaticModifier;

        if (isExtensionMember)
            methodSymbol.MarkDeclaredInExtension();

        if (isAsync && methodDecl.ReturnType is null)
            methodSymbol.RequireAsyncReturnTypeInference();

        if (methodDecl.TypeParameterList is not null)
        {
            var typeParametersBuilder = ImmutableArray.CreateBuilder<ITypeParameterSymbol>(methodDecl.TypeParameterList.Parameters.Count);
            int ordinal = 0;
            foreach (var typeParameterSyntax in methodDecl.TypeParameterList.Parameters)
            {
                var (constraintKind, constraintTypeReferences) = AnalyzeTypeParameterConstraints(typeParameterSyntax);
                var variance = GetDeclaredVariance(typeParameterSyntax);

                var typeParameterSymbol = new SourceTypeParameterSymbol(
                    typeParameterSyntax.Identifier.ValueText,
                    methodSymbol,
                    _containingType,
                    CurrentNamespace!.AsSourceNamespace(),
                    [typeParameterSyntax.GetLocation()],
                    [typeParameterSyntax.GetReference()],
                    ordinal++,
                    constraintKind,
                    constraintTypeReferences,
                    variance);
                typeParametersBuilder.Add(typeParameterSymbol);
            }

            methodSymbol.SetTypeParameters(typeParametersBuilder);
        }

        var hasInvalidAsyncReturnType = false;

        var methodBinder = new MethodBinder(methodSymbol, this);
        methodBinder.EnsureTypeParameterConstraintTypesResolved(methodSymbol.TypeParameters);

        var returnType = methodDecl.ReturnType is null
            ? defaultReturnType
            : methodBinder.ResolveType(methodDecl.ReturnType.Type);

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
            var refKindForType = refKind == RefKind.None && typeSyntax is ByRefTypeSyntax ? RefKind.Ref : refKind;
            var resolvedType = refKindForType is RefKind.Ref or RefKind.Out or RefKind.In or RefKind.RefReadOnly or RefKind.RefReadOnlyParameter
                ? methodBinder.ResolveType(typeSyntax, refKindForType)
                : methodBinder.ResolveType(typeSyntax);
            resolvedParamInfos.Add((paramName, resolvedType, refKind, syntax, isMutable));
        }

        ITypeSymbol? receiverType = null;
        if (isExtensionMember)
            receiverType = GetExtensionReceiverType();

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

            if (candidate is null || !candidate.IsVirtual)
            {
                _diagnostics.ReportOverrideMemberNotFound(name, identifierToken.GetLocation());
                isOverride = false;
                isVirtual = false;
                isSealed = false;
            }
            else if (candidate.IsSealed)
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

        methodSymbol.UpdateModifiers(isVirtual, isOverride, isSealed);

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

    public MethodBinder BindOperatorDeclaration(OperatorDeclarationSyntax operatorDecl)
    {
        var operatorText = OperatorFacts.GetDisplayText(operatorDecl.OperatorToken.Kind);
        var defaultAccessibility = AccessibilityUtilities.GetDefaultMemberAccessibility(_containingType);
        var operatorAccessibility = AccessibilityUtilities.DetermineAccessibility(operatorDecl.Modifiers, defaultAccessibility);
        var hasStaticModifier = operatorDecl.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);
        var isExtensionContainer = IsExtensionContainer;

        if (isExtensionContainer)
            _diagnostics.ReportOperatorNotSupportedInExtensions(operatorText, operatorDecl.OperatorKeyword.GetLocation());

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

        var operatorBinder = new MethodBinder(operatorSymbol, this);

        var returnType = operatorDecl.ReturnType is null
            ? defaultReturnType
            : operatorBinder.ResolveType(operatorDecl.ReturnType.Type);

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

            var refKindForType = refKind == RefKind.None && typeSyntax is ByRefTypeSyntax ? RefKind.Ref : refKind;
            var pType = refKindForType is RefKind.Ref or RefKind.Out or RefKind.In or RefKind.RefReadOnly or RefKind.RefReadOnlyParameter
                ? operatorBinder.ResolveType(typeSyntax, refKindForType)
                : operatorBinder.ResolveType(typeSyntax);
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
        var isStatic = ctorDecl.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);
        var defaultAccessibility = AccessibilityUtilities.GetDefaultMemberAccessibility(_containingType);
        var ctorAccessibility = AccessibilityUtilities.DetermineAccessibility(ctorDecl.Modifiers, defaultAccessibility);
        if (isStatic)
            ctorAccessibility = Accessibility.Private;

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

            var refKindForType = refKind == RefKind.None && typeSyntax is ByRefTypeSyntax ? RefKind.Ref : refKind;
            var pType = refKindForType is RefKind.Ref or RefKind.Out or RefKind.In or RefKind.RefReadOnly or RefKind.RefReadOnlyParameter
                ? ResolveType(typeSyntax, refKindForType)
                : ResolveType(typeSyntax);
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
        var defaultAccessibility = AccessibilityUtilities.GetDefaultMemberAccessibility(_containingType);
        var ctorAccessibility = AccessibilityUtilities.DetermineAccessibility(ctorDecl.Modifiers, defaultAccessibility);

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

            var refKindForType = refKind == RefKind.None && typeSyntax is ByRefTypeSyntax ? RefKind.Ref : refKind;
            var pType = refKindForType is RefKind.Ref or RefKind.Out or RefKind.In or RefKind.RefReadOnly or RefKind.RefReadOnlyParameter
                ? ResolveType(typeSyntax, refKindForType)
                : ResolveType(typeSyntax);
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

    public Dictionary<SyntaxNode, Binder> BindPropertyDeclaration(PropertyDeclarationSyntax propertyDecl)
    {
        var propertyType = ResolveType(propertyDecl.Type.Type);
        var modifiers = propertyDecl.Modifiers;
        var hasStaticModifier = modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);
        var isStatic = hasStaticModifier;
        var isVirtual = modifiers.Any(m => m.Kind == SyntaxKind.VirtualKeyword);
        var isOverride = modifiers.Any(m => m.Kind == SyntaxKind.OverrideKeyword);
        var isSealed = modifiers.Any(m => m.Kind == SyntaxKind.SealedKeyword);
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

        if (isExtensionContainer)
        {
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
        }

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

        ValidateTypeAccessibility(
            propertyType,
            propertyAccessibility,
            "property",
            GetMemberDisplayName(propertyName),
            "property",
            propertyDecl.Type.Type.GetLocation());

        ITypeSymbol? receiverType = null;
        if (isExtensionMember)
            receiverType = GetExtensionReceiverType();

        if (receiverType is not null && _extensionReceiverTypeSyntax is not null)
        {
            ValidateTypeAccessibility(
                receiverType,
                propertyAccessibility,
                "property",
                GetMemberDisplayName(propertyName),
                "receiver",
                _extensionReceiverTypeSyntax.GetLocation());
        }

        var propertySymbol = new SourcePropertySymbol(
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

        if (isExtensionMember)
            propertySymbol.MarkDeclaredInExtension(receiverType);

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
                declaredAccessibility: Accessibility.Private);

            propertySymbol.SetBackingField(backingField);
        }

        var hasExpressionBody = propertyDecl.ExpressionBody is not null;
        var hasGetter = propertyDecl.AccessorList?.Accessors.Any(a => a.Kind == SyntaxKind.GetAccessorDeclaration) ?? hasExpressionBody;
        var hasSetter = propertyDecl.AccessorList?.Accessors.Any(a => a.Kind == SyntaxKind.SetAccessorDeclaration) ?? false;

        IMethodSymbol? overriddenGetter = null;
        IMethodSymbol? overriddenSetter = null;

        if (isOverride)
        {
            var candidate = FindPropertyOverrideCandidate(propertyName, propertyType, isStatic, isIndexer: false, parameters: Array.Empty<(ITypeSymbol type, RefKind refKind)>());
            bool overrideValid = true;

            if (candidate is null)
            {
                _diagnostics.ReportOverrideMemberNotFound(propertyName, identifierToken.GetLocation());
                overrideValid = false;
            }
            else
            {
                if (hasGetter)
                {
                    if (candidate.GetMethod is null || !candidate.GetMethod.IsVirtual)
                    {
                        _diagnostics.ReportOverrideMemberNotFound(propertyName, identifierToken.GetLocation());
                        overrideValid = false;
                    }
                    else if (candidate.GetMethod.IsSealed)
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
                        _diagnostics.ReportOverrideMemberNotFound(propertyName, identifierToken.GetLocation());
                        overrideValid = false;
                    }
                    else if (candidate.SetMethod.IsSealed)
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

        var binders = new Dictionary<SyntaxNode, Binder>();

        SourceMethodSymbol? getMethod = null;
        SourceMethodSymbol? setMethod = null;
        var explicitAccessorPrefix = explicitInterfaceMetadataName is not null
            ? explicitInterfaceMetadataName + "."
            : string.Empty;

        if (propertyDecl.AccessorList is not null)
        {
            foreach (var accessor in propertyDecl.AccessorList.Accessors)
            {
                bool isGet = accessor.Kind == SyntaxKind.GetAccessorDeclaration;
                var returnType = isGet ? propertyType : Compilation.GetSpecialType(SpecialType.System_Unit);
                var name = explicitAccessorPrefix + (isGet ? "get_" : "set_") + propertySymbol.Name;

                var accessorOverride = isOverride && (isGet ? overriddenGetter is not null : overriddenSetter is not null);
                var accessorVirtual = accessorOverride || isVirtual;
                var accessorSealed = accessorOverride && isSealed;

                var isAsync = accessor.Modifiers.Any(m => m.Kind == SyntaxKind.AsyncKeyword);

                var propertyTypeSyntax = propertyDecl.Type?.Type;
                var requiresAsyncReturnTypeDiagnostic = isGet && isAsync && propertyTypeSyntax is not null &&
                    (!IsValidAsyncReturnType(propertyType) || propertyType.TypeKind == TypeKind.Error);

                if (requiresAsyncReturnTypeDiagnostic)
                {
                    var display = propertyType.TypeKind == TypeKind.Error
                        ? propertyTypeSyntax.ToString()
                        : propertyType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
                    _diagnostics.ReportAsyncReturnTypeMustBeTaskLike(display, propertyTypeSyntax.GetLocation());
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
                    methodKind: isGet ? MethodKind.PropertyGet : MethodKind.PropertySet,
                    isAsync: isAsync,
                    isVirtual: accessorVirtual,
                    isOverride: accessorOverride,
                    isSealed: accessorSealed,
                    declaredAccessibility: propertyAccessibility);

                if (isExtensionMember)
                    methodSymbol.MarkDeclaredInExtension();

                var parameters = new List<SourceParameterSymbol>();
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

                var binder = new MethodBinder(methodSymbol, this);
                binders[accessor] = binder;

                if (isGet)
                    getMethod = methodSymbol;
                else
                    setMethod = methodSymbol;
            }
        }
        else if (propertyDecl.ExpressionBody is not null)
        {
            var name = explicitAccessorPrefix + "get_" + propertySymbol.Name;
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
                declaredAccessibility: propertyAccessibility);

            if (isExtensionMember)
                methodSymbol.MarkDeclaredInExtension();

            var parameters = new List<SourceParameterSymbol>();
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

            var binder = new MethodBinder(methodSymbol, this);
            var expressionBodyBinder = new MethodBodyBinder(methodSymbol, binder);

            binders[propertyDecl.ExpressionBody!] = expressionBodyBinder;

            getMethod = methodSymbol;
        }

        propertySymbol.SetAccessors(getMethod, setMethod);

        return binders;
    }

    public Dictionary<AccessorDeclarationSyntax, MethodBinder> BindIndexerDeclaration(IndexerDeclarationSyntax indexerDecl)
    {
        var propertyType = ResolveType(indexerDecl.Type.Type);
        var modifiers = indexerDecl.Modifiers;
        var isStatic = modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);
        var isVirtual = modifiers.Any(m => m.Kind == SyntaxKind.VirtualKeyword);
        var isOverride = modifiers.Any(m => m.Kind == SyntaxKind.OverrideKeyword);
        var isSealed = modifiers.Any(m => m.Kind == SyntaxKind.SealedKeyword);
        var defaultAccessibility = AccessibilityUtilities.GetDefaultMemberAccessibility(_containingType);
        var indexerAccessibility = AccessibilityUtilities.DetermineAccessibility(modifiers, defaultAccessibility);
        var explicitInterfaceSpecifier = indexerDecl.ExplicitInterfaceSpecifier;
        var identifierToken = ResolveExplicitInterfaceIdentifier(indexerDecl.Identifier, explicitInterfaceSpecifier);

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

            var refKindForType = refKind == RefKind.None && isByRefSyntax ? RefKind.Ref : refKind;
            var type = refKindForType is RefKind.Ref or RefKind.Out or RefKind.In or RefKind.RefReadOnly or RefKind.RefReadOnlyParameter
                ? ResolveType(typeSyntax, refKindForType)
                : ResolveType(typeSyntax);

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
            var resolved = ResolveType(explicitInterfaceSpecifier.Name);
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
        }

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
                _diagnostics.ReportOverrideMemberNotFound("Item", identifierToken.GetLocation());
                overrideValid = false;
            }
            else
            {
                if (hasGetter)
                {
                    if (candidate.GetMethod is null || !candidate.GetMethod.IsVirtual)
                    {
                        _diagnostics.ReportOverrideMemberNotFound("Item", identifierToken.GetLocation());
                        overrideValid = false;
                    }
                    else if (candidate.GetMethod.IsSealed)
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
                        _diagnostics.ReportOverrideMemberNotFound("Item", identifierToken.GetLocation());
                        overrideValid = false;
                    }
                    else if (candidate.SetMethod.IsSealed)
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

        SourceMethodSymbol? getMethod = null;
        SourceMethodSymbol? setMethod = null;
        var explicitIndexerAccessorPrefix = explicitInterfaceMetadataName is not null
            ? explicitInterfaceMetadataName + "."
            : string.Empty;

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

    private static (TypeParameterConstraintKind constraintKind, ImmutableArray<SyntaxReference> constraintTypeReferences) AnalyzeTypeParameterConstraints(TypeParameterSyntax parameter)
    {
        var constraints = parameter.Constraints;
        if (constraints.Count == 0)
            return (TypeParameterConstraintKind.None, ImmutableArray<SyntaxReference>.Empty);

        var constraintKind = TypeParameterConstraintKind.None;
        var typeConstraintReferences = ImmutableArray.CreateBuilder<SyntaxReference>();

        foreach (var constraint in constraints)
        {
            switch (constraint)
            {
                case ClassConstraintSyntax:
                    constraintKind |= TypeParameterConstraintKind.ReferenceType;
                    break;
                case StructConstraintSyntax:
                    constraintKind |= TypeParameterConstraintKind.ValueType;
                    break;
                case TypeConstraintSyntax typeConstraint:
                    constraintKind |= TypeParameterConstraintKind.TypeConstraint;
                    typeConstraintReferences.Add(typeConstraint.GetReference());
                    break;
            }
        }

        return (constraintKind, typeConstraintReferences.ToImmutable());
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
}
