using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

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
            ConversionOperatorDeclarationSyntax conversionDecl => BindConversionOperatorSymbol(conversionDecl),
            PropertyDeclarationSyntax property => BindPropertySymbol(property),
            EventDeclarationSyntax @event => BindEventSymbol(@event),
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
                [decl.GetReference(), fieldDecl.GetReference()],
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
        var isSealed = modifiers.Any(m => m.Kind is SyntaxKind.SealedKeyword or SyntaxKind.FinalKeyword);
        var isAbstract = modifiers.Any(m => m.Kind == SyntaxKind.AbstractKeyword);
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
            declaredAccessibility: methodAccessibility);

        var isExtensionMember = isExtensionContainer && !hasStaticModifier;

        if (isExtensionMember)
            methodSymbol.MarkDeclaredInExtension();

        if (isAsync && methodDecl.ReturnType is null)
            methodSymbol.RequireAsyncReturnTypeInference();

        InitializeMethodTypeParameters(methodSymbol, methodDecl.TypeParameterList);

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
        if (isExtensionMember && _extensionReceiverTypeSyntax is not null)
            receiverType = methodBinder.ResolveType(_extensionReceiverTypeSyntax);

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

        methodSymbol.UpdateModifiers(isVirtual, isOverride, isSealed, isAbstract);

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

    public MethodBinder BindConversionOperatorDeclaration(ConversionOperatorDeclarationSyntax conversionDecl)
    {
        var operatorText = OperatorFacts.GetConversionOperatorDisplayText(conversionDecl.ConversionKindKeyword.Kind);
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
            : conversionBinder.ResolveType(conversionDecl.ReturnType.Type);

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

            var refKindForType = refKind == RefKind.None && typeSyntax is ByRefTypeSyntax ? RefKind.Ref : refKind;
            var pType = refKindForType is RefKind.Ref or RefKind.Out or RefKind.In or RefKind.RefReadOnly or RefKind.RefReadOnlyParameter
                ? conversionBinder.ResolveType(typeSyntax, refKindForType)
                : conversionBinder.ResolveType(typeSyntax);
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
        var isAbstract = modifiers.Any(m => m.Kind == SyntaxKind.AbstractKeyword);
        var isVirtual = modifiers.Any(m => m.Kind == SyntaxKind.VirtualKeyword);
        var isOverride = modifiers.Any(m => m.Kind == SyntaxKind.OverrideKeyword);
        var isSealed = modifiers.Any(m => m.Kind is SyntaxKind.SealedKeyword or SyntaxKind.FinalKeyword);
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
                        _diagnostics.ReportOverrideMemberNotFound(propertyName, "property", identifierToken.GetLocation());
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

        if (isAbstract && propertyDecl.ExpressionBody is not null)
        {
            _diagnostics.ReportAbstractMemberCannotHaveBody(propertyName, identifierToken.GetLocation());
        }

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
                    methodKind: isGet ? MethodKind.PropertyGet : MethodKind.PropertySet,
                    isAsync: isAsync,
                    isVirtual: accessorVirtual,
                    isOverride: accessorOverride,
                    isSealed: accessorSealed,
                    isAbstract: isAbstract,
                    declaredAccessibility: propertyAccessibility);

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
                    receiverTypeForAccessor = binder.ResolveType(_extensionReceiverTypeSyntax);
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

                binder ??= new MethodBinder(methodSymbol, this);
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
                isAbstract: isAbstract,
                declaredAccessibility: propertyAccessibility);

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
                receiverTypeForAccessor = binder.ResolveType(_extensionReceiverTypeSyntax);
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

            binder ??= new MethodBinder(methodSymbol, this);
            var expressionBodyBinder = new MethodBodyBinder(methodSymbol, binder);

            binders[propertyDecl.ExpressionBody!] = expressionBodyBinder;

            getMethod = methodSymbol;
        }

        propertySymbol.SetAccessors(getMethod, setMethod);

        return binders;
    }

    public Dictionary<AccessorDeclarationSyntax, MethodBinder> BindEventDeclaration(EventDeclarationSyntax eventDecl)
    {
        var eventType = ResolveType(eventDecl.Type.Type);
        var modifiers = eventDecl.Modifiers;
        var hasStaticModifier = modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);
        var isStatic = hasStaticModifier;
        var isAbstract = modifiers.Any(m => m.Kind == SyntaxKind.AbstractKeyword);
        var isVirtual = modifiers.Any(m => m.Kind == SyntaxKind.VirtualKeyword);
        var isOverride = modifiers.Any(m => m.Kind == SyntaxKind.OverrideKeyword);
        var isSealed = modifiers.Any(m => m.Kind is SyntaxKind.SealedKeyword or SyntaxKind.FinalKeyword);
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
            var resolved = ResolveType(explicitInterfaceSpecifier.Name);
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
                else if (candidate.AddMethod.IsSealed)
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
                    else if (candidate.RemoveMethod.IsSealed)
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
                    receiverTypeForAccessor = binder.ResolveType(_extensionReceiverTypeSyntax);
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
        var propertyType = ResolveType(indexerDecl.Type.Type);
        var modifiers = indexerDecl.Modifiers;
        var hasStaticModifier = modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);
        var isStatic = hasStaticModifier; var isAbstract = modifiers.Any(m => m.Kind == SyntaxKind.AbstractKeyword);
        var isVirtual = modifiers.Any(m => m.Kind == SyntaxKind.VirtualKeyword);
        var isOverride = modifiers.Any(m => m.Kind == SyntaxKind.OverrideKeyword);
        var isSealed = modifiers.Any(m => m.Kind is SyntaxKind.SealedKeyword or SyntaxKind.FinalKeyword);
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
            isAbstract = false;
        }

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
                        _diagnostics.ReportOverrideMemberNotFound("Item", "indexer", identifierToken.GetLocation());
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
                receiverTypeForAccessor = binder.ResolveType(_extensionReceiverTypeSyntax);
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
