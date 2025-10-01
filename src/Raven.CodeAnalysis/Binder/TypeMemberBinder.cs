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

    public TypeMemberBinder(Binder parent, INamedTypeSymbol containingType)
        : base(parent, parent.Diagnostics)
    {
        _containingType = containingType;
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

    public override ISymbol? BindDeclaredSymbol(SyntaxNode node)
    {
        return node switch
        {
            MethodDeclarationSyntax method => BindMethodSymbol(method),
            ConstructorDeclarationSyntax ctor => BindConstructorSymbol(ctor),
            NamedConstructorDeclarationSyntax namedCtor => BindConstructorSymbol(namedCtor),
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
        var isStatic = fieldDecl.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);
        var fieldAccessibility = AccessibilityUtilities.DetermineAccessibility(
            fieldDecl.Modifiers,
            AccessibilityUtilities.GetDefaultMemberAccessibility(_containingType));

        foreach (var decl in fieldDecl.Declaration.Declarators)
        {
            var fieldType = decl.TypeAnnotation is null
                ? Compilation.GetSpecialType(SpecialType.System_Object)
                : ResolveType(decl.TypeAnnotation.Type);

            BoundExpression? initializer = null;
            if (decl.Initializer is not null)
            {
                var exprBinder = new BlockBinder(_containingType, this);
                initializer = exprBinder.BindExpression(decl.Initializer.Value);

                foreach (var diag in exprBinder.Diagnostics.AsEnumerable())
                    _diagnostics.Report(diag);
            }

            _ = new SourceFieldSymbol(
                decl.Identifier.ValueText,
                fieldType,
                isStatic: isStatic,
                isLiteral: false,
                constantValue: null,
                _containingType,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [decl.GetLocation()],
                [decl.GetReference()],
                initializer,
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

        var paramInfos = new List<(string name, TypeSyntax typeSyntax, RefKind refKind, ParameterSyntax syntax)>();
        foreach (var p in methodDecl.ParameterList.Parameters)
        {
            var typeSyntax = p.TypeAnnotation!.Type;
            var refKind = RefKind.None;
            if (typeSyntax is ByRefTypeSyntax)
                refKind = p.Modifiers.Any(m => m.Kind == SyntaxKind.OutKeyword) ? RefKind.Out : RefKind.Ref;

            paramInfos.Add((p.Identifier.ValueText, typeSyntax, refKind, p));
        }

        var modifiers = methodDecl.Modifiers;
        var isStatic = modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);
        var isVirtual = modifiers.Any(m => m.Kind == SyntaxKind.VirtualKeyword);
        var isOverride = modifiers.Any(m => m.Kind == SyntaxKind.OverrideKeyword);
        var isSealed = modifiers.Any(m => m.Kind == SyntaxKind.SealedKeyword);
        var defaultAccessibility = AccessibilityUtilities.GetDefaultMemberAccessibility(_containingType);
        var methodAccessibility = AccessibilityUtilities.DetermineAccessibility(modifiers, defaultAccessibility);

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

        var methodSymbol = new SourceMethodSymbol(
            metadataName,
            Compilation.GetSpecialType(SpecialType.System_Unit),
            ImmutableArray<SourceParameterSymbol>.Empty,
            _containingType,
            _containingType,
            CurrentNamespace!.AsSourceNamespace(),
            [methodDecl.GetLocation()],
            [methodDecl.GetReference()],
            isStatic: isStatic,
            methodKind: methodKind,
            isVirtual: isVirtual,
            isOverride: isOverride,
            isSealed: isSealed,
            declaredAccessibility: methodAccessibility);

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

        var methodBinder = new MethodBinder(methodSymbol, this);
        methodBinder.EnsureTypeParameterConstraintTypesResolved(methodSymbol.TypeParameters);

        var returnType = methodDecl.ReturnType is null
            ? Compilation.GetSpecialType(SpecialType.System_Unit)
            : methodBinder.ResolveType(methodDecl.ReturnType.Type);

        var resolvedParamInfos = new List<(string name, ITypeSymbol type, RefKind refKind, ParameterSyntax syntax)>();
        foreach (var (paramName, typeSyntax, refKind, syntax) in paramInfos)
        {
            var refKindForType = refKind == RefKind.None && typeSyntax is ByRefTypeSyntax ? RefKind.Ref : refKind;
            var resolvedType = refKindForType is RefKind.Ref or RefKind.Out or RefKind.In or RefKind.RefReadOnly or RefKind.RefReadOnlyParameter
                ? methodBinder.ResolveType(typeSyntax, refKindForType)
                : methodBinder.ResolveType(typeSyntax);
            resolvedParamInfos.Add((paramName, resolvedType, refKind, syntax));
        }

        if (explicitInterfaceType is not null)
        {
            explicitInterfaceMember = FindExplicitInterfaceImplementation(
                explicitInterfaceType,
                name,
                returnType,
                resolvedParamInfos.Select(p => (p.type, p.refKind)).ToArray());

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
            var candidate = FindOverrideCandidate(name, resolvedParamInfos.Select(p => (p.type, p.refKind)).ToArray());

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

        CheckForDuplicateSignature(metadataName, displayName, resolvedParamInfos.Select(p => (p.type, p.refKind)).ToArray(), identifierToken.GetLocation(), methodDecl);

        if (overriddenMethod is not null)
            methodSymbol.SetOverriddenMethod(overriddenMethod);

        if (explicitInterfaceMember is not null)
            methodSymbol.SetExplicitInterfaceImplementations(ImmutableArray.Create(explicitInterfaceMember));

        var parameters = new List<SourceParameterSymbol>();
        foreach (var (paramName, paramType, refKind, syntax) in resolvedParamInfos)
        {
            var hasDefaultValue = TryEvaluateParameterDefaultValue(syntax, paramType, out var defaultValue);
            var pSymbol = new SourceParameterSymbol(
                paramName,
                paramType,
                methodSymbol,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [syntax.GetLocation()],
                [syntax.GetReference()],
                refKind,
                hasDefaultValue,
                defaultValue
            );
            parameters.Add(pSymbol);
        }

        methodSymbol.SetReturnType(returnType);
        methodSymbol.SetParameters(parameters);
        return methodBinder;
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

        var paramInfos = new List<(string name, ITypeSymbol type, RefKind refKind, ParameterSyntax syntax)>();
        foreach (var p in ctorDecl.ParameterList.Parameters)
        {
            var typeSyntax = p.TypeAnnotation!.Type;
            var refKind = RefKind.None;
            if (typeSyntax is ByRefTypeSyntax)
                refKind = p.Modifiers.Any(m => m.Kind == SyntaxKind.OutKeyword) ? RefKind.Out : RefKind.Ref;

            var refKindForType = refKind == RefKind.None && typeSyntax is ByRefTypeSyntax ? RefKind.Ref : refKind;
            var pType = refKindForType is RefKind.Ref or RefKind.Out or RefKind.In or RefKind.RefReadOnly or RefKind.RefReadOnlyParameter
                ? ResolveType(typeSyntax, refKindForType)
                : ResolveType(typeSyntax);
            paramInfos.Add((p.Identifier.ValueText, pType, refKind, p));
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
        foreach (var (paramName, paramType, refKind, syntax) in paramInfos)
        {
            var hasDefaultValue = TryEvaluateParameterDefaultValue(syntax, paramType, out var defaultValue);
            var pSymbol = new SourceParameterSymbol(
                paramName,
                paramType,
                ctorSymbol,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [syntax.GetLocation()],
                [syntax.GetReference()],
                refKind,
                hasDefaultValue,
                defaultValue
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

        var paramInfos = new List<(string name, ITypeSymbol type, RefKind refKind, ParameterSyntax syntax)>();
        foreach (var p in ctorDecl.ParameterList.Parameters)
        {
            var typeSyntax = p.TypeAnnotation!.Type;
            var refKind = RefKind.None;
            if (typeSyntax is ByRefTypeSyntax)
                refKind = p.Modifiers.Any(m => m.Kind == SyntaxKind.OutKeyword) ? RefKind.Out : RefKind.Ref;

            var refKindForType = refKind == RefKind.None && typeSyntax is ByRefTypeSyntax ? RefKind.Ref : refKind;
            var pType = refKindForType is RefKind.Ref or RefKind.Out or RefKind.In or RefKind.RefReadOnly or RefKind.RefReadOnlyParameter
                ? ResolveType(typeSyntax, refKindForType)
                : ResolveType(typeSyntax);
            paramInfos.Add((p.Identifier.ValueText, pType, refKind, p));
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
        foreach (var (paramName, paramType, refKind, syntax) in paramInfos)
        {
            var hasDefaultValue = TryEvaluateParameterDefaultValue(syntax, paramType, out var defaultValue);
            var pSymbol = new SourceParameterSymbol(
                paramName,
                paramType,
                ctorSymbol,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [syntax.GetLocation()],
                [syntax.GetReference()],
                refKind,
                hasDefaultValue,
                defaultValue
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

    public Dictionary<AccessorDeclarationSyntax, MethodBinder> BindPropertyDeclaration(PropertyDeclarationSyntax propertyDecl)
    {
        var propertyType = ResolveType(propertyDecl.Type.Type);
        var modifiers = propertyDecl.Modifiers;
        var isStatic = modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);
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

        if (_containingType.TypeKind != TypeKind.Interface &&
            propertyDecl.AccessorList is { } accessorList &&
            accessorList.Accessors.All(a => a.Body is null && a.ExpressionBody is null))
        {
            var backingField = new SourceFieldSymbol(
                $"<{propertySymbol.Name}>k__BackingField",
                propertyType,
                isStatic: isStatic,
                isLiteral: false,
                constantValue: null,
                _containingType,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [propertyDecl.GetLocation()],
                [propertyDecl.GetReference()],
                declaredAccessibility: Accessibility.Private);

            propertySymbol.SetBackingField(backingField);
        }

        var hasGetter = propertyDecl.AccessorList?.Accessors.Any(a => a.Kind == SyntaxKind.GetAccessorDeclaration) ?? false;
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

        var binders = new Dictionary<AccessorDeclarationSyntax, MethodBinder>();

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
                    isVirtual: accessorVirtual,
                    isOverride: accessorOverride,
                    isSealed: accessorSealed,
                    declaredAccessibility: propertyAccessibility);

                var parameters = new List<SourceParameterSymbol>();
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

        var indexerParameters = indexerDecl.ParameterList.Parameters
            .Select(p =>
            {
                var typeSyntax = p.TypeAnnotation!.Type;
                var refKind = RefKind.None;
                var isByRefSyntax = typeSyntax is ByRefTypeSyntax;
                if (isByRefSyntax)
                    refKind = p.Modifiers.Any(m => m.Kind == SyntaxKind.OutKeyword) ? RefKind.Out : RefKind.Ref;

                var refKindForType = refKind == RefKind.None && isByRefSyntax ? RefKind.Ref : refKind;
                var type = refKindForType is RefKind.Ref or RefKind.Out or RefKind.In or RefKind.RefReadOnly or RefKind.RefReadOnlyParameter
                    ? ResolveType(typeSyntax, refKindForType)
                    : ResolveType(typeSyntax);
                var hasDefaultValue = TryEvaluateParameterDefaultValue(p, type, out var defaultValue);
                return new { Syntax = p, Type = type, RefKind = refKind, HasDefaultValue = hasDefaultValue, DefaultValue = defaultValue };
            })
            .ToArray();

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
                        param.DefaultValue));
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

    internal static bool TryEvaluateParameterDefaultValue(
        ParameterSyntax parameterSyntax,
        ITypeSymbol parameterType,
        out object? defaultValue)
    {
        defaultValue = null;

        if (parameterSyntax.DefaultValue is null)
            return false;

        if (!TryEvaluateDefaultExpression(parameterSyntax.DefaultValue.Value, out var rawValue))
            return false;

        return TryConvertParameterDefault(parameterType, rawValue, out defaultValue);
    }

    private static bool TryEvaluateDefaultExpression(ExpressionSyntax expression, out object? value)
    {
        switch (expression)
        {
            case LiteralExpressionSyntax literal:
                return TryGetLiteralValue(literal, out value);
            case UnaryExpressionSyntax unary when unary.Kind == SyntaxKind.UnaryMinusExpression:
                if (TryEvaluateDefaultExpression(unary.Expression, out var operand) && TryNegate(operand, out var negated))
                {
                    value = negated;
                    return true;
                }
                break;
            case UnaryExpressionSyntax unary when unary.Kind == SyntaxKind.UnaryPlusExpression:
                return TryEvaluateDefaultExpression(unary.Expression, out value);
            case ParenthesizedExpressionSyntax parenthesized:
                return TryEvaluateDefaultExpression(parenthesized.Expression, out value);
        }

        value = null;
        return false;
    }

    private static bool TryGetLiteralValue(LiteralExpressionSyntax literal, out object? value)
    {
        if (literal.Kind == SyntaxKind.NullLiteralExpression)
        {
            value = null;
            return true;
        }

        value = literal.Token.Value;
        if (value is not null)
            return true;

        if (literal.Kind == SyntaxKind.StringLiteralExpression)
        {
            value = literal.Token.ValueText;
            return true;
        }

        return false;
    }

    private static bool TryNegate(object? operand, out object? value)
    {
        switch (operand)
        {
            case int i:
                value = -i;
                return true;
            case long l:
                value = -l;
                return true;
            case float f:
                value = -f;
                return true;
            case double d:
                value = -d;
                return true;
        }

        value = null;
        return false;
    }

    private static bool TryConvertParameterDefault(ITypeSymbol parameterType, object? value, out object? converted)
    {
        if (parameterType is NullableTypeSymbol nullable)
        {
            if (value is null)
            {
                converted = null;
                return true;
            }

            parameterType = nullable.UnderlyingType;
        }

        if (value is null)
        {
            if (!parameterType.IsValueType || parameterType.TypeKind == TypeKind.Nullable)
            {
                converted = null;
                return true;
            }

            converted = null;
            return false;
        }

        if (parameterType.SpecialType == SpecialType.System_Object)
        {
            converted = value;
            return true;
        }

        switch (parameterType.SpecialType)
        {
            case SpecialType.System_Boolean when value is bool boolValue:
                converted = boolValue;
                return true;
            case SpecialType.System_String when value is string stringValue:
                converted = stringValue;
                return true;
            case SpecialType.System_Char when value is char charValue:
                converted = charValue;
                return true;
            case SpecialType.System_Int32:
                if (value is int intValue)
                {
                    converted = intValue;
                    return true;
                }
                break;
            case SpecialType.System_Int64:
                if (value is long longValue)
                {
                    converted = longValue;
                    return true;
                }
                if (value is int intToLong)
                {
                    converted = (long)intToLong;
                    return true;
                }
                break;
            case SpecialType.System_Single:
                if (value is float floatValue)
                {
                    converted = floatValue;
                    return true;
                }
                if (value is int intToFloat)
                {
                    converted = (float)intToFloat;
                    return true;
                }
                if (value is long longToFloat)
                {
                    converted = (float)longToFloat;
                    return true;
                }
                if (value is double doubleToFloat)
                {
                    converted = (float)doubleToFloat;
                    return true;
                }
                break;
            case SpecialType.System_Double:
                if (value is double doubleValue)
                {
                    converted = doubleValue;
                    return true;
                }
                if (value is int intToDouble)
                {
                    converted = (double)intToDouble;
                    return true;
                }
                if (value is long longToDouble)
                {
                    converted = (double)longToDouble;
                    return true;
                }
                if (value is float floatToDouble)
                {
                    converted = (double)floatToDouble;
                    return true;
                }
                break;
        }

        converted = null;
        return false;
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

