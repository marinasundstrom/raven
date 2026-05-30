using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class MemberSignatureDeclarationPass
{
    public static void DeclareMethodSignature(
        SemanticModel semanticModel,
        MethodDeclarationSyntax methodDeclaration)
    {
        var compilation = semanticModel.Compilation;

        if (compilation.TryGetMethodSymbol(methodDeclaration, out _))
            return;

        if (!TryGetContainingDeclaredType(compilation, methodDeclaration, out var containingType))
            return;

        // Explicit implementations need the fully resolved interface metadata
        // name, so leave them to the full member binder for now.
        if (methodDeclaration.ExplicitInterfaceSpecifier is not null)
            return;

        var isAsync = methodDeclaration.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.AsyncKeyword);
        var isStatic = methodDeclaration.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.StaticKeyword);
        var isExtern = methodDeclaration.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.ExternKeyword);
        var isVirtual = methodDeclaration.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.VirtualKeyword);
        var isOverride = methodDeclaration.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.OverrideKeyword);
        var isSealed = methodDeclaration.Modifiers.Any(static modifier => modifier.Kind is SyntaxKind.SealedKeyword or SyntaxKind.FinalKeyword);
        var isAbstract = methodDeclaration.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.AbstractKeyword);
        var defaultAccessibility = compilation.Options.MembersPublicByDefault
            ? Accessibility.Public
            : AccessibilityUtilities.GetDefaultMemberAccessibility(containingType);
        var methodAccessibility = AccessibilityUtilities.DetermineAccessibility(
            methodDeclaration.Modifiers,
            defaultAccessibility);
        var methodKind = methodDeclaration.ExplicitInterfaceSpecifier is not null
            ? MethodKind.ExplicitInterfaceImplementation
            : MethodKind.Ordinary;

        if (containingType is SourceNamedTypeSymbol { IsExtensionDeclaration: true } &&
            !methodDeclaration.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.StaticKeyword))
        {
            isStatic = true;
            isVirtual = false;
            isOverride = false;
            isSealed = false;
            isAbstract = false;
            methodAccessibility = methodDeclaration.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.InternalKeyword)
                ? Accessibility.Internal
                : Accessibility.Public;
        }

        if (methodKind == MethodKind.ExplicitInterfaceImplementation)
        {
            isStatic = false;
            isVirtual = false;
            isOverride = false;
            isSealed = false;
            isAbstract = false;
            methodAccessibility = Accessibility.Private;
        }

        var defaultReturnType = isAsync
            ? compilation.GetSpecialType(SpecialType.System_Threading_Tasks_Task)
            : compilation.GetSpecialType(SpecialType.System_Unit);

        var methodName = methodDeclaration.Identifier.Kind == SyntaxKind.SelfKeyword
            ? "Invoke"
            : methodDeclaration.Identifier.ValueText;

        var methodSymbol = new SourceMethodSymbol(
            methodName,
            defaultReturnType,
            ImmutableArray<SourceParameterSymbol>.Empty,
            containingType,
            containingType,
            containingType.ContainingNamespace,
            [methodDeclaration.GetLocation()],
            [methodDeclaration.GetReference()],
            isStatic: isStatic,
            methodKind: methodKind,
            isAsync: isAsync,
            isVirtual: isVirtual,
            isOverride: isOverride,
            isSealed: isSealed,
            isAbstract: isAbstract,
            isExtern: isExtern,
            declaredAccessibility: methodAccessibility);

        var isExtensionMember = containingType is SourceNamedTypeSymbol { IsExtensionDeclaration: true } &&
                                !methodDeclaration.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.StaticKeyword);

        if (containingType is SourceNamedTypeSymbol { IsExtensionDeclaration: true })
            InitializeExtensionMethodTypeParameters(methodSymbol, containingType, methodDeclaration);
        else
            TypeParameterInitializer.InitializeMethodTypeParameters(
                methodSymbol,
                containingType,
                methodDeclaration.TypeParameterList,
                methodDeclaration.ConstraintClauses,
                methodDeclaration.SyntaxTree);

        if (methodDeclaration.ReturnType is { } returnTypeSyntax)
        {
            methodSymbol.SetReturnType(ResolveSkeletonType(
                semanticModel,
                returnTypeSyntax.Type,
                defaultReturnType,
                containingType,
                methodSymbol.TypeParameters));
        }

        if (isExtensionMember)
            methodSymbol.MarkDeclaredInExtension();

        var parameters = ImmutableArray.CreateBuilder<SourceParameterSymbol>();

        if (isExtensionMember &&
            methodDeclaration.Parent is ExtensionDeclarationSyntax extensionDeclaration)
        {
            var receiverType = ResolveSkeletonType(
                semanticModel,
                extensionDeclaration.ReceiverType,
                compilation.ErrorTypeSymbol,
                containingType,
                methodSymbol.TypeParameters);

            parameters.Add(new SourceParameterSymbol(
                "self",
                receiverType,
                methodSymbol,
                containingType,
                containingType.ContainingNamespace,
                [extensionDeclaration.ReceiverType.GetLocation()],
                [extensionDeclaration.ReceiverType.GetReference()]));
        }

        if (methodDeclaration.ParameterList is not null)
        {
            foreach (var parameter in methodDeclaration.ParameterList.Parameters)
                parameters.Add(CreateSkeletonParameterSymbol(semanticModel, parameter, methodSymbol, containingType));
        }

        methodSymbol.SetParameters(parameters.ToImmutable());
        if (methodDeclaration.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.PartialKeyword))
        {
            if (methodDeclaration.Body is not null || methodDeclaration.ExpressionBody is not null)
                methodSymbol.MarkAsPartialImplementation();
            else
                methodSymbol.MarkAsPartialDefinition();
        }

        methodSymbol.MarkSignatureSkeleton();
        compilation.RegisterMethodSymbol(methodDeclaration, methodSymbol);
    }

    public static void DeclareConstructorSignature(
        SemanticModel semanticModel,
        ConstructorDeclarationSyntax constructorDeclaration)
    {
        var compilation = semanticModel.Compilation;

        if (compilation.TryGetMethodSymbol(constructorDeclaration, out _))
            return;

        if (!TryGetContainingDeclaredType(compilation, constructorDeclaration, out var containingType))
            return;

        var isStatic = constructorDeclaration.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.StaticKeyword);
        var defaultAccessibility = compilation.Options.MembersPublicByDefault
            ? Accessibility.Public
            : AccessibilityUtilities.GetDefaultMemberAccessibility(containingType);
        var constructorAccessibility = isStatic
            ? Accessibility.Private
            : AccessibilityUtilities.DetermineAccessibility(constructorDeclaration.Modifiers, defaultAccessibility);
        var constructorMetadataName = isStatic ? ".cctor" : ".ctor";
        var constructorKind = isStatic ? MethodKind.StaticConstructor : MethodKind.Constructor;

        var constructorSymbol = new SourceMethodSymbol(
            constructorMetadataName,
            compilation.GetSpecialType(SpecialType.System_Unit),
            ImmutableArray<SourceParameterSymbol>.Empty,
            containingType,
            containingType,
            containingType.ContainingNamespace,
            [constructorDeclaration.GetLocation()],
            [constructorDeclaration.GetReference()],
            isStatic: isStatic,
            methodKind: constructorKind,
            declaredAccessibility: constructorAccessibility);

        var parameters = ImmutableArray.CreateBuilder<SourceParameterSymbol>();
        foreach (var parameter in constructorDeclaration.ParameterList.Parameters)
            parameters.Add(CreateSkeletonParameterSymbol(semanticModel, parameter, constructorSymbol, containingType));

        constructorSymbol.SetParameters(parameters.ToImmutable());
        constructorSymbol.MarkSignatureSkeleton();
        compilation.RegisterMethodSymbol(constructorDeclaration, constructorSymbol);
    }

    public static void DeclareParameterlessConstructorSignature(
        SemanticModel semanticModel,
        ParameterlessConstructorDeclarationSyntax constructorDeclaration)
    {
        var compilation = semanticModel.Compilation;

        if (compilation.TryGetMethodSymbol(constructorDeclaration, out _))
            return;

        if (!TryGetContainingDeclaredType(compilation, constructorDeclaration, out var containingType))
            return;

        var isStatic = constructorDeclaration.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.StaticKeyword);
        var defaultAccessibility = compilation.Options.MembersPublicByDefault
            ? Accessibility.Public
            : AccessibilityUtilities.GetDefaultMemberAccessibility(containingType);
        var constructorAccessibility = isStatic
            ? Accessibility.Private
            : AccessibilityUtilities.DetermineAccessibility(constructorDeclaration.Modifiers, defaultAccessibility);
        var constructorKind = isStatic ? MethodKind.StaticConstructor : MethodKind.Constructor;

        var constructorSymbol = new SourceMethodSymbol(
            isStatic ? ".cctor" : ".ctor",
            compilation.GetSpecialType(SpecialType.System_Unit),
            ImmutableArray<SourceParameterSymbol>.Empty,
            containingType,
            containingType,
            containingType.ContainingNamespace,
            [constructorDeclaration.GetLocation()],
            [constructorDeclaration.GetReference()],
            isStatic: isStatic,
            methodKind: constructorKind,
            declaredAccessibility: constructorAccessibility);

        constructorSymbol.MarkSignatureSkeleton();
        compilation.RegisterMethodSymbol(constructorDeclaration, constructorSymbol);
    }

    private static void InitializeExtensionMethodTypeParameters(
        SourceMethodSymbol methodSymbol,
        SourceNamedTypeSymbol containingType,
        MethodDeclarationSyntax methodDeclaration)
    {
        var builder = ImmutableArray.CreateBuilder<ITypeParameterSymbol>();
        var ordinal = 0;

        foreach (var typeParameter in containingType.TypeParameters.OfType<SourceTypeParameterSymbol>())
        {
            builder.Add(new SourceTypeParameterSymbol(
                typeParameter.Name,
                methodSymbol,
                containingType,
                methodSymbol.ContainingNamespace,
                typeParameter.Locations.ToArray(),
                typeParameter.DeclaringSyntaxReferences.ToArray(),
                ordinal++,
                typeParameter.ConstraintKind,
                typeParameter.ConstraintTypeReferences,
                typeParameter.Variance));
        }

        foreach (var typeParameter in CreateMethodTypeParameters(
                     methodSymbol,
                     containingType,
                     methodDeclaration.TypeParameterList,
                     methodDeclaration.ConstraintClauses,
                     methodDeclaration.SyntaxTree,
                     ordinal))
        {
            builder.Add(typeParameter);
        }

        methodSymbol.SetTypeParameters(builder);
    }

    private static IEnumerable<ITypeParameterSymbol> CreateMethodTypeParameters(
        SourceMethodSymbol methodSymbol,
        INamedTypeSymbol containingType,
        TypeParameterListSyntax? typeParameterList,
        SyntaxList<TypeParameterConstraintClauseSyntax> constraintClauses,
        SyntaxTree syntaxTree,
        int ordinal)
    {
        if (typeParameterList is null || typeParameterList.Parameters.Count == 0)
            yield break;

        Dictionary<string, List<TypeParameterConstraintClauseSyntax>>? clausesByName = null;
        if (constraintClauses.Count > 0)
        {
            clausesByName = new(StringComparer.Ordinal);

            foreach (var clause in constraintClauses)
            {
                var name = clause.TypeParameter.Identifier.ValueText;
                if (!clausesByName.TryGetValue(name, out var list))
                    clausesByName[name] = list = new List<TypeParameterConstraintClauseSyntax>();

                list.Add(clause);
            }
        }

        foreach (var parameter in typeParameterList.Parameters)
        {
            var identifier = parameter.Identifier;
            var location = syntaxTree.GetLocation(identifier.Span);
            var reference = parameter.GetReference();

            var (inlineKind, inlineRefs) = TypeParameterConstraintAnalyzer.AnalyzeInline(parameter);
            var clauseKind = TypeParameterConstraintKind.None;
            var clauseRefsBuilder = ImmutableArray.CreateBuilder<SyntaxReference>();

            if (clausesByName is not null &&
                clausesByName.TryGetValue(identifier.ValueText, out var matchingClauses))
            {
                foreach (var clause in matchingClauses)
                {
                    var (kind, refs) = TypeParameterConstraintAnalyzer.AnalyzeClause(clause);
                    clauseKind |= kind;
                    clauseRefsBuilder.AddRange(refs);
                }
            }

            var variance = parameter.VarianceKeyword.Kind switch
            {
                SyntaxKind.OutKeyword => VarianceKind.Out,
                SyntaxKind.InKeyword => VarianceKind.In,
                _ => VarianceKind.None,
            };

            yield return new SourceTypeParameterSymbol(
                identifier.ValueText,
                methodSymbol,
                containingType,
                methodSymbol.ContainingNamespace,
                [location],
                [reference],
                ordinal++,
                inlineKind | clauseKind,
                inlineRefs.AddRange(clauseRefsBuilder.ToImmutable()),
                variance);
        }
    }

    public static void DeclarePropertySignature(
        SemanticModel semanticModel,
        PropertyDeclarationSyntax propertyDeclaration)
    {
        var compilation = semanticModel.Compilation;

        SourcePropertySymbol? existingProperty = null;
        if (compilation.TryGetPropertySymbol(propertyDeclaration, out var existingSymbol))
        {
            if (existingSymbol is not SourcePropertySymbol existingSourceProperty ||
                !existingSourceProperty.Type.ContainsErrorType())
            {
                return;
            }

            existingProperty = existingSourceProperty;
        }

        if (existingProperty is not null &&
            !existingProperty.Type.ContainsErrorType())
        {
            return;
        }

        if (!TryGetContainingDeclaredType(compilation, propertyDeclaration, out var containingType))
            return;

        // Extension and explicit-interface properties need accessor/receiver/interface
        // state from the full binder before their symbols are reliable.
        if (containingType is SourceNamedTypeSymbol { IsExtensionDeclaration: true } ||
            propertyDeclaration.ExplicitInterfaceSpecifier is not null ||
            propertyDeclaration.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.PartialKeyword))
        {
            return;
        }

        if (!HasExplicitPropertyTypeAnnotation(propertyDeclaration))
            return;

        var propertyTypeSyntax = propertyDeclaration.Type.Type;
        var propertyType = ResolveSkeletonType(semanticModel, propertyTypeSyntax, compilation.ErrorTypeSymbol, containingType);
        if (existingProperty is not null &&
            propertyType.ContainsErrorType())
        {
            return;
        }

        var isStatic = propertyDeclaration.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.StaticKeyword);
        var defaultAccessibility = compilation.Options.MembersPublicByDefault
            ? Accessibility.Public
            : AccessibilityUtilities.GetDefaultMemberAccessibility(containingType);
        var propertyAccessibility = AccessibilityUtilities.DetermineAccessibility(
            propertyDeclaration.Modifiers,
            defaultAccessibility);
        var propertySymbol = new SourcePropertySymbol(
            propertyDeclaration.Identifier.ValueText,
            propertyType,
            containingType,
            containingType,
            containingType.ContainingNamespace,
            [propertyDeclaration.GetLocation()],
            [propertyDeclaration.GetReference()],
            isStatic: isStatic,
            declaredAccessibility: propertyAccessibility);

        if (propertyDeclaration.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.RequiredKeyword))
            propertySymbol.MarkAsRequired();

        ReportAsyncAccessorReturnTypeDiagnostics(
            semanticModel,
            propertyDeclaration.AccessorList?.Accessors,
            propertyType,
            propertyTypeSyntax);

        propertySymbol.SetMutability(propertyDeclaration.BindingKeyword.Kind == SyntaxKind.VarKeyword);
        propertySymbol.SetAccessors(
            CreatePropertyAccessorSymbol(
                propertyDeclaration,
                propertyDeclaration.AccessorList?.Accessors.FirstOrDefault(static accessor => accessor.Kind == SyntaxKind.GetAccessorDeclaration),
                propertySymbol,
                containingType,
                MethodKind.PropertyGet,
                "get_" + propertySymbol.Name,
                propertyType,
                propertyAccessibility),
            CreatePropertyAccessorSymbol(
                propertyDeclaration,
                propertyDeclaration.AccessorList?.Accessors.FirstOrDefault(static accessor => accessor.Kind == SyntaxKind.SetAccessorDeclaration),
                propertySymbol,
                containingType,
                MethodKind.PropertySet,
                "set_" + propertySymbol.Name,
                compilation.GetSpecialType(SpecialType.System_Unit),
                propertyAccessibility));
        compilation.RegisterPropertySymbol(propertyDeclaration, propertySymbol);
    }

    public static void DeclareIndexerSignature(
        SemanticModel semanticModel,
        IndexerDeclarationSyntax indexerDeclaration)
    {
        var compilation = semanticModel.Compilation;

        if (!TryGetContainingDeclaredType(compilation, indexerDeclaration, out var containingType))
            return;

        // Extension and explicit-interface indexers need receiver/interface state
        // from the full binder before their symbols are reliable.
        if (containingType is SourceNamedTypeSymbol { IsExtensionDeclaration: true } ||
            indexerDeclaration.ExplicitInterfaceSpecifier is not null ||
            indexerDeclaration.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.PartialKeyword))
        {
            return;
        }

        if (!HasExplicitPropertyTypeAnnotation(indexerDeclaration))
            return;

        foreach (var parameter in indexerDeclaration.ParameterList.Parameters)
        {
            if (parameter.TypeAnnotation is null)
            {
                semanticModel.ReportDeclarationParameterTypeAnnotationRequired(
                    parameter.Identifier.ValueText,
                    parameter.Identifier.GetLocation());
            }

            if (parameter.BindingKeyword.Kind is SyntaxKind.LetKeyword or SyntaxKind.ValKeyword or SyntaxKind.VarKeyword)
            {
                semanticModel.ReportDeclarationParameterBindingKeywordNotAllowed(
                    parameter.BindingKeyword.Text,
                    parameter.Identifier.ValueText,
                    parameter.BindingKeyword.GetLocation());
            }
        }

        var propertyTypeSyntax = indexerDeclaration.Type.Type;
        var propertyType = ResolveSkeletonType(semanticModel, propertyTypeSyntax, compilation.ErrorTypeSymbol, containingType);
        if (propertyType.TypeKind == TypeKind.Error &&
            propertyTypeSyntax is IdentifierNameSyntax identifierName)
        {
            semanticModel.ReportDeclarationNameDoesNotExist(
                identifierName.Identifier.ValueText,
                propertyTypeSyntax.GetLocation());
        }

        ReportAsyncAccessorReturnTypeDiagnostics(
            semanticModel,
            indexerDeclaration.AccessorList?.Accessors,
            propertyType,
            propertyTypeSyntax);
    }

    public static void DeclareFieldSignature(
        SemanticModel semanticModel,
        FieldDeclarationSyntax fieldDeclaration)
    {
        var compilation = semanticModel.Compilation;

        if (!TryGetContainingDeclaredType(compilation, fieldDeclaration, out var containingType))
            return;

        if (containingType is SourceNamedTypeSymbol { IsExtensionDeclaration: true })
            return;

        var isStatic = fieldDeclaration.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.StaticKeyword);
        var isReadonly = fieldDeclaration.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.ReadonlyKeyword);
        var isRequired = fieldDeclaration.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.RequiredKeyword);
        var defaultAccessibility = compilation.Options.MembersPublicByDefault
            ? Accessibility.Public
            : AccessibilityUtilities.GetDefaultMemberAccessibility(containingType);
        var fieldAccessibility = AccessibilityUtilities.DetermineAccessibility(
            fieldDeclaration.Modifiers,
            defaultAccessibility);

        foreach (var declarator in fieldDeclaration.Declaration.Declarators)
        {
            if (declarator.Initializer is not null ||
                declarator.TypeAnnotation?.Type is not { } typeSyntax ||
                containingType.GetDeclaredMembersWithoutEnsuring(declarator.Identifier.ValueText)
                    .OfType<IFieldSymbol>()
                    .Any(field => SymbolDeclarationUtilities.HasDeclaringSpan(field, declarator)))
            {
                continue;
            }

            var fieldType = ResolveSkeletonType(semanticModel, typeSyntax, compilation.ErrorTypeSymbol, containingType);
            if (fieldType.TypeKind == TypeKind.Error)
                continue;

            var isMutable = fieldDeclaration.FieldKeyword.Kind switch
            {
                SyntaxKind.FieldKeyword => !isReadonly,
                _ => !isReadonly,
            };

            var fieldSymbol = new SourceFieldSymbol(
                declarator.Identifier.ValueText,
                fieldType,
                isStatic: isStatic,
                isMutable: isMutable,
                isConst: false,
                constantValue: null!,
                containingType,
                containingType,
                containingType.ContainingNamespace,
                [declarator.GetLocation()],
                [declarator.GetReference(), fieldDeclaration.GetReference()],
                initializer: null,
                declaredAccessibility: fieldAccessibility);

            if (isRequired)
                fieldSymbol.MarkAsRequired();
        }
    }

    private static void ReportAsyncAccessorReturnTypeDiagnostics(
        SemanticModel semanticModel,
        SyntaxList<AccessorDeclarationSyntax>? accessors,
        ITypeSymbol propertyType,
        TypeSyntax propertyTypeSyntax)
    {
        if (accessors is null)
            return;

        foreach (var accessor in accessors)
        {
            var isAsyncGetter = accessor.Kind == SyntaxKind.GetAccessorDeclaration &&
                accessor.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.AsyncKeyword);
            if (!isAsyncGetter)
                continue;

            if (AsyncReturnTypeUtilities.IsValidAsyncReturnType(propertyType, allowErrorType: false))
                continue;

            var display = propertyType.TypeKind == TypeKind.Error
                ? propertyTypeSyntax.ToString()
                : propertyType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
            var suggestedReturnType = AsyncReturnTypeUtilities.GetSuggestedAsyncReturnTypeDisplay(
                semanticModel.Compilation,
                propertyType);
            semanticModel.ReportDeclarationAsyncReturnTypeMustBeTaskLike(
                display,
                suggestedReturnType,
                propertyTypeSyntax.GetLocation());
        }
    }

    public static void DeclareEventSignature(
        SemanticModel semanticModel,
        EventDeclarationSyntax eventDeclaration)
    {
        var compilation = semanticModel.Compilation;

        if (compilation.TryGetEventSymbol(eventDeclaration, out _))
            return;

        if (!TryGetContainingDeclaredType(compilation, eventDeclaration, out var containingType))
            return;

        if (containingType is SourceNamedTypeSymbol { IsExtensionDeclaration: true } ||
            eventDeclaration.ExplicitInterfaceSpecifier is not null ||
            eventDeclaration.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.PartialKeyword))
        {
            return;
        }

        var eventType = ResolveSkeletonType(semanticModel, eventDeclaration.Type.Type, compilation.ErrorTypeSymbol, containingType);
        if (eventType.TypeKind == TypeKind.Error)
            return;

        var isStatic = eventDeclaration.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.StaticKeyword);
        var defaultAccessibility = compilation.Options.MembersPublicByDefault
            ? Accessibility.Public
            : AccessibilityUtilities.GetDefaultMemberAccessibility(containingType);
        var eventAccessibility = AccessibilityUtilities.DetermineAccessibility(
            eventDeclaration.Modifiers,
            defaultAccessibility);
        var eventSymbol = new SourceEventSymbol(
            eventDeclaration.Identifier.ValueText,
            eventType,
            containingType,
            containingType,
            containingType.ContainingNamespace,
            [eventDeclaration.GetLocation()],
            [eventDeclaration.GetReference()],
            isStatic: isStatic,
            declaredAccessibility: eventAccessibility);

        eventSymbol.SetAccessors(
            CreateEventAccessorSymbol(
                compilation,
                eventDeclaration,
                eventDeclaration.AccessorList?.Accessors.FirstOrDefault(static accessor => accessor.Kind == SyntaxKind.AddAccessorDeclaration),
                eventSymbol,
                containingType,
                MethodKind.EventAdd,
                "add_" + eventSymbol.Name,
                eventType,
                eventAccessibility),
            CreateEventAccessorSymbol(
                compilation,
                eventDeclaration,
                eventDeclaration.AccessorList?.Accessors.FirstOrDefault(static accessor => accessor.Kind == SyntaxKind.RemoveAccessorDeclaration),
                eventSymbol,
                containingType,
                MethodKind.EventRemove,
                "remove_" + eventSymbol.Name,
                eventType,
                eventAccessibility));
        compilation.RegisterEventSymbol(eventDeclaration, eventSymbol);
    }

    private static SourceMethodSymbol? CreatePropertyAccessorSymbol(
        PropertyDeclarationSyntax propertyDeclaration,
        AccessorDeclarationSyntax? accessorDeclaration,
        SourcePropertySymbol propertySymbol,
        INamedTypeSymbol containingType,
        MethodKind methodKind,
        string name,
        ITypeSymbol returnType,
        Accessibility accessibility)
    {
        if (accessorDeclaration is null)
            return null;

        var methodSymbol = new SourceMethodSymbol(
            name,
            returnType,
            ImmutableArray<SourceParameterSymbol>.Empty,
            propertySymbol,
            containingType,
            containingType.ContainingNamespace,
            [accessorDeclaration.GetLocation()],
            [accessorDeclaration.GetReference()],
            isStatic: propertySymbol.IsStatic,
            methodKind: methodKind,
            declaredAccessibility: accessibility);

        if (methodKind == MethodKind.PropertySet)
        {
            methodSymbol.SetParameters([
                new SourceParameterSymbol(
                    "value",
                    propertySymbol.Type,
                    methodSymbol,
                    containingType,
                    containingType.ContainingNamespace,
                    [propertyDeclaration.GetLocation()],
                    [propertyDeclaration.GetReference()])
            ]);
        }

        return methodSymbol;
    }

    private static SourceMethodSymbol CreateEventAccessorSymbol(
        Compilation compilation,
        EventDeclarationSyntax eventDeclaration,
        AccessorDeclarationSyntax? accessorDeclaration,
        SourceEventSymbol eventSymbol,
        INamedTypeSymbol containingType,
        MethodKind methodKind,
        string name,
        ITypeSymbol eventType,
        Accessibility accessibility)
    {
        var location = accessorDeclaration?.GetLocation() ?? eventDeclaration.GetLocation();
        var reference = accessorDeclaration?.GetReference() ?? eventDeclaration.GetReference();
        var methodSymbol = new SourceMethodSymbol(
            name,
            compilation.GetSpecialType(SpecialType.System_Unit),
            ImmutableArray<SourceParameterSymbol>.Empty,
            eventSymbol,
            containingType,
            containingType.ContainingNamespace,
            [location],
            [reference],
            isStatic: eventSymbol.IsStatic,
            methodKind: methodKind,
            declaredAccessibility: accessibility);

        methodSymbol.SetParameters([
            new SourceParameterSymbol(
                "value",
                eventType,
                methodSymbol,
                containingType,
                containingType.ContainingNamespace,
                [eventDeclaration.GetLocation()],
                [eventDeclaration.GetReference()])
        ]);

        return methodSymbol;
    }

    private static bool TryGetContainingDeclaredType(
        Compilation compilation,
        SyntaxNode declaration,
        out SourceNamedTypeSymbol containingType)
    {
        for (var current = declaration.Parent; current is not null; current = current.Parent)
        {
            if (current is TypeDeclarationSyntax or InterfaceDeclarationSyntax or ExtensionDeclarationSyntax or UnionDeclarationSyntax)
            {
                if (compilation.TryGetDeclaredTypeSymbol(current, out containingType!))
                    return true;
            }
        }

        containingType = null!;
        return false;
    }

    private static bool HasExplicitPropertyTypeAnnotation(PropertyDeclarationSyntax propertyDeclaration)
    {
        if (propertyDeclaration.Type.ColonToken.IsMissing)
            return false;

        return propertyDeclaration.Type.Type switch
        {
            IdentifierNameSyntax { Identifier.IsMissing: true } => false,
            _ => true
        };
    }

    private static bool HasExplicitPropertyTypeAnnotation(IndexerDeclarationSyntax indexerDeclaration)
    {
        if (indexerDeclaration.Type.ColonToken.IsMissing)
            return false;

        return indexerDeclaration.Type.Type switch
        {
            IdentifierNameSyntax { Identifier.IsMissing: true } => false,
            _ => true
        };
    }

    internal static SourceParameterSymbol CreateSkeletonParameterSymbol(
        SemanticModel semanticModel,
        ParameterSyntax parameter,
        SourceMethodSymbol methodSymbol,
        INamedTypeSymbol containingType)
    {
        var compilation = semanticModel.Compilation;
        var typeSyntax = parameter.TypeAnnotation?.Type;
        var parameterType = typeSyntax is null
            ? compilation.ErrorTypeSymbol
            : ResolveSkeletonType(semanticModel, typeSyntax, compilation.ErrorTypeSymbol, containingType, methodSymbol.TypeParameters);
        var defaultEvaluation = TypeMemberBinder.EvaluateParameterDefaultValue(parameter, parameterType);
        var hasExplicitDefaultValue = defaultEvaluation is { HasDefaultSyntax: true, Success: true };
        var refKind = ParameterSyntaxUtilities.GetRefKind(parameter);

        return new SourceParameterSymbol(
            parameter.Identifier.ValueText,
            parameterType,
            methodSymbol,
            containingType,
            containingType.ContainingNamespace,
            [parameter.Identifier.GetLocation()],
            [parameter.GetReference()],
            refKind,
            hasExplicitDefaultValue,
            hasExplicitDefaultValue ? defaultEvaluation.Value : null,
            isMutable: refKind is RefKind.Ref or RefKind.Out,
            isVarParams: TypeMemberBinder.IsVarParamsSyntax(parameter));
    }

    internal static ITypeSymbol ResolveSkeletonType(
        SemanticModel semanticModel,
        TypeSyntax typeSyntax,
        ITypeSymbol fallbackType,
        INamedTypeSymbol? containingType = null,
        ImmutableArray<ITypeParameterSymbol> methodTypeParameters = default)
    {
        var compilation = semanticModel.Compilation;
        return typeSyntax switch
        {
            UnitTypeSyntax => compilation.GetSpecialType(SpecialType.System_Unit),
            PredefinedTypeSyntax predefined => ResolvePredefinedSkeletonType(compilation, predefined.Keyword.Kind, fallbackType),
            ParenthesizedTypeSyntax parenthesized => ResolveSkeletonType(
                semanticModel,
                parenthesized.Type,
                fallbackType,
                containingType,
                methodTypeParameters),
            QualifiedNameSyntax qualifiedName => ResolveQualifiedSkeletonType(
                semanticModel,
                qualifiedName,
                fallbackType,
                containingType,
                methodTypeParameters),
            IdentifierNameSyntax identifier => ResolveIdentifierSkeletonType(
                semanticModel,
                typeSyntax,
                identifier.Identifier.ValueText,
                containingType,
                methodTypeParameters,
                fallbackType,
                arity: 0),
            ArrayTypeSyntax array => ResolveArraySkeletonType(semanticModel, array, fallbackType, containingType, methodTypeParameters),
            ByRefTypeSyntax byRef => ResolveSkeletonType(semanticModel, byRef.ElementType, fallbackType, containingType, methodTypeParameters),
            GenericNameSyntax genericName => ResolveGenericSkeletonType(semanticModel, genericName, fallbackType, containingType, methodTypeParameters),
            _ => fallbackType
        };
    }

    private static ITypeSymbol ResolveArraySkeletonType(
        SemanticModel semanticModel,
        ArrayTypeSyntax array,
        ITypeSymbol fallbackType,
        INamedTypeSymbol? containingType,
        ImmutableArray<ITypeParameterSymbol> methodTypeParameters)
    {
        var compilation = semanticModel.Compilation;
        var type = ResolveSkeletonType(semanticModel, array.ElementType, fallbackType, containingType, methodTypeParameters);
        if (type.TypeKind == TypeKind.Error)
            return fallbackType;

        foreach (var rankSpecifier in array.RankSpecifiers)
        {
            var rank = rankSpecifier.CommaTokens.Count + 1;
            type = compilation.CreateArrayTypeSymbol(type, rank, TryGetFixedArraySize(rankSpecifier));
        }

        return type;
    }

    private static int? TryGetFixedArraySize(ArrayRankSpecifierSyntax rankSpecifier)
    {
        if (rankSpecifier.CommaTokens.Count != 0 ||
            rankSpecifier.SizeToken.Kind != SyntaxKind.NumericLiteralToken)
        {
            return null;
        }

        return int.TryParse(rankSpecifier.SizeToken.ValueText, out var fixedSize)
            ? fixedSize
            : null;
    }

    private static ITypeSymbol ResolveGenericSkeletonType(
        SemanticModel semanticModel,
        GenericNameSyntax genericName,
        ITypeSymbol fallbackType,
        INamedTypeSymbol? containingType,
        ImmutableArray<ITypeParameterSymbol> methodTypeParameters)
    {
        var compilation = semanticModel.Compilation;
        var definition = ResolveIdentifierSkeletonType(
            semanticModel,
            genericName,
            genericName.Identifier.ValueText,
            containingType,
            methodTypeParameters,
            fallbackType,
            genericName.TypeArgumentList.Arguments.Count);

        if (definition is not INamedTypeSymbol namedDefinition ||
            genericName.TypeArgumentList.Arguments.Count != namedDefinition.Arity)
        {
            return fallbackType;
        }

        var arguments = genericName.TypeArgumentList.Arguments
            .Select(argument => ResolveSkeletonType(semanticModel, argument.Type, fallbackType, containingType, methodTypeParameters))
            .ToArray();

        if (arguments.Any(static argument => argument.TypeKind == TypeKind.Error))
            return fallbackType;

        return namedDefinition.Construct(arguments);
    }

    private static ITypeSymbol ResolveQualifiedSkeletonType(
        SemanticModel semanticModel,
        QualifiedNameSyntax qualifiedName,
        ITypeSymbol fallbackType,
        INamedTypeSymbol? containingType,
        ImmutableArray<ITypeParameterSymbol> methodTypeParameters)
    {
        if (!TryGetQualifiedMetadataName(qualifiedName, out var metadataName))
            return fallbackType;

        var genericName = qualifiedName.Right as GenericNameSyntax;
        var arity = genericName is not null
            ? genericName.TypeArgumentList.Arguments.Count
            : (int?)null;
        var resolved = ResolveExactImportedSkeletonType(semanticModel.Compilation, metadataName, arity);
        if (resolved is null)
            return fallbackType;

        if (genericName is null)
            return resolved;

        if (resolved is not INamedTypeSymbol namedDefinition ||
            genericName.TypeArgumentList.Arguments.Count != namedDefinition.Arity)
        {
            return fallbackType;
        }

        var arguments = genericName.TypeArgumentList.Arguments
            .Select(argument => ResolveSkeletonType(
                semanticModel,
                argument.Type,
                fallbackType,
                containingType,
                methodTypeParameters))
            .ToArray();

        if (arguments.Any(static argument => argument.TypeKind == TypeKind.Error))
            return fallbackType;

        return namedDefinition.Construct(arguments);
    }

    private static bool TryGetQualifiedMetadataName(
        NameSyntax nameSyntax,
        out string metadataName)
    {
        switch (nameSyntax)
        {
            case IdentifierNameSyntax identifier:
                metadataName = identifier.Identifier.ValueText;
                return !string.IsNullOrEmpty(metadataName);

            case QualifiedNameSyntax qualifiedName when
                TryGetQualifiedMetadataName(qualifiedName.Left, out var left) &&
                TryGetUnqualifiedMetadataName(qualifiedName.Right, out var right):
                metadataName = left + "." + right;
                return true;

            default:
                metadataName = string.Empty;
                return false;
        }
    }

    private static bool TryGetUnqualifiedMetadataName(
        UnqualifiedNameSyntax nameSyntax,
        out string metadataName)
    {
        switch (nameSyntax)
        {
            case IdentifierNameSyntax identifier:
                metadataName = identifier.Identifier.ValueText;
                return !string.IsNullOrEmpty(metadataName);

            case GenericNameSyntax genericName:
                metadataName = GetMetadataTypeName(
                    genericName.Identifier.ValueText,
                    genericName.TypeArgumentList.Arguments.Count);
                return !string.IsNullOrEmpty(metadataName);

            default:
                metadataName = string.Empty;
                return false;
        }
    }

    private static ITypeSymbol ResolveIdentifierSkeletonType(
        SemanticModel semanticModel,
        SyntaxNode contextNode,
        string name,
        INamedTypeSymbol? containingType,
        ImmutableArray<ITypeParameterSymbol> methodTypeParameters,
        ITypeSymbol fallbackType,
        int? arity)
    {
        var compilation = semanticModel.Compilation;

        if (!methodTypeParameters.IsDefaultOrEmpty &&
            methodTypeParameters.FirstOrDefault(typeParameter => string.Equals(typeParameter.Name, name, StringComparison.Ordinal)) is { } methodTypeParameter)
        {
            return methodTypeParameter;
        }

        for (var currentType = containingType; currentType is not null; currentType = currentType.ContainingType)
        {
            var typeParameter = currentType.TypeParameters.FirstOrDefault(typeParameter => string.Equals(typeParameter.Name, name, StringComparison.Ordinal));
            if (typeParameter is not null)
                return typeParameter;

            var nestedType = currentType.LookupType(name);
            if (nestedType is not null)
                return nestedType;
        }

        var namespaceType = ResolveTypeInNamespaceForSkeleton(
            compilation,
            containingType?.ContainingNamespace,
            name,
            arity);
        if (namespaceType is not null)
            return namespaceType;

        var importedType = ResolveImportedSkeletonType(semanticModel, contextNode, name, arity);
        if (importedType is not null)
            return importedType;

        return compilation.SymbolLookup.LookupTypeSourceFirst(null, name) ?? fallbackType;
    }

    private static ITypeSymbol? ResolveImportedSkeletonType(
        SemanticModel semanticModel,
        SyntaxNode contextNode,
        string name,
        int? arity)
    {
        var compilation = semanticModel.Compilation;

        foreach (var import in EnumerateEffectiveImports(semanticModel.Compilation, contextNode))
        {
            var importName = import.Name.ToString();
            if (string.IsNullOrWhiteSpace(importName))
                continue;

            if (TryGetWildcardImportName(importName, out var scopeName))
            {
                if (ResolveWildcardImportedSkeletonType(compilation, scopeName, name, arity) is { } scopedType)
                {
                    return scopedType;
                }

                continue;
            }

            if (!NameEndsWith(importName, name))
                continue;

            var importedType = ResolveExactImportedSkeletonType(compilation, importName, arity);
            if (importedType is not null && MatchesArity(importedType, arity))
                return importedType;
        }

        return null;
    }

    private static IEnumerable<ImportDirectiveSyntax> EnumerateEffectiveImports(
        Compilation compilation,
        SyntaxNode contextNode)
    {
        foreach (var tree in compilation.SyntaxTrees)
        {
            if (tree.GetRoot() is not CompilationUnitSyntax root)
                continue;

            foreach (var globalImport in root.Members.OfType<GlobalImportBlockSyntax>())
            {
                foreach (var import in globalImport.Imports)
                    yield return import;
            }
        }

        if (contextNode.SyntaxTree.GetRoot() is not CompilationUnitSyntax compilationUnit)
            yield break;

        foreach (var import in compilationUnit.Imports)
            yield return import;

        foreach (var namespaceDeclaration in contextNode
                     .AncestorsAndSelf()
                     .OfType<BaseNamespaceDeclarationSyntax>()
                     .Reverse())
        {
            foreach (var import in namespaceDeclaration.Imports)
                yield return import;
        }
    }

    private static ITypeSymbol? ResolveWildcardImportedSkeletonType(
        Compilation compilation,
        string scopeName,
        string name,
        int? arity)
    {
        var metadataTypeName = GetMetadataTypeName(name, arity);
        var scopedMetadataName = scopeName + "." + metadataTypeName;

        if (ResolveExactImportedSkeletonType(compilation, scopedMetadataName, arity) is { } exactType)
            return exactType;

        if (ResolveTypeInNamespaceForSkeleton(compilation, ResolveSourceNamespace(compilation, scopeName), name, arity) is { } sourceType &&
            MatchesArity(sourceType, arity))
        {
            return sourceType;
        }

        if (ResolveExactImportedSkeletonType(compilation, scopeName, arity: null) is { } importedTypeScope &&
            importedTypeScope.LookupType(name) is { } nestedType &&
            MatchesArity(nestedType, arity))
        {
            return nestedType;
        }

        return null;
    }

    private static ITypeSymbol? ResolveExactImportedSkeletonType(
        Compilation compilation,
        string metadataName,
        int? arity)
    {
        var resolved = compilation.TryGetMetadataReferenceTypeByMetadataName(metadataName)
            ?? compilation.Assembly.GetTypeByMetadataName(metadataName);

        return MatchesArity(resolved, arity) ? resolved : null;
    }

    private static ITypeSymbol? ResolveTypeInNamespaceForSkeleton(
        Compilation compilation,
        INamespaceSymbol? namespaceSymbol,
        string name,
        int? arity)
    {
        if (namespaceSymbol is null)
            return null;

        var resolved = namespaceSymbol is SourceNamespaceSymbol sourceNamespace
            ? sourceNamespace.LookupTypeDeclared(name)
            : namespaceSymbol.LookupType(name);

        if (resolved is not null && MatchesArity(resolved, arity))
            return resolved;

        return compilation.SymbolLookup.LookupTypeSourceFirst(namespaceSymbol, name) is { } sourceFirst &&
            MatchesArity(sourceFirst, arity)
            ? sourceFirst
            : null;
    }

    private static INamespaceSymbol? ResolveSourceNamespace(
        Compilation compilation,
        string metadataName)
    {
        var parts = metadataName.Split('.', StringSplitOptions.RemoveEmptyEntries);
        if (parts.Length == 0)
            return compilation.SourceGlobalNamespace;

        INamespaceSymbol current = compilation.SourceGlobalNamespace;
        foreach (var part in parts)
        {
            current = current.GetMembers(part).OfType<INamespaceSymbol>().FirstOrDefault();
            if (current is null)
                return null;
        }

        return current;
    }

    private static bool TryGetWildcardImportName(string importName, out string scopeName)
    {
        const string suffix = ".*";
        if (importName.EndsWith(suffix, StringComparison.Ordinal))
        {
            scopeName = importName[..^suffix.Length];
            return scopeName.Length > 0;
        }

        scopeName = string.Empty;
        return false;
    }

    private static bool NameEndsWith(string metadataName, string name)
        => string.Equals(metadataName, name, StringComparison.Ordinal) ||
           metadataName.EndsWith("." + name, StringComparison.Ordinal);

    private static bool MatchesArity(ITypeSymbol? type, int? arity)
        => type is not null &&
           (arity is null ||
           type is not INamedTypeSymbol namedType ||
           namedType.Arity == arity);

    private static string GetMetadataTypeName(string name, int? arity)
        => arity is > 0 ? name + "`" + arity.Value : name;

    private static ITypeSymbol ResolvePredefinedSkeletonType(
        Compilation compilation,
        SyntaxKind kind,
        ITypeSymbol fallbackType)
        => kind switch
        {
            SyntaxKind.BoolKeyword => compilation.GetSpecialType(SpecialType.System_Boolean),
            SyntaxKind.CharKeyword => compilation.GetSpecialType(SpecialType.System_Char),
            SyntaxKind.SByteKeyword => compilation.GetSpecialType(SpecialType.System_SByte),
            SyntaxKind.ShortKeyword => compilation.GetSpecialType(SpecialType.System_Int16),
            SyntaxKind.UShortKeyword => compilation.GetSpecialType(SpecialType.System_UInt16),
            SyntaxKind.DoubleKeyword => compilation.GetSpecialType(SpecialType.System_Double),
            SyntaxKind.DecimalKeyword => compilation.GetSpecialType(SpecialType.System_Decimal),
            SyntaxKind.FloatKeyword => compilation.GetSpecialType(SpecialType.System_Single),
            SyntaxKind.IntKeyword => compilation.GetSpecialType(SpecialType.System_Int32),
            SyntaxKind.NIntKeyword => compilation.GetSpecialType(SpecialType.System_IntPtr),
            SyntaxKind.NUIntKeyword => compilation.GetSpecialType(SpecialType.System_UIntPtr),
            SyntaxKind.ByteKeyword => compilation.GetSpecialType(SpecialType.System_Byte),
            SyntaxKind.ObjectKeyword => compilation.GetSpecialType(SpecialType.System_Object),
            SyntaxKind.LongKeyword => compilation.GetSpecialType(SpecialType.System_Int64),
            SyntaxKind.ULongKeyword => compilation.GetSpecialType(SpecialType.System_UInt64),
            SyntaxKind.StringKeyword => compilation.GetSpecialType(SpecialType.System_String),
            SyntaxKind.UIntKeyword => compilation.GetSpecialType(SpecialType.System_UInt32),
            SyntaxKind.UnitKeyword => compilation.GetSpecialType(SpecialType.System_Unit),
            _ => fallbackType
        };
}
