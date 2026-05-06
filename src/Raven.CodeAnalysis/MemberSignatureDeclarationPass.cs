using System.Collections.Immutable;

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
        var returnType = methodDeclaration.ReturnType is { } returnTypeSyntax
            ? ResolveSkeletonType(compilation, returnTypeSyntax.Type, defaultReturnType, containingType)
            : defaultReturnType;

        var methodSymbol = new SourceMethodSymbol(
            methodDeclaration.Identifier.ValueText,
            returnType,
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

        TypeParameterInitializer.InitializeMethodTypeParameters(
            methodSymbol,
            containingType,
            methodDeclaration.TypeParameterList,
            methodDeclaration.ConstraintClauses,
            methodDeclaration.SyntaxTree);

        var parameters = methodDeclaration.ParameterList?.Parameters
            .Select(parameter => CreateSkeletonParameterSymbol(compilation, parameter, methodSymbol, containingType))
            .ToImmutableArray() ?? ImmutableArray<SourceParameterSymbol>.Empty;

        methodSymbol.SetParameters(parameters);
        methodSymbol.MarkSignatureSkeleton();
        compilation.RegisterMethodSymbol(methodDeclaration, methodSymbol);
    }

    public static void DeclarePropertySignature(
        SemanticModel semanticModel,
        PropertyDeclarationSyntax propertyDeclaration)
    {
        var compilation = semanticModel.Compilation;

        if (compilation.TryGetPropertySymbol(propertyDeclaration, out _))
            return;

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

        var propertyType = ResolveSkeletonType(compilation, propertyDeclaration.Type.Type, compilation.ErrorTypeSymbol, containingType);
        if (propertyType.TypeKind == TypeKind.Error)
            return;

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

        var eventType = ResolveSkeletonType(compilation, eventDeclaration.Type.Type, compilation.ErrorTypeSymbol, containingType);
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

    private static SourceParameterSymbol CreateSkeletonParameterSymbol(
        Compilation compilation,
        ParameterSyntax parameter,
        SourceMethodSymbol methodSymbol,
        INamedTypeSymbol containingType)
    {
        var typeSyntax = parameter.TypeAnnotation?.Type;
        var parameterType = typeSyntax is null
            ? compilation.ErrorTypeSymbol
            : ResolveSkeletonType(compilation, typeSyntax, compilation.ErrorTypeSymbol, containingType, methodSymbol.TypeParameters);

        return new SourceParameterSymbol(
            parameter.Identifier.ValueText,
            parameterType,
            methodSymbol,
            containingType,
            containingType.ContainingNamespace,
            [parameter.Identifier.GetLocation()],
            [parameter.GetReference()],
            ParameterSyntaxUtilities.GetRefKind(parameter));
    }

    private static ITypeSymbol ResolveSkeletonType(
        Compilation compilation,
        TypeSyntax typeSyntax,
        ITypeSymbol fallbackType,
        INamedTypeSymbol? containingType = null,
        ImmutableArray<ITypeParameterSymbol> methodTypeParameters = default)
        => typeSyntax switch
        {
            UnitTypeSyntax => compilation.GetSpecialType(SpecialType.System_Unit),
            PredefinedTypeSyntax predefined => ResolvePredefinedSkeletonType(compilation, predefined.Keyword.Kind, fallbackType),
            QualifiedNameSyntax qualifiedName => ResolveQualifiedSkeletonType(compilation, qualifiedName, fallbackType),
            IdentifierNameSyntax identifier => ResolveIdentifierSkeletonType(
                compilation,
                identifier.Identifier.ValueText,
                containingType,
                methodTypeParameters,
                fallbackType),
            ByRefTypeSyntax byRef => ResolveSkeletonType(compilation, byRef.ElementType, fallbackType, containingType, methodTypeParameters),
            _ => fallbackType
        };

    private static ITypeSymbol ResolveQualifiedSkeletonType(
        Compilation compilation,
        QualifiedNameSyntax qualifiedName,
        ITypeSymbol fallbackType)
        => TryGetQualifiedMetadataName(qualifiedName, out var metadataName)
            ? compilation.GetTypeByMetadataName(metadataName) ?? fallbackType
            : fallbackType;

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

            default:
                metadataName = string.Empty;
                return false;
        }
    }

    private static ITypeSymbol ResolveIdentifierSkeletonType(
        Compilation compilation,
        string name,
        INamedTypeSymbol? containingType,
        ImmutableArray<ITypeParameterSymbol> methodTypeParameters,
        ITypeSymbol fallbackType)
    {
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

        var namespaceType = containingType?.ContainingNamespace?.LookupType(name);
        if (namespaceType is not null)
            return namespaceType;

        return compilation.GlobalNamespace.LookupType(name) ?? fallbackType;
    }

    private static ITypeSymbol ResolvePredefinedSkeletonType(
        Compilation compilation,
        SyntaxKind kind,
        ITypeSymbol fallbackType)
        => kind switch
        {
            SyntaxKind.BoolKeyword => compilation.GetSpecialType(SpecialType.System_Boolean),
            SyntaxKind.DoubleKeyword => compilation.GetSpecialType(SpecialType.System_Double),
            SyntaxKind.FloatKeyword => compilation.GetSpecialType(SpecialType.System_Single),
            SyntaxKind.IntKeyword => compilation.GetSpecialType(SpecialType.System_Int32),
            SyntaxKind.NIntKeyword => compilation.GetSpecialType(SpecialType.System_IntPtr),
            SyntaxKind.NUIntKeyword => compilation.GetSpecialType(SpecialType.System_UIntPtr),
            SyntaxKind.StringKeyword => compilation.GetSpecialType(SpecialType.System_String),
            SyntaxKind.UIntKeyword => compilation.GetSpecialType(SpecialType.System_UInt32),
            SyntaxKind.UnitKeyword => compilation.GetSpecialType(SpecialType.System_Unit),
            _ => fallbackType
        };
}
