using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Collections.ObjectModel;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Threading;

using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    private readonly Dictionary<LabeledStatementSyntax, ILabelSymbol> _labelDeclarations = new();
    private readonly Dictionary<ILabelSymbol, LabeledStatementSyntax> _labelSyntax = new(SymbolEqualityComparer.Default);
    private readonly Dictionary<string, List<ILabelSymbol>> _labelsByName = new(StringComparer.Ordinal);
    private readonly Dictionary<GotoStatementSyntax, ILabelSymbol> _gotoTargets = new();
    private readonly Dictionary<AttributeSyntax, AttributeData?> _attributeCache = new();
    private static readonly object s_boundChildPropertyCacheGate = new();
    private static readonly Dictionary<Type, PropertyInfo[]> s_boundChildPropertyCache = new();
    private static readonly DiagnosticDescriptor s_globalStatementsDisabled = DiagnosticDescriptor.Create(
        "RAV7001",
        "Top-level statements are disabled",
        "",
        "",
        "Top-level statements are disabled by compilation options",
        "compiler",
        DiagnosticSeverity.Error,
        true);

    internal AttributeData? BindAttribute(AttributeSyntax attribute)
    {
        if (attribute is null)
            throw new ArgumentNullException(nameof(attribute));

        if (_attributeCache.TryGetValue(attribute, out var cached))
            return cached;

        EnsureDiagnosticsCollected();

        BoundExpression? boundExpression = TryGetCachedBoundNode(attribute) as BoundExpression;
        var binderNode = (SyntaxNode?)attribute.Parent ?? attribute;
        var binder = GetBinder(binderNode);

        if (boundExpression is null)
        {
            var attributeBinder = binder as AttributeBinder
                ?? new AttributeBinder(binder.ContainingSymbol, binder);
            boundExpression = attributeBinder.BindAttribute(attribute);
        }

        var data = AttributeDataFactory.Create(boundExpression, attribute);

        _attributeCache[attribute] = data;
        return data;
    }

    internal void RegisterLabel(LabeledStatementSyntax syntax, ILabelSymbol symbol)
    {
        _labelDeclarations[syntax] = symbol;
        _labelSyntax[symbol] = syntax;

        if (!_labelsByName.TryGetValue(symbol.Name, out var list))
        {
            list = new List<ILabelSymbol>();
            _labelsByName[symbol.Name] = list;
        }

        if (!list.Any(existing => SymbolEqualityComparer.Default.Equals(existing, symbol)))
            list.Add(symbol);
    }

    internal void RegisterGoto(GotoStatementSyntax syntax, ILabelSymbol target)
    {
        _gotoTargets[syntax] = target;
    }

    internal void EnsureDeclarations()
    {
        if (_declarationsComplete)
            return;

        if (SyntaxTree.GetRoot() is CompilationUnitSyntax cu)
            DeclareCompilationUnit(cu);

        _declarationsComplete = true;
    }

    private void DeclareCompilationUnit(CompilationUnitSyntax cu)
    {
        var fileScopedNamespace = cu.Members.OfType<FileScopedNamespaceDeclarationSyntax>().FirstOrDefault();
        INamespaceSymbol targetNamespace;

        if (fileScopedNamespace is not null)
        {
            targetNamespace = Compilation.GetOrCreateNamespaceSymbol(fileScopedNamespace.Name.ToString())
                ?? throw new Exception("Namespace not found");
        }
        else
        {
            targetNamespace = Compilation.GlobalNamespace;
        }

        DeclareNamespaceMembers(cu, targetNamespace);
    }

    private void DeclareNamespaceMembers(SyntaxNode containerNode, INamespaceSymbol parentNamespace)
    {
        var objectType = Compilation.GetTypeByMetadataName("System.Object");

        foreach (var member in containerNode.ChildNodes())
        {
            switch (member)
            {
                case BaseNamespaceDeclarationSyntax nsDecl:
                    {
                        var namespaceName = nsDecl is FileScopedNamespaceDeclarationSyntax
                            ? nsDecl.Name.ToString()
                            : parentNamespace.QualifyName(nsDecl.Name.ToString());

                        var nsSymbol = Compilation.GetOrCreateNamespaceSymbol(namespaceName)
                            ?? throw new Exception($"Unable to resolve namespace '{namespaceName}'.");

                        DeclareNamespaceMembers(nsDecl, nsSymbol);
                        break;
                    }

                case TypeDeclarationSyntax typeDecl when IsNominalTypeDeclaration(typeDecl):
                    {
                        var typeSymbol = DeclareClassSymbol(typeDecl, parentNamespace, objectType);
                        DeclareClassMemberTypes(typeDecl, typeSymbol);
                        break;
                    }

                case DelegateDeclarationSyntax delegateDecl:
                    {
                        DeclareDelegateSymbol(delegateDecl, parentNamespace);
                        break;
                    }

                case InterfaceDeclarationSyntax interfaceDecl:
                    {
                        DeclareInterfaceSymbol(interfaceDecl, parentNamespace, objectType);
                        break;
                    }

                case ExtensionDeclarationSyntax extensionDecl:
                    {
                        DeclareExtensionSymbol(extensionDecl, parentNamespace, objectType);
                        break;
                    }

                case EnumDeclarationSyntax enumDecl:
                    {
                        DeclareEnumSymbol(enumDecl, parentNamespace);
                        break;
                    }

                case UnionDeclarationSyntax unionDecl:
                    {
                        DeclareUnionSymbol(unionDecl, parentNamespace);
                        break;
                    }
            }
        }
    }

    private SourceNamedTypeSymbol DeclareClassSymbol(
        TypeDeclarationSyntax classDecl,
        INamespaceSymbol parentNamespace,
        INamedTypeSymbol? objectType)
    {
        ReportInvalidTypeModifiers(classDecl, isNestedType: false, _declarationDiagnostics);
        ReportRedundantTypeModifiers(classDecl, _declarationDiagnostics);

        var declaredTypeKind = IsStructLikeNominalType(classDecl)
            ? TypeKind.Struct
            : TypeKind.Class;
        var defaultBaseType = declaredTypeKind == TypeKind.Struct
            ? Compilation.GetSpecialType(SpecialType.System_ValueType)
            : objectType;

        var hasSealedModifier = classDecl.Modifiers.Any(m => m.Kind == SyntaxKind.SealedKeyword);
        var isStatic = classDecl.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);
        var isAbstract = isStatic || classDecl.Modifiers.Any(m => m.Kind == SyntaxKind.AbstractKeyword) || hasSealedModifier;
        var isSealedHierarchy = hasSealedModifier && !isStatic;
        var isSealed = isStatic || (!isSealedHierarchy && !classDecl.Modifiers.Any(m => m.Kind == SyntaxKind.OpenKeyword) && !isAbstract);
        var isPartial = classDecl.Modifiers.Any(m => m.Kind == SyntaxKind.PartialKeyword);
        var hasPermitsClause = (classDecl is ClassDeclarationSyntax classDeclaration2 && classDeclaration2.PermitsClause is not null)
            || (classDecl is RecordDeclarationSyntax recordDeclaration2 && recordDeclaration2.PermitsClause is not null);

        if (hasPermitsClause && !hasSealedModifier)
        {
            _declarationDiagnostics.ReportPermitsClauseRequiresSealed(
                classDecl.Identifier.GetLocation());
        }
        var isRecord = classDecl is RecordDeclarationSyntax || classDecl.Modifiers.Any(m => m.Kind == SyntaxKind.RecordKeyword);
        var typeAccessibility = AccessibilityUtilities.DetermineAccessibility(
            classDecl.Modifiers,
            AccessibilityUtilities.GetDefaultTypeAccessibility(parentNamespace.AsSourceNamespace()));

        var declarationLocation = classDecl.GetLocation();
        var declarationReference = classDecl.GetReference();

        var parentSourceNamespace = parentNamespace.AsSourceNamespace();
        SourceNamedTypeSymbol classSymbol;
        var isNewSymbol = true;

        ReportExternalTypeRedeclaration(
            parentNamespace,
            classDecl.Identifier,
            GetTypeParameterList(classDecl)?.Parameters.Count ?? 0,
            _declarationDiagnostics);

        if (parentSourceNamespace is not null &&
            parentSourceNamespace.IsMemberDefined(classDecl.Identifier.ValueText, out var existingMember) &&
            existingMember is SourceNamedTypeSymbol existingType &&
            existingType.TypeKind == declaredTypeKind)
        {
            var hadPartial = existingType.HasPartialModifier;
            var hadNonPartial = existingType.HasNonPartialDeclaration;
            var previouslyMixed = hadPartial && hadNonPartial;
            var willBeMixed = (hadPartial || isPartial) && (hadNonPartial || !isPartial);

            if (willBeMixed && !previouslyMixed)
            {
                _declarationDiagnostics.ReportPartialTypeDeclarationMissingPartial(
                    classDecl.Identifier.ValueText,
                    classDecl.Identifier.GetLocation());
            }
            else if (hadNonPartial && !isPartial)
            {
                _declarationDiagnostics.ReportTypeAlreadyDefined(
                    classDecl.Identifier.ValueText,
                    classDecl.Identifier.GetLocation());
            }

            existingType.AddDeclaration(declarationLocation, declarationReference);
            existingType.UpdateDeclarationModifiers(isSealed, isAbstract, isStatic);
            existingType.RegisterPartialModifier(isPartial);
            existingType.RegisterRecordModifier(isRecord);

            if (isSealedHierarchy)
                existingType.MarkAsSealedHierarchy(classDecl.SyntaxTree.FilePath, hasPermitsClause);

            classSymbol = existingType;
            isNewSymbol = false;
        }
        else
        {
            classSymbol = new SourceNamedTypeSymbol(
                classDecl.Identifier.ValueText,
                defaultBaseType!,
                declaredTypeKind,
                parentNamespace.AsSourceNamespace(),
                null,
                parentNamespace.AsSourceNamespace(),
                new[] { declarationLocation },
                new[] { declarationReference },
                isSealed,
                isAbstract,
                isStatic,
                declaredAccessibility: typeAccessibility);

            classSymbol.RegisterPartialModifier(isPartial);
            classSymbol.RegisterRecordModifier(isRecord);

            if (isSealedHierarchy)
                classSymbol.MarkAsSealedHierarchy(classDecl.SyntaxTree.FilePath, hasPermitsClause);
        }

        if (isNewSymbol)
            InitializeTypeParameters(classSymbol, GetTypeParameterList(classDecl), GetConstraintClauses(classDecl));

        RegisterDeclaredTypeSymbol(classDecl, classSymbol);

        return classSymbol;
    }

    private void DeclareClassMemberTypes(TypeDeclarationSyntax classDecl, SourceNamedTypeSymbol classSymbol)
    {
        var objectType = Compilation.GetTypeByMetadataName("System.Object");
        var valueType = Compilation.GetSpecialType(SpecialType.System_ValueType);
        var parentType = (INamedTypeSymbol)classSymbol;

        foreach (var member in classDecl.Members)
        {
            switch (member)
            {
                case TypeDeclarationSyntax nestedClass when nestedClass is ClassDeclarationSyntax or StructDeclarationSyntax or RecordDeclarationSyntax:
                    {
                        ReportInvalidTypeModifiers(nestedClass, isNestedType: true, _declarationDiagnostics);
                        ReportRedundantTypeModifiers(nestedClass, _declarationDiagnostics);

                        var nestedTypeKind = IsStructLikeNominalType(nestedClass)
                            ? TypeKind.Struct
                            : TypeKind.Class;
                        var nestedBaseType = nestedTypeKind == TypeKind.Struct ? valueType : objectType;
                        var nestedStatic = nestedClass.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);
                        var nestedAbstract = nestedStatic || nestedClass.Modifiers.Any(m => m.Kind == SyntaxKind.AbstractKeyword);
                        var nestedSealed = nestedStatic || (!nestedClass.Modifiers.Any(m => m.Kind == SyntaxKind.OpenKeyword) && !nestedAbstract);
                        var nestedPartial = nestedClass.Modifiers.Any(m => m.Kind == SyntaxKind.PartialKeyword);
                        var nestedRecord = nestedClass is RecordDeclarationSyntax || nestedClass.Modifiers.Any(m => m.Kind == SyntaxKind.RecordKeyword);
                        var nestedAccessibility = AccessibilityUtilities.DetermineAccessibility(
                            nestedClass.Modifiers,
                            AccessibilityUtilities.GetDefaultTypeAccessibility(parentType));

                        var nestedLocation = nestedClass.GetLocation();
                        var nestedReference = nestedClass.GetReference();

                        SourceNamedTypeSymbol nestedSymbol;
                        var isNewNestedSymbol = true;

                        var existingNested = parentType.GetMembers(nestedClass.Identifier.ValueText)
                            .OfType<SourceNamedTypeSymbol>()
                            .FirstOrDefault(t => t.TypeKind == nestedTypeKind);

                        if (existingNested is not null)
                        {
                            var hadPartial = existingNested.HasPartialModifier;
                            var hadNonPartial = existingNested.HasNonPartialDeclaration;
                            var previouslyMixed = hadPartial && hadNonPartial;
                            var willBeMixed = (hadPartial || nestedPartial) && (hadNonPartial || !nestedPartial);

                            if (willBeMixed && !previouslyMixed)
                            {
                                _declarationDiagnostics.ReportPartialTypeDeclarationMissingPartial(
                                    nestedClass.Identifier.ValueText,
                                    nestedClass.Identifier.GetLocation());
                            }
                            else if (hadNonPartial && !nestedPartial)
                            {
                                _declarationDiagnostics.ReportTypeAlreadyDefined(
                                    nestedClass.Identifier.ValueText,
                                    nestedClass.Identifier.GetLocation());
                            }

                            existingNested.AddDeclaration(nestedLocation, nestedReference);
                            existingNested.UpdateDeclarationModifiers(nestedSealed, nestedAbstract, nestedStatic);
                            existingNested.RegisterPartialModifier(nestedPartial);
                            existingNested.RegisterRecordModifier(nestedRecord);

                            nestedSymbol = existingNested;
                            isNewNestedSymbol = false;
                        }
                        else
                        {
                            nestedSymbol = new SourceNamedTypeSymbol(
                                nestedClass.Identifier.ValueText,
                                nestedBaseType!,
                                nestedTypeKind,
                                parentType,
                                parentType,
                                classSymbol.ContainingNamespace,
                                [nestedLocation],
                                [nestedReference],
                                nestedSealed,
                                nestedAbstract,
                                nestedStatic,
                                declaredAccessibility: nestedAccessibility
                            );

                            nestedSymbol.RegisterPartialModifier(nestedPartial);
                            nestedSymbol.RegisterRecordModifier(nestedRecord);
                        }

                        if (isNewNestedSymbol)
                            InitializeTypeParameters(nestedSymbol, GetTypeParameterList(nestedClass), GetConstraintClauses(nestedClass));

                        RegisterDeclaredTypeSymbol(nestedClass, nestedSymbol);
                        DeclareClassMemberTypes(nestedClass, nestedSymbol);
                        break;
                    }

                case InterfaceDeclarationSyntax nestedInterface:
                    {
                        ReportInvalidTypeModifiers(nestedInterface, isNestedType: true, _declarationDiagnostics);

                        var nestedInterfaceSymbol = new SourceNamedTypeSymbol(
                            nestedInterface.Identifier.ValueText,
                            objectType!,
                            TypeKind.Interface,
                            parentType,
                            parentType,
                            classSymbol.ContainingNamespace,
                            [nestedInterface.GetLocation()],
                            [nestedInterface.GetReference()],
                            true,
                            isAbstract: true,
                            declaredAccessibility: AccessibilityUtilities.DetermineAccessibility(
                                nestedInterface.Modifiers,
                                AccessibilityUtilities.GetDefaultTypeAccessibility(parentType))
                        );

                        InitializeTypeParameters(nestedInterfaceSymbol, nestedInterface.TypeParameterList, nestedInterface.ConstraintClauses);
                        RegisterDeclaredTypeSymbol(nestedInterface, nestedInterfaceSymbol);
                        break;
                    }

                case EnumDeclarationSyntax enumDecl:
                    {
                        ReportInvalidTypeModifiers(enumDecl, isNestedType: true, _declarationDiagnostics);

                        var enumSymbol = new SourceNamedTypeSymbol(
                            enumDecl.Identifier.ValueText,
                            Compilation.GetTypeByMetadataName("System.Enum"),
                            TypeKind.Enum,
                            parentType,
                            parentType,
                            classSymbol.ContainingNamespace,
                            [enumDecl.GetLocation()],
                            [enumDecl.GetReference()],
                            true,
                            declaredAccessibility: AccessibilityUtilities.DetermineAccessibility(
                                enumDecl.Modifiers,
                                AccessibilityUtilities.GetDefaultTypeAccessibility(parentType))
                        );

                        RegisterDeclaredTypeSymbol(enumDecl, enumSymbol);
                        break;
                    }

                case UnionDeclarationSyntax nestedUnion:
                    {
                        ReportInvalidTypeModifiers(nestedUnion, isNestedType: true, _declarationDiagnostics);

                        var unionSymbol = new SourceDiscriminatedUnionSymbol(
                            nestedUnion.Identifier.ValueText,
                            Compilation.GetSpecialType(SpecialType.System_ValueType)!,
                            parentType,
                            parentType,
                            classSymbol.ContainingNamespace,
                            [nestedUnion.GetLocation()],
                            [nestedUnion.GetReference()],
                            AccessibilityUtilities.DetermineAccessibility(
                                nestedUnion.Modifiers,
                                AccessibilityUtilities.GetDefaultTypeAccessibility(parentType)));

                        InitializeTypeParameters(unionSymbol, nestedUnion.TypeParameterList, nestedUnion.ConstraintClauses);
                        RegisterDeclaredTypeSymbol(nestedUnion, unionSymbol);
                        break;
                    }

                case DelegateDeclarationSyntax delegateDecl:
                    {
                        ReportInvalidTypeModifiers(delegateDecl, isNestedType: true, _declarationDiagnostics);

                        var delegateAccessibility = AccessibilityUtilities.DetermineAccessibility(
                            delegateDecl.Modifiers,
                            AccessibilityUtilities.GetDefaultTypeAccessibility(parentType));

                        var delegateSymbol = new SourceNamedTypeSymbol(
                            delegateDecl.Identifier.ValueText,
                            Compilation.GetSpecialType(SpecialType.System_MulticastDelegate)!,
                            TypeKind.Delegate,
                            parentType,
                            parentType,
                            classSymbol.ContainingNamespace,
                            [delegateDecl.GetLocation()],
                            [delegateDecl.GetReference()],
                            isSealed: true,
                            isAbstract: true,
                            isStatic: false,
                            declaredAccessibility: delegateAccessibility);

                        InitializeTypeParameters(delegateSymbol, delegateDecl.TypeParameterList, delegateDecl.ConstraintClauses);
                        RegisterDeclaredTypeSymbol(delegateDecl, delegateSymbol);
                        break;
                    }
            }
        }
    }

    private void DeclareDelegateSymbol(DelegateDeclarationSyntax delegateDecl, INamespaceSymbol parentNamespace)
    {
        ReportInvalidTypeModifiers(delegateDecl, isNestedType: false, _declarationDiagnostics);

        ReportExternalTypeRedeclaration(
            parentNamespace,
            delegateDecl.Identifier,
            delegateDecl.TypeParameterList?.Parameters.Count ?? 0,
            _declarationDiagnostics);

        var baseType = Compilation.GetSpecialType(SpecialType.System_MulticastDelegate);

        var delegateAccessibility = AccessibilityUtilities.DetermineAccessibility(
            delegateDecl.Modifiers,
            AccessibilityUtilities.GetDefaultTypeAccessibility(parentNamespace.AsSourceNamespace()));

        var delegateSymbol = new SourceNamedTypeSymbol(
            delegateDecl.Identifier.ValueText,
            baseType!,
            TypeKind.Delegate,
            parentNamespace.AsSourceNamespace(),
            null,
            parentNamespace.AsSourceNamespace(),
            new[] { delegateDecl.GetLocation() },
            new[] { delegateDecl.GetReference() },
            isSealed: true,
            isAbstract: true,
            isStatic: false,
            declaredAccessibility: delegateAccessibility);

        InitializeTypeParameters(delegateSymbol, delegateDecl.TypeParameterList, delegateDecl.ConstraintClauses);
        RegisterDeclaredTypeSymbol(delegateDecl, delegateSymbol);
    }

    private void DeclareInterfaceSymbol(InterfaceDeclarationSyntax interfaceDecl, INamespaceSymbol parentNamespace, INamedTypeSymbol? objectType)
    {
        ReportInvalidTypeModifiers(interfaceDecl, isNestedType: false, _declarationDiagnostics);

        ReportExternalTypeRedeclaration(
            parentNamespace,
            interfaceDecl.Identifier,
            interfaceDecl.TypeParameterList?.Parameters.Count ?? 0,
            _declarationDiagnostics);

        var interfaceSymbol = new SourceNamedTypeSymbol(
            interfaceDecl.Identifier.ValueText,
            objectType!,
            TypeKind.Interface,
            parentNamespace.AsSourceNamespace(),
            null,
            parentNamespace.AsSourceNamespace(),
            new[] { interfaceDecl.GetLocation() },
            new[] { interfaceDecl.GetReference() },
            true,
            isAbstract: true,
            declaredAccessibility: AccessibilityUtilities.DetermineAccessibility(
                interfaceDecl.Modifiers,
                AccessibilityUtilities.GetDefaultTypeAccessibility(parentNamespace.AsSourceNamespace())));

        InitializeTypeParameters(interfaceSymbol, interfaceDecl.TypeParameterList, interfaceDecl.ConstraintClauses);
        RegisterDeclaredTypeSymbol(interfaceDecl, interfaceSymbol);
    }

    private void DeclareExtensionSymbol(ExtensionDeclarationSyntax extensionDecl, INamespaceSymbol parentNamespace, INamedTypeSymbol? objectType)
    {
        ReportInvalidTypeModifiers(extensionDecl, isNestedType: false, _declarationDiagnostics);

        var extensionAccessibility = AccessibilityUtilities.DetermineAccessibility(
            extensionDecl.Modifiers,
            AccessibilityUtilities.GetDefaultTypeAccessibility(parentNamespace.AsSourceNamespace()));

        if (extensionAccessibility == Accessibility.Public &&
            extensionDecl.Identifier.Kind == SyntaxKind.None)
        {
            _declarationDiagnostics.ReportPublicExtensionRequiresIdentifier(
                extensionDecl.GetLocation());
        }

        var extensionName = extensionDecl.Identifier.Kind == SyntaxKind.None
            ? MangleUnnamedExtensionName(extensionDecl)
            : extensionDecl.Identifier.ValueText;

        ReportExternalTypeRedeclaration(
            parentNamespace,
            extensionName,
            extensionDecl.GetLocation(),
            extensionDecl.TypeParameterList?.Parameters.Count ?? 0,
            _declarationDiagnostics);

        var extensionSymbol = new SourceNamedTypeSymbol(
            extensionName,
            objectType!,
            TypeKind.Class,
            parentNamespace.AsSourceNamespace(),
            null,
            parentNamespace.AsSourceNamespace(),
            new[] { extensionDecl.GetLocation() },
            new[] { extensionDecl.GetReference() },
            isSealed: true,
            isAbstract: true,
            declaredAccessibility: extensionAccessibility);

        extensionSymbol.MarkAsExtensionContainer();
        InitializeTypeParameters(extensionSymbol, extensionDecl.TypeParameterList, extensionDecl.ConstraintClauses);
        RegisterDeclaredTypeSymbol(extensionDecl, extensionSymbol);
    }

    private void DeclareEnumSymbol(EnumDeclarationSyntax enumDecl, INamespaceSymbol parentNamespace)
    {
        ReportInvalidTypeModifiers(enumDecl, isNestedType: false, _declarationDiagnostics);

        var enumAccessibility = AccessibilityUtilities.DetermineAccessibility(
            enumDecl.Modifiers,
            AccessibilityUtilities.GetDefaultTypeAccessibility(parentNamespace.AsSourceNamespace()));
        ReportExternalTypeRedeclaration(
            parentNamespace,
            enumDecl.Identifier,
            arity: 0,
            _declarationDiagnostics);

        var enumSymbol = new SourceNamedTypeSymbol(
            enumDecl.Identifier.ValueText,
            Compilation.GetTypeByMetadataName("System.Enum"),
            TypeKind.Enum,
            parentNamespace.AsSourceNamespace(),
            null,
            parentNamespace.AsSourceNamespace(),
            new[] { enumDecl.GetLocation() },
            new[] { enumDecl.GetReference() },
            true,
            declaredAccessibility: enumAccessibility
        );

        RegisterDeclaredTypeSymbol(enumDecl, enumSymbol);
    }

    private void DeclareUnionSymbol(UnionDeclarationSyntax unionDecl, INamespaceSymbol parentNamespace)
    {
        ReportInvalidTypeModifiers(unionDecl, isNestedType: false, _declarationDiagnostics);

        var declaringSymbol = (ISymbol)(parentNamespace.AsSourceNamespace() ?? parentNamespace);
        var namespaceSymbol = parentNamespace.AsSourceNamespace();
        ReportExternalTypeRedeclaration(
            parentNamespace,
            unionDecl.Identifier,
            unionDecl.TypeParameterList?.Parameters.Count ?? 0,
            _declarationDiagnostics);

        var unionSymbol = new SourceDiscriminatedUnionSymbol(
            unionDecl.Identifier.ValueText,
            Compilation.GetSpecialType(SpecialType.System_ValueType)!,
            declaringSymbol,
            null,
            namespaceSymbol,
            [unionDecl.GetLocation()],
            [unionDecl.GetReference()],
            AccessibilityUtilities.DetermineAccessibility(
                unionDecl.Modifiers,
                AccessibilityUtilities.GetDefaultTypeAccessibility(declaringSymbol)));

        InitializeTypeParameters(unionSymbol, unionDecl.TypeParameterList, unionDecl.ConstraintClauses);
        RegisterDeclaredTypeSymbol(unionDecl, unionSymbol);
    }

    private Binder BindCompilationUnit(CompilationUnitSyntax cu, Binder parentBinder)
    {
        // Step 1: Resolve namespace
        var fileScopedNamespace = cu.Members.OfType<FileScopedNamespaceDeclarationSyntax>().FirstOrDefault();
        INamespaceSymbol targetNamespace;
        NamespaceBinder namespaceBinder;

        if (fileScopedNamespace != null)
        {
            targetNamespace = Compilation.GetOrCreateNamespaceSymbol(fileScopedNamespace.Name.ToString())
                             ?? throw new Exception("Namespace not found");

            namespaceBinder = new NamespaceBinder(parentBinder, targetNamespace);
            parentBinder = namespaceBinder;
        }
        else
        {
            targetNamespace = Compilation.GlobalNamespace;
            namespaceBinder = new NamespaceBinder(parentBinder, targetNamespace);
            parentBinder = namespaceBinder;
        }

        // Step 2: Handle imports
        var namespaceImports = new List<INamespaceOrTypeSymbol>();
        var typeImports = new List<ITypeSymbol>();
        var aliases = new Dictionary<string, IReadOnlyList<IAliasSymbol>>();
        var deferredWildcardImports = new List<NameSyntax>();

        var provisionalImportBinder = new ImportBinder(namespaceBinder, namespaceImports, typeImports, aliases);

        foreach (var import in cu.DescendantNodes().OfType<ImportDirectiveSyntax>())
        {
            var name = import.Name.ToString();

            if (IsWildcard(import.Name, out var nsName))
            {
                INamespaceOrTypeSymbol? nsImport =
                    (INamespaceOrTypeSymbol?)ResolveNamespace(targetNamespace, nsName.ToString())
                    ?? ResolveType(targetNamespace, nsName.ToString());
                if (nsImport != null)
                {
                    namespaceImports.Add(nsImport);
                }
                else
                {
                    deferredWildcardImports.Add(nsName);
                }

                continue;
            }

            var nsSymbol = ResolveNamespace(targetNamespace, name);
            if (nsSymbol != null)
            {
                namespaceBinder.Diagnostics.ReportTypeExpectedWithoutWildcard(import.Name.GetLocation());
                continue;
            }

            ITypeSymbol? typeSymbol = HasTypeArguments(import.Name)
                ? ResolveOpenGenericType(targetNamespace, import.Name)
                : ResolveType(targetNamespace, name);

            if (typeSymbol != null)
            {
                typeImports.Add(typeSymbol);
            }
            else
            {
                namespaceBinder.Diagnostics.ReportInvalidImportTarget(import.Name.GetLocation());
            }
        }

        var importBinder = new ImportBinder(namespaceBinder, namespaceImports, typeImports, aliases);

        parentBinder = importBinder;

        var compilationUnitBinder = new CompilationUnitBinder(parentBinder, this);
        BindNamespaceMembers(cu, compilationUnitBinder, targetNamespace);

        foreach (var baseName in deferredWildcardImports)
        {
            INamespaceOrTypeSymbol? resolved =
                (INamespaceOrTypeSymbol?)ResolveNamespace(targetNamespace, baseName.ToString())
                ?? ResolveType(targetNamespace, baseName.ToString());

            if (resolved != null)
            {
                namespaceImports.Add(resolved);
            }
            else
            {
                namespaceBinder.Diagnostics.ReportInvalidImportTarget(baseName.GetLocation());
            }
        }

        BindAliases(cu.Aliases);

        if (fileScopedNamespace is not null)
        {
            BindAliases(fileScopedNamespace.Aliases);
        }

        foreach (var diagnostic in namespaceBinder.Diagnostics.AsEnumerable())
            importBinder.Diagnostics.Report(diagnostic);

        foreach (var diagnostic in compilationUnitBinder.Diagnostics.AsEnumerable())
            importBinder.Diagnostics.Report(diagnostic);

        var topLevelBinder = CreateTopLevelBinder(cu, targetNamespace, importBinder);

        foreach (var diagnostic in importBinder.Diagnostics.AsEnumerable())
            topLevelBinder.Diagnostics.Report(diagnostic);

        CacheBinder(cu, topLevelBinder);
        if (fileScopedNamespace != null)
            CacheBinder(fileScopedNamespace, importBinder);

        return topLevelBinder;

        void BindAliases(SyntaxList<AliasDirectiveSyntax> aliasList)
        {
            foreach (var alias in aliasList)
            {
                IReadOnlyList<ISymbol> symbols;
                if (alias.Target is NameSyntax name)
                {
                    symbols = ResolveAlias(targetNamespace, name);
                }
                else
                {
                    var typeSymbol = TryResolveTypeSyntaxSilently(alias.Target);
                    symbols = typeSymbol is null
                        ? Array.Empty<ISymbol>()
                        : new ISymbol[] { typeSymbol };
                }

                if (symbols.Count > 0)
                {
                    var aliasName = alias.Identifier.ValueText;
                    var aliasSymbols = symbols
                        .Select(s => AliasSymbolFactory.Create(aliasName, s))
                        .ToArray();
                    aliases[aliasName] = aliasSymbols;
                }
                else
                {
                    namespaceBinder.Diagnostics.ReportInvalidAliasType(alias.Target.GetLocation());
                }
            }
        }

        INamespaceSymbol? ResolveNamespace(INamespaceSymbol current, string name)
        {
            var full = Combine(current, name);
            return Compilation.GetNamespaceSymbol(full) ?? Compilation.GetNamespaceSymbol(name);
        }

        ITypeSymbol? ResolveType(INamespaceSymbol current, string name)
        {
            var full = Combine(current, name);
            return Compilation.GetTypeByMetadataName(full) ?? Compilation.GetTypeByMetadataName(name);
        }

        IReadOnlyList<ISymbol> ResolveAlias(INamespaceSymbol current, NameSyntax name)
        {
            var nsSymbol = ResolveNamespace(current, name.ToString());
            if (nsSymbol != null)
                return [nsSymbol];

            ITypeSymbol? typeSymbol = HasTypeArguments(name)
                ? ResolveGenericType(current, name)
                : ResolveType(current, name.ToString());
            if (typeSymbol != null)
                return [typeSymbol];

            if (name is QualifiedNameSyntax qn)
            {
                var memberName = GetRightmostIdentifier(name);
                var left = qn.Left;

                ITypeSymbol? containingType = HasTypeArguments(left)
                    ? ResolveGenericType(current, left)
                    : ResolveType(current, left.ToString());

                if (containingType != null)
                {
                    var members = containingType.GetMembers(memberName)
                        .Where(m => m.IsStatic)
                        .ToArray();

                    if (qn.Right is GenericNameSyntax genericRight &&
                        TryResolveTypeArgumentsSilently(genericRight.TypeArgumentList, out var memberTypeArguments))
                    {
                        var closedTypeMembers = members
                            .OfType<INamedTypeSymbol>()
                            .Where(t => t.Arity == memberTypeArguments.Length)
                            .Select(t => t.Construct(memberTypeArguments))
                            .Cast<ISymbol>()
                            .ToArray();

                        if (closedTypeMembers.Length > 0)
                            return closedTypeMembers;
                    }

                    if (members.Length > 0)
                        return members;
                }
            }

            return Array.Empty<ISymbol>();
        }

        static bool HasTypeArguments(NameSyntax nameSyntax)
            => nameSyntax.DescendantNodes().OfType<GenericNameSyntax>().Any();

        ITypeSymbol? ResolveGenericType(INamespaceSymbol current, NameSyntax name)
        {
            if (name is GenericNameSyntax g)
            {
                var baseName = $"{g.Identifier.ValueText}`{g.TypeArgumentList.Arguments.Count}";
                var full = Combine(current, baseName);
                var unconstructed = (INamedTypeSymbol?)Compilation.GetTypeByMetadataName(full)
                    ?? (INamedTypeSymbol?)Compilation.GetTypeByMetadataName(baseName);
                if (unconstructed is null)
                    return null;

                if (!TryResolveTypeArgumentsSilently(g.TypeArgumentList, out var args))
                    return null;

                return Compilation.ConstructGenericType(unconstructed, args);
            }

            if (name is QualifiedNameSyntax { Right: GenericNameSyntax gen })
            {
                var leftName = ((QualifiedNameSyntax)name).Left.ToString();
                var baseName = $"{leftName}.{gen.Identifier.ValueText}`{gen.TypeArgumentList.Arguments.Count}";
                var full = Combine(current, baseName);
                var unconstructed = (INamedTypeSymbol?)Compilation.GetTypeByMetadataName(full)
                    ?? (INamedTypeSymbol?)Compilation.GetTypeByMetadataName(baseName);
                if (unconstructed is null)
                    return null;

                if (!TryResolveTypeArgumentsSilently(gen.TypeArgumentList, out var args))
                    return null;

                return Compilation.ConstructGenericType(unconstructed, args);
            }

            return null;
        }

        ITypeSymbol? ResolveOpenGenericType(INamespaceSymbol current, NameSyntax name)
        {
            if (name is GenericNameSyntax g)
            {
                var baseName = $"{g.Identifier.ValueText}`{g.TypeArgumentList.Arguments.SeparatorCount + 1}";
                var full = Combine(current, baseName);
                var unconstructed = (INamedTypeSymbol?)Compilation.GetTypeByMetadataName(full)
                    ?? (INamedTypeSymbol?)Compilation.GetTypeByMetadataName(baseName);
                if (unconstructed is null)
                    return null;

                if (!TryResolveTypeArgumentsSilently(g.TypeArgumentList, out var args))
                    return null;

                return Compilation.ConstructGenericType(unconstructed, args);
            }

            if (name is QualifiedNameSyntax { Right: GenericNameSyntax gen })
            {
                var leftName = ((QualifiedNameSyntax)name).Left.ToString();
                var baseName = $"{leftName}.{gen.Identifier.ValueText}`{gen.TypeArgumentList.Arguments.SeparatorCount + 1}";
                var full = Combine(current, baseName);
                var unconstructed = (INamedTypeSymbol?)Compilation.GetTypeByMetadataName(full)
                    ?? (INamedTypeSymbol?)Compilation.GetTypeByMetadataName(baseName);
                if (unconstructed is not null)
                    return unconstructed;
            }

            return null;
        }

        ITypeSymbol? TryResolveTypeSyntaxSilently(TypeSyntax syntax)
        {
            using (provisionalImportBinder.Diagnostics.CreateNonReportingScope())
            {
                var result = provisionalImportBinder.BindTypeSyntax(syntax);
                if (result.Success)
                    return result.ResolvedType;

                var legacy = provisionalImportBinder.BindTypeSyntaxDirect(syntax);
                return legacy.TypeKind == TypeKind.Error ? null : legacy;
            }
        }

        bool TryResolveTypeArgumentsSilently(TypeArgumentListSyntax list, out ITypeSymbol[] args)
        {
            args = new ITypeSymbol[list.Arguments.Count];

            for (var i = 0; i < list.Arguments.Count; i++)
            {
                var resolved = TryResolveTypeSyntaxSilently(list.Arguments[i].Type);
                if (resolved is null)
                    return false;

                args[i] = resolved;
            }

            return true;
        }

        static string Combine(INamespaceSymbol current, string name)
        {
            var currentName = current.ToString();
            return string.IsNullOrEmpty(currentName) ? name : currentName + "." + name;
        }

        static string GetRightmostIdentifier(NameSyntax nameSyntax)
        {
            return nameSyntax switch
            {
                IdentifierNameSyntax id => id.Identifier.ValueText,
                QualifiedNameSyntax qn => GetRightmostIdentifier(qn.Right),
                _ => nameSyntax.ToString()
            };
        }

        static bool IsWildcard(NameSyntax nameSyntax, out NameSyntax baseName)
        {
            if (nameSyntax is QualifiedNameSyntax { Right: WildcardNameSyntax, Left: var left })
            {
                baseName = left;
                return true;
            }

            baseName = default!;
            return false;
        }
    }

    private Binder CreateTopLevelBinder(CompilationUnitSyntax cu, INamespaceSymbol namespaceSymbol, Binder parentBinder)
    {
        var fileScopedNamespace = cu.Members.OfType<FileScopedNamespaceDeclarationSyntax>().FirstOrDefault();

        var bindableGlobals = Compilation.CollectBindableGlobalStatements(cu);
        var hasNonGlobalMembers = Compilation.HasNonGlobalMembers(cu);
        var hadDisabledGlobalStatements = false;

        if (!Compilation.Options.AllowGlobalStatements && bindableGlobals.Count > 0)
        {
            foreach (var statement in bindableGlobals)
                parentBinder.Diagnostics.Report(Diagnostic.Create(s_globalStatementsDisabled, statement.GetLocation()));

            bindableGlobals = [];
            hadDisabledGlobalStatements = true;
        }

        var topLevelMainFunctions = bindableGlobals
            .Where(static g => g.Statement is FunctionStatementSyntax { Identifier.ValueText: "Main" })
            .ToArray();
        var hasTopLevelFunctionDeclarations = bindableGlobals.Any(static g => g.Statement is FunctionStatementSyntax);

        if (topLevelMainFunctions.Length > 0)
        {
            foreach (var statement in bindableGlobals)
            {
                if (statement.Statement is FunctionStatementSyntax { Identifier.ValueText: "Main" })
                    continue;

                if (statement.Statement is FunctionStatementSyntax)
                    continue;

                parentBinder.Diagnostics.ReportTopLevelStatementsDisallowedWithMainFunction(statement.GetLocation());
            }
        }

        var supportsTopLevelProgram = Compilation.Options.OutputKind == OutputKind.ConsoleApplication
            || hasTopLevelFunctionDeclarations;

        var shouldCreateTopLevelProgram = supportsTopLevelProgram
            && (bindableGlobals.Count > 0
            || (bindableGlobals.Count == 0
                && !hasNonGlobalMembers
                && !hadDisabledGlobalStatements
                && ShouldCreateImplicitTopLevelProgramForCompilationUnit(cu)));
        var hasExecutableFileScopedCode = bindableGlobals.Any(static g => g.Statement is not FunctionStatementSyntax);

        void CheckOrder(SyntaxList<MemberDeclarationSyntax> members)
        {
            var seenTypeDeclaration = false;
            foreach (var member in members)
            {
                if (member is GlobalStatementSyntax gs)
                {
                    if (seenTypeDeclaration)
                        parentBinder.Diagnostics.ReportFileScopedCodeOutOfOrder(gs.GetLocation());
                }
                else if (IsTypeDeclarationMember(member))
                {
                    seenTypeDeclaration = true;
                }
            }
        }

        static bool IsTypeDeclarationMember(MemberDeclarationSyntax member)
            => member is BaseTypeDeclarationSyntax or DelegateDeclarationSyntax;

        if (fileScopedNamespace != null)
        {
            foreach (var member in cu.Members)
            {
                if (member == fileScopedNamespace)
                    break;

                parentBinder.Diagnostics.ReportFileScopedNamespaceOutOfOrder(member.GetLocation());
            }

            CheckOrder(fileScopedNamespace.Members);
        }
        else
        {
            CheckOrder(cu.Members);
        }

        if (hasExecutableFileScopedCode)
        {
            var firstExecutableGlobal = bindableGlobals.First(static g => g.Statement is not FunctionStatementSyntax);

            if (Compilation.Options.OutputKind != OutputKind.ConsoleApplication)
                parentBinder.Diagnostics.ReportFileScopedCodeRequiresConsole(firstExecutableGlobal.GetLocation());

            if (Compilation.SyntaxTreeWithFileScopedCode is null)
                Compilation.SyntaxTreeWithFileScopedCode = cu.SyntaxTree;
            else if (Compilation.SyntaxTreeWithFileScopedCode != cu.SyntaxTree)
                parentBinder.Diagnostics.ReportFileScopedCodeMultipleFiles(firstExecutableGlobal.GetLocation());
        }

        if (!shouldCreateTopLevelProgram)
            return parentBinder;

        var (programClass, mainMethod, asyncImplementation) = Compilation.GetOrCreateTopLevelProgram(
            cu,
            namespaceSymbol.AsSourceNamespace()!,
            bindableGlobals);

        var scriptMethod = asyncImplementation is { } asyncMain
            ? (SourceMethodSymbol)asyncMain
            : mainMethod;

        var topLevelBinder = new TopLevelBinder(parentBinder, this, scriptMethod, mainMethod, cu);

        // Cache the compilation unit and its global statements before binding.
        //
        // Binding the statements will request declared symbols, which rely on
        // binder lookup through SemanticModel.GetBinder. Without priming the
        // cache, any attempt to resolve the compilation unit binder re-enters
        // BindCompilationUnit, causing unbounded recursion (see stack trace in
        // bug report). By caching eagerly we guarantee re-entrant lookups
        // retrieve the partially constructed top-level binder instead of
        // rebuilding it.
        CacheBinder(cu, topLevelBinder);

        foreach (var stmt in bindableGlobals)
            CacheBinder(stmt, topLevelBinder);

        topLevelBinder.BindGlobalStatements(bindableGlobals);

        return topLevelBinder;

        bool ShouldCreateImplicitTopLevelProgramForCompilationUnit(CompilationUnitSyntax current)
        {
            if (Compilation.SyntaxTreeWithFileScopedCode is { } selected)
                return selected == current.SyntaxTree;

            var firstEligibleSyntaxTree = Compilation.SyntaxTrees
                .Select(tree => tree.GetRoot() as CompilationUnitSyntax)
                .FirstOrDefault(root =>
                    root is not null
                    && Compilation.CollectBindableGlobalStatements(root).Count == 0
                    && !Compilation.HasNonGlobalMembers(root))
                ?.SyntaxTree;

            return firstEligibleSyntaxTree == current.SyntaxTree;
        }
    }

    private static string MangleUnnamedExtensionName(ExtensionDeclarationSyntax extensionDecl)
    {
        // Stable, deterministic mangling for unnamed extensions.
        // The name is not intended to be user-visible; it exists to provide a unique symbol identity
        // within the assembly for binding, caching, and metadata emission.
        var loc = extensionDecl.GetLocation();
        var span = loc.SourceSpan;

        var filePath = extensionDecl.SyntaxTree?.FilePath ?? string.Empty;

        // FNV-1a 32-bit hash for stability across processes/runs.
        unchecked
        {
            uint hash = 2166136261;
            foreach (var ch in filePath)
            {
                hash ^= ch;
                hash *= 16777619;
            }

            // Include span to avoid collisions within the same file.
            hash ^= (uint)span.Start;
            hash *= 16777619;
            hash ^= (uint)span.Length;
            hash *= 16777619;

            return $"__ext${hash:x8}_{span.Start}_{span.Length}";
        }
    }

    private void ReportExternalTypeRedeclaration(
        INamespaceSymbol? namespaceSymbol,
        string name,
        Location location,
        int arity,
        DiagnosticBag diagnostics)
    {
        var mergedNamespace = GetMergedNamespace(namespaceSymbol);
        if (mergedNamespace is null)
            return;

        foreach (var member in mergedNamespace.GetMembers(name).OfType<INamedTypeSymbol>())
        {
            if (member.Arity == arity && member.ContainingAssembly != Compilation.Assembly)
            {
                diagnostics.ReportTypeAlreadyDefined(name, location);
                break;
            }
        }
    }

    private void BindNamespaceMembers(SyntaxNode containerNode, Binder parentBinder, INamespaceSymbol parentNamespace)
    {
        var classBinders = new List<(TypeDeclarationSyntax Syntax, ClassDeclarationBinder Binder)>();
        var interfaceBinders = new List<(InterfaceDeclarationSyntax Syntax, InterfaceDeclarationBinder Binder)>();
        var extensionBinders = new List<(ExtensionDeclarationSyntax Syntax, ExtensionDeclarationBinder Binder)>();
        var unionBinders = new List<(UnionDeclarationSyntax Syntax, UnionDeclarationBinder Binder, SourceDiscriminatedUnionSymbol Symbol)>();

        var objectType = Compilation.GetTypeByMetadataName("System.Object");

        foreach (var member in containerNode.ChildNodes())
        {
            switch (member)
            {
                case BaseNamespaceDeclarationSyntax nsDecl:
                    {
                        var namespaceName = nsDecl is FileScopedNamespaceDeclarationSyntax
                            ? nsDecl.Name.ToString()
                            : parentNamespace.QualifyName(nsDecl.Name.ToString());

                        var nsSymbol = Compilation.GetOrCreateNamespaceSymbol(namespaceName);

                        var nsBinder = Compilation.BinderFactory.GetBinder(nsDecl, parentBinder)!;
                        CacheBinder(nsDecl, nsBinder);

                        BindNamespaceMembers(nsDecl, nsBinder, nsSymbol);
                        break;
                    }

                case TypeDeclarationSyntax typeDecl when IsNominalTypeDeclaration(typeDecl):
                    {
                        BindNominalTypeDeclaration(typeDecl, parentBinder, objectType, classBinders);
                        break;
                    }

                case UnionDeclarationSyntax unionDecl:
                    {
                        var declaringSymbol = (ISymbol)(parentNamespace.AsSourceNamespace() ?? parentNamespace);
                        var namespaceSymbol = parentNamespace.AsSourceNamespace();
                        var unionSymbol = (SourceDiscriminatedUnionSymbol)GetDeclaredTypeSymbol(unionDecl);
                        var (unionBinder, resolvedSymbol) = RegisterUnionDeclaration(
                            unionDecl,
                            parentBinder,
                            declaringSymbol,
                            namespaceSymbol,
                            unionSymbol);
                        unionBinders.Add((unionDecl, unionBinder, resolvedSymbol));
                        break;
                    }

                case InterfaceDeclarationSyntax interfaceDecl:
                    {
                        var interfaceSymbol = GetDeclaredTypeSymbol(interfaceDecl);
                        ImmutableArray<INamedTypeSymbol> interfaceList = ImmutableArray<INamedTypeSymbol>.Empty;

                        if (interfaceDecl.BaseList is not null)
                        {
                            var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
                            foreach (var t in interfaceDecl.BaseList.Types)
                            {
                                if (parentBinder.TryResolveNamedTypeFromTypeSyntax(t, out var resolved) &&
                                    resolved is not null &&
                                    resolved.TypeKind == TypeKind.Interface)
                                    builder.Add(resolved);
                            }

                            if (builder.Count > 0)
                                interfaceList = builder.ToImmutable();
                        }

                        if (!interfaceList.IsDefaultOrEmpty)
                            interfaceSymbol.SetInterfaces(MergeInterfaceSets(interfaceSymbol.Interfaces, interfaceList));

                        var interfaceBinder = new InterfaceDeclarationBinder(parentBinder, interfaceSymbol, interfaceDecl);
                        interfaceBinder.EnsureTypeParameterConstraintTypesResolved(interfaceSymbol.TypeParameters);
                        CacheBinder(interfaceDecl, interfaceBinder);
                        interfaceBinders.Add((interfaceDecl, interfaceBinder));
                        break;
                    }

                case ExtensionDeclarationSyntax extensionDecl:
                    {
                        var extensionSymbol = GetDeclaredTypeSymbol(extensionDecl);
                        var extensionBinder = new ExtensionDeclarationBinder(parentBinder, extensionSymbol, extensionDecl);
                        extensionBinder.EnsureTypeParameterConstraintTypesResolved(extensionSymbol.TypeParameters);
                        CacheBinder(extensionDecl, extensionBinder);

                        extensionBinders.Add((extensionDecl, extensionBinder));
                        break;
                    }

                case DelegateDeclarationSyntax delegateDecl:
                    {
                        var delegateSymbol = GetDeclaredTypeSymbol(delegateDecl);

                        // Binder for attributes/type parameter constraints
                        var delegateBinder = new DelegateDeclarationBinder(parentBinder, delegateSymbol, delegateDecl);
                        delegateBinder.EnsureTypeParameterConstraintTypesResolved(delegateSymbol.TypeParameters);
                        CacheBinder(delegateDecl, delegateBinder);

                        EnsureDelegateMembers(delegateSymbol, delegateDecl, delegateBinder);

                        break;
                    }

                case EnumDeclarationSyntax enumDecl:
                    {
                        var enumSymbol = GetDeclaredTypeSymbol(enumDecl);
                        var enumBinder = new EnumDeclarationBinder(parentBinder, enumSymbol, enumDecl);
                        CacheBinder(enumDecl, enumBinder);

                        var enumUnderlyingType = ResolveEnumUnderlyingType(enumDecl, parentBinder);
                        enumSymbol.SetEnumUnderlyingType(enumUnderlyingType);

                        RegisterEnumMembers(enumDecl, enumBinder, enumSymbol, parentNamespace.AsSourceNamespace());
                        break;
                    }
            }
        }

        DiscoverSameFileSealedHierarchySubtypes(classBinders);
        ValidatePermitsListEntries(classBinders);

        foreach (var (unionDecl, unionBinder, unionSymbol) in unionBinders)
            RegisterUnionCases(unionDecl, unionBinder, unionSymbol);

        foreach (var (classDecl, classBinder) in classBinders)
        {
            RegisterClassMembers(classDecl, classBinder);
            classBinder.EnsureDefaultConstructor();
        }

        var checkedClasses = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);
        foreach (var (classDecl, classBinder) in classBinders)
        {
            var classSymbol = (INamedTypeSymbol)classBinder.ContainingSymbol;
            if (!checkedClasses.Add(classSymbol))
                continue;

            ReportMissingAbstractBaseMembers(classSymbol, classDecl, classBinder.Diagnostics);
        }

        foreach (var (interfaceDecl, interfaceBinder) in interfaceBinders)
            RegisterInterfaceMembers(interfaceDecl, interfaceBinder);

        foreach (var (extensionDecl, extensionBinder) in extensionBinders)
            RegisterExtensionMembers(extensionDecl, extensionBinder);

    }

    private void BindNominalTypeDeclaration(
        TypeDeclarationSyntax declaration,
        Binder parentBinder,
        INamedTypeSymbol? objectType,
        List<(TypeDeclarationSyntax Syntax, ClassDeclarationBinder Binder)> classBinders)
    {
        var typeSymbol = GetDeclaredTypeSymbol(declaration);
        var declarationBinder = new ClassDeclarationBinder(parentBinder, typeSymbol, declaration);
        declarationBinder.EnsureTypeParameterConstraintTypesResolved(typeSymbol.TypeParameters);

        var valueType = Compilation.GetSpecialType(SpecialType.System_ValueType);
        var defaultBaseType = GetDefaultBaseTypeForNominalDeclaration(declaration, objectType, valueType);
        var defaultInterfaces = GetDefaultNominalInterfaces(typeSymbol);
        var shape = declarationBinder.BindNominalTypeShape(declaration, defaultBaseType, defaultInterfaces);
        var baseTypeSymbol = shape.BaseType;
        var interfaceList = shape.Interfaces;

        if (baseTypeSymbol is not null &&
            !SymbolEqualityComparer.Default.Equals(typeSymbol.BaseType, baseTypeSymbol) &&
            SymbolEqualityComparer.Default.Equals(typeSymbol.BaseType, defaultBaseType))
        {
            typeSymbol.SetBaseType(baseTypeSymbol);
        }

        if (!interfaceList.IsDefaultOrEmpty)
            typeSymbol.SetInterfaces(MergeInterfaceSets(typeSymbol.Interfaces, interfaceList));

        CacheBinder(declaration, declarationBinder);
        RegisterClassSymbol(declaration, typeSymbol);

        if (typeSymbol.IsSealedHierarchy)
            ResolveSealedHierarchyPermits(declaration, typeSymbol, declarationBinder);

        classBinders.Add((declaration, declarationBinder));
    }

    private static bool IsNominalTypeDeclaration(TypeDeclarationSyntax declaration)
        => declaration is ClassDeclarationSyntax or RecordDeclarationSyntax or StructDeclarationSyntax;

    private void ValidatePermitsListEntries(
        List<(TypeDeclarationSyntax Syntax, ClassDeclarationBinder Binder)> classBinders)
    {
        foreach (var (classDecl, classBinder) in classBinders)
        {
            var typeSymbol = (SourceNamedTypeSymbol)classBinder.ContainingSymbol;
            if (!typeSymbol.IsSealedHierarchy || !typeSymbol.HasExplicitPermits)
                continue;

            PermitsClauseSyntax? permitsClause = classDecl switch
            {
                ClassDeclarationSyntax classDeclarationSyntax => classDeclarationSyntax.PermitsClause,
                RecordDeclarationSyntax recordDeclarationSyntax => recordDeclarationSyntax.PermitsClause,
                _ => null,
            };

            if (permitsClause is null)
                continue;

            var typeIndex = 0;
            foreach (var typeSyntax in permitsClause.Types)
            {
                if (typeIndex >= typeSymbol.PermittedDirectSubtypes.Length)
                    break;

                var permitted = typeSymbol.PermittedDirectSubtypes[typeIndex];
                typeIndex++;

                if (permitted.BaseType is null ||
                    !SymbolEqualityComparer.Default.Equals(permitted.BaseType, typeSymbol))
                {
                    _declarationDiagnostics.ReportPermitsTypeNotDirectSubtype(
                        permitted.Name,
                        typeSymbol.Name,
                        typeSyntax.GetLocation());
                }
            }
        }
    }

    private void DiscoverSameFileSealedHierarchySubtypes(
        List<(TypeDeclarationSyntax Syntax, ClassDeclarationBinder Binder)> classBinders)
    {
        foreach (var (classDecl, classBinder) in classBinders)
        {
            var typeSymbol = (SourceNamedTypeSymbol)classBinder.ContainingSymbol;
            if (!typeSymbol.IsSealedHierarchy || typeSymbol.HasExplicitPermits)
                continue;

            var sealedFile = typeSymbol.SealedHierarchySourceFile;
            var directSubtypes = ImmutableArray.CreateBuilder<INamedTypeSymbol>();

            foreach (var (otherDecl, otherBinder) in classBinders)
            {
                var otherSymbol = (SourceNamedTypeSymbol)otherBinder.ContainingSymbol;
                if (ReferenceEquals(otherSymbol, typeSymbol))
                    continue;

                if (otherSymbol.BaseType is null)
                    continue;

                if (!SymbolEqualityComparer.Default.Equals(otherSymbol.BaseType, typeSymbol))
                    continue;

                if (string.Equals(otherDecl.SyntaxTree?.FilePath, sealedFile, StringComparison.Ordinal))
                    directSubtypes.Add(otherSymbol);
            }

            typeSymbol.SetPermittedDirectSubtypes(directSubtypes.ToImmutable());
        }
    }

    private void ResolveSealedHierarchyPermits(
        TypeDeclarationSyntax declaration,
        SourceNamedTypeSymbol typeSymbol,
        Binder declarationBinder)
    {
        PermitsClauseSyntax? permitsClause = declaration switch
        {
            ClassDeclarationSyntax classDecl => classDecl.PermitsClause,
            RecordDeclarationSyntax recordDecl => recordDecl.PermitsClause,
            _ => null,
        };

        if (permitsClause is null)
            return;

        var permitted = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
        var seen = new HashSet<string>(StringComparer.Ordinal);

        foreach (var typeSyntax in permitsClause.Types)
        {
            if (!declarationBinder.TryResolveNamedTypeFromTypeSyntax(typeSyntax, out var resolved) || resolved is null)
            {
                _declarationDiagnostics.ReportPermitsTypeNotFound(
                    typeSyntax.ToString(),
                    typeSyntax.GetLocation());
                continue;
            }

            if (!seen.Add(resolved.Name))
            {
                _declarationDiagnostics.ReportPermitsTypeDuplicate(
                    resolved.Name,
                    typeSyntax.GetLocation());
                continue;
            }

            permitted.Add(resolved);
        }

        typeSymbol.SetPermittedDirectSubtypes(permitted.ToImmutable());
    }

    private static void ReportRedundantTypeModifiers(
        TypeDeclarationSyntax declaration,
        DiagnosticBag diagnostics)
    {
        if (declaration is not (ClassDeclarationSyntax or RecordDeclarationSyntax))
            return;

        var hasAbstractModifier = declaration.Modifiers.Any(static m => m.Kind == SyntaxKind.AbstractKeyword);
        if (!hasAbstractModifier)
            return;

        var hasOpenModifier = declaration.Modifiers.Any(static m => m.Kind == SyntaxKind.OpenKeyword);
        if (hasOpenModifier)
        {
            var openModifier = declaration.Modifiers.First(static m => m.Kind == SyntaxKind.OpenKeyword);
            diagnostics.ReportOpenModifierRedundantOnAbstractClass(
                declaration.Identifier.ValueText,
                openModifier.GetLocation());
        }

        var hasSealedModifier = declaration.Modifiers.Any(static m => m.Kind == SyntaxKind.SealedKeyword);
        if (!hasSealedModifier)
            return;

        var abstractModifier = declaration.Modifiers.First(static m => m.Kind == SyntaxKind.AbstractKeyword);
        diagnostics.ReportAbstractModifierRedundantOnSealedClass(
            declaration.Identifier.ValueText,
            abstractModifier.GetLocation());
    }

    private static void ReportInvalidTypeModifiers(
        MemberDeclarationSyntax declaration,
        bool isNestedType,
        DiagnosticBag diagnostics)
    {
        var (typeName, typeKind, modifiers, allowed) = declaration switch
        {
            ClassDeclarationSyntax classDecl => (
                classDecl.Identifier.ValueText,
                "class",
                classDecl.Modifiers,
                new HashSet<SyntaxKind>
                {
                    SyntaxKind.PublicKeyword,
                    SyntaxKind.PrivateKeyword,
                    SyntaxKind.InternalKeyword,
                    SyntaxKind.ProtectedKeyword,
                    SyntaxKind.StaticKeyword,
                    SyntaxKind.AbstractKeyword,
                    SyntaxKind.SealedKeyword,
                    SyntaxKind.PartialKeyword,
                    SyntaxKind.OpenKeyword,
                }),
            RecordDeclarationSyntax recordDecl => (
                recordDecl.Identifier.ValueText,
                "record",
                recordDecl.Modifiers,
                new HashSet<SyntaxKind>
                {
                    SyntaxKind.PublicKeyword,
                    SyntaxKind.PrivateKeyword,
                    SyntaxKind.InternalKeyword,
                    SyntaxKind.ProtectedKeyword,
                    SyntaxKind.StaticKeyword,
                    SyntaxKind.AbstractKeyword,
                    SyntaxKind.SealedKeyword,
                    SyntaxKind.PartialKeyword,
                    SyntaxKind.OpenKeyword,
                    SyntaxKind.RecordKeyword,
                }),
            StructDeclarationSyntax structDecl => (
                structDecl.Identifier.ValueText,
                "struct",
                structDecl.Modifiers,
                new HashSet<SyntaxKind>
                {
                    SyntaxKind.PublicKeyword,
                    SyntaxKind.PrivateKeyword,
                    SyntaxKind.InternalKeyword,
                    SyntaxKind.ProtectedKeyword,
                    SyntaxKind.PartialKeyword,
                }),
            InterfaceDeclarationSyntax interfaceDecl => (
                interfaceDecl.Identifier.ValueText,
                "interface",
                interfaceDecl.Modifiers,
                new HashSet<SyntaxKind>
                {
                    SyntaxKind.PublicKeyword,
                    SyntaxKind.PrivateKeyword,
                    SyntaxKind.InternalKeyword,
                    SyntaxKind.ProtectedKeyword,
                    SyntaxKind.PartialKeyword,
                }),
            EnumDeclarationSyntax enumDecl => (
                enumDecl.Identifier.ValueText,
                "enum",
                enumDecl.Modifiers,
                new HashSet<SyntaxKind>
                {
                    SyntaxKind.PublicKeyword,
                    SyntaxKind.PrivateKeyword,
                    SyntaxKind.InternalKeyword,
                    SyntaxKind.ProtectedKeyword,
                }),
            DelegateDeclarationSyntax delegateDecl => (
                delegateDecl.Identifier.ValueText,
                "delegate",
                delegateDecl.Modifiers,
                new HashSet<SyntaxKind>
                {
                    SyntaxKind.PublicKeyword,
                    SyntaxKind.PrivateKeyword,
                    SyntaxKind.InternalKeyword,
                    SyntaxKind.ProtectedKeyword,
                }),
            ExtensionDeclarationSyntax extensionDecl => (
                extensionDecl.Identifier.ValueText,
                "extension",
                extensionDecl.Modifiers,
                new HashSet<SyntaxKind>
                {
                    SyntaxKind.PublicKeyword,
                    SyntaxKind.PrivateKeyword,
                    SyntaxKind.InternalKeyword,
                    SyntaxKind.ProtectedKeyword,
                }),
            UnionDeclarationSyntax unionDecl => (
                unionDecl.Identifier.ValueText,
                "union",
                unionDecl.Modifiers,
                new HashSet<SyntaxKind>
                {
                    SyntaxKind.PublicKeyword,
                    SyntaxKind.PrivateKeyword,
                    SyntaxKind.InternalKeyword,
                    SyntaxKind.ProtectedKeyword,
                }),
            _ => (null, null, default, null),
        };

        if (typeName is null || typeKind is null || allowed is null)
            return;

        foreach (var modifier in modifiers)
        {
            var kind = modifier.Kind;
            var text = modifier.Text;

            if (kind is SyntaxKind.PrivateKeyword or SyntaxKind.ProtectedKeyword && !isNestedType)
            {
                diagnostics.ReportModifierNotValidOnMember(text, typeKind, typeName, modifier.GetLocation());
                continue;
            }

            if (!allowed.Contains(kind))
                diagnostics.ReportModifierNotValidOnMember(text, typeKind, typeName, modifier.GetLocation());
        }
    }

    private static INamedTypeSymbol? GetDefaultBaseTypeForNominalDeclaration(
        TypeDeclarationSyntax declaration,
        INamedTypeSymbol? objectType,
        INamedTypeSymbol? valueType)
        => IsStructLikeNominalType(declaration) ? valueType : objectType;

    private static bool IsStructLikeNominalType(TypeDeclarationSyntax declaration)
        => declaration is StructDeclarationSyntax ||
           declaration.Modifiers.Any(modifier => modifier.Kind == SyntaxKind.StructKeyword);

    private ImmutableArray<INamedTypeSymbol> GetDefaultNominalInterfaces(INamedTypeSymbol typeSymbol)
    {
        if (typeSymbol is not SourceNamedTypeSymbol sourceType || !sourceType.IsRecord)
            return ImmutableArray<INamedTypeSymbol>.Empty;

        if (Compilation.GetTypeByMetadataName("System.IEquatable`1") is not INamedTypeSymbol equatableDefinition)
            return ImmutableArray<INamedTypeSymbol>.Empty;

        var equatableType = (INamedTypeSymbol)equatableDefinition.Construct(typeSymbol);
        return ImmutableArray.Create(equatableType);
    }

    private static ImmutableArray<INamedTypeSymbol> MergeInterfaceSets(
        ImmutableArray<INamedTypeSymbol> existing,
        ImmutableArray<INamedTypeSymbol> additional)
    {
        if (existing.IsDefaultOrEmpty)
            return additional;

        var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
        var seen = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);
        foreach (var type in existing)
            if (seen.Add(type))
                builder.Add(type);

        foreach (var type in additional)
            if (seen.Add(type))
                builder.Add(type);

        return builder.ToImmutable();
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

        ITypeSymbol ResolveTypeSyntaxForSignature(TypeSyntax typeSyntax, RefKind refKindHint)
        {
            var result = binder.BindTypeSyntax(typeSyntax);
            if (!result.Success)
                return binder.BindTypeSyntaxDirect(typeSyntax);

            return result.ResolvedType;
        }

        ITypeSymbol ResolveParameterTypeSyntaxForSignature(TypeSyntax typeSyntax, RefKind refKind)
        {
            var boundTypeSyntax = refKind.IsByRef() && typeSyntax is ByRefTypeSyntax byRefType
                ? byRefType.ElementType
                : typeSyntax;
            return ResolveTypeSyntaxForSignature(boundTypeSyntax, RefKind.None);
        }

        // .ctor(object, IntPtr)
        var ctor = new SourceMethodSymbol(
            ".ctor",
            unitType!,
            ImmutableArray<SourceParameterSymbol>.Empty,
            delegateSymbol,
            delegateSymbol,
            delegateSymbol.ContainingNamespace,
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
                delegateSymbol.ContainingNamespace,
                new[] { delegateDecl.GetLocation() },
                Array.Empty<SyntaxReference>(),
                RefKind.None),
            new SourceParameterSymbol(
                "method",
                intPtrType!,
                ctor,
                delegateSymbol,
                delegateSymbol.ContainingNamespace,
                new[] { delegateDecl.GetLocation() },
                Array.Empty<SyntaxReference>(),
                RefKind.None));

        ctor.SetParameters(ctorParams);
        RegisterMember(delegateSymbol, ctor);

        // Invoke
        var returnType = delegateDecl.ReturnType is null
            ? unitType!
            : ResolveTypeSyntaxForSignature(delegateDecl.ReturnType.Type, RefKind.None);

        var invoke = new SourceMethodSymbol(
            "Invoke",
            returnType,
            ImmutableArray<SourceParameterSymbol>.Empty,
            delegateSymbol,
            delegateSymbol,
            delegateSymbol.ContainingNamespace,
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
            var pType = ResolveParameterTypeSyntaxForSignature(typeSyntax, refKind);

            invokeParams.Add(new SourceParameterSymbol(
                p.Identifier.ValueText,
                pType,
                invoke,
                delegateSymbol,
                delegateSymbol.ContainingNamespace,
                new[] { p.GetLocation() },
                new[] { p.GetReference() },
                refKind));
        }

        invoke.SetParameters(invokeParams.ToImmutable());
        RegisterMember(delegateSymbol, invoke);
    }

    private void RegisterUnionCases(UnionDeclarationSyntax unionDecl, UnionDeclarationBinder unionBinder, SourceDiscriminatedUnionSymbol unionSymbol)
    {
        var allCasesAlreadyRegistered = true;
        foreach (var caseClause in unionDecl.Cases)
        {
            if (!TryGetUnionCaseSymbol(caseClause, out _))
            {
                allCasesAlreadyRegistered = false;
                break;
            }
        }

        if (allCasesAlreadyRegistered)
            return;

        if (!unionSymbol.Cases.IsDefaultOrEmpty && unionSymbol.Cases.Length > 0)
            return;

        var namespaceSymbol = unionBinder.CurrentNamespace?.AsSourceNamespace()
            ?? unionSymbol.ContainingNamespace?.AsSourceNamespace();
        var caseSymbols = new List<IDiscriminatedUnionCaseSymbol>();
        var payloadFields = new List<SourceFieldSymbol>();
        var unitType = Compilation.GetSpecialType(SpecialType.System_Unit);
        var valueType = Compilation.GetSpecialType(SpecialType.System_ValueType);
        var boolType = Compilation.GetSpecialType(SpecialType.System_Boolean);
        var stringType = Compilation.GetSpecialType(SpecialType.System_String);
        var objectToString = GetObjectToStringMethod();
        int ordinal = 0;

        void RegisterMember(SourceNamedTypeSymbol owner, ISymbol member)
        {
            if (!owner.GetMembers().Any(m => SymbolEqualityComparer.Default.Equals(m, member)))
                owner.AddMember(member);
        }

        void RegisterCaseMember(ISymbol member)
        {
            RegisterMember(unionSymbol, member);
            RegisterMember((SourceNamedTypeSymbol)member.ContainingType!, member);
        }

        static ITypeSymbol SubstituteTypeParameters(
            ITypeSymbol type,
            Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions)
        {
            if (type is ITypeParameterSymbol parameter &&
                substitutions.TryGetValue(parameter, out var replacement))
            {
                return replacement;
            }

            if (type is NullableTypeSymbol nullableType)
            {
                var substituted = SubstituteTypeParameters(nullableType.UnderlyingType, substitutions);
                return SymbolEqualityComparer.Default.Equals(substituted, nullableType.UnderlyingType)
                    ? type
                    : substituted.MakeNullable();
            }

            if (type is RefTypeSymbol refType)
            {
                var substituted = SubstituteTypeParameters(refType.ElementType, substitutions);
                return SymbolEqualityComparer.Default.Equals(substituted, refType.ElementType)
                    ? type
                    : new RefTypeSymbol(substituted);
            }

            if (type is IAddressTypeSymbol addressType)
            {
                var substituted = SubstituteTypeParameters(addressType.ReferencedType, substitutions);
                return SymbolEqualityComparer.Default.Equals(substituted, addressType.ReferencedType)
                    ? type
                    : new AddressTypeSymbol(substituted);
            }

            if (type is IArrayTypeSymbol arrayType)
            {
                var substituted = SubstituteTypeParameters(arrayType.ElementType, substitutions);
                return SymbolEqualityComparer.Default.Equals(substituted, arrayType.ElementType)
                    ? type
                    : new ArrayTypeSymbol(arrayType.BaseType, substituted, arrayType.ContainingSymbol, arrayType.ContainingType, arrayType.ContainingNamespace, [], arrayType.Rank);
            }

            if (type is INamedTypeSymbol namedType && !namedType.TypeArguments.IsDefaultOrEmpty)
            {
                var typeArguments = namedType.TypeArguments;
                var substituted = new ITypeSymbol[typeArguments.Length];
                var changed = false;

                for (var i = 0; i < typeArguments.Length; i++)
                {
                    substituted[i] = SubstituteTypeParameters(typeArguments[i], substitutions);
                    changed |= !SymbolEqualityComparer.Default.Equals(substituted[i], typeArguments[i]);
                }

                if (changed)
                    return namedType.Construct(substituted);
            }

            return type;
        }

        static void CollectReferencedTypeParameters(
            ITypeSymbol type,
            HashSet<ITypeParameterSymbol> knownUnionTypeParameters,
            List<ITypeParameterSymbol> usedUnionTypeParameters,
            HashSet<ITypeParameterSymbol> seen)
        {
            if (type is ITypeParameterSymbol parameter)
            {
                if (knownUnionTypeParameters.Contains(parameter) && seen.Add(parameter))
                    usedUnionTypeParameters.Add(parameter);
                return;
            }

            if (type is NullableTypeSymbol nullableType)
            {
                CollectReferencedTypeParameters(nullableType.UnderlyingType, knownUnionTypeParameters, usedUnionTypeParameters, seen);
                return;
            }

            if (type is RefTypeSymbol refType)
            {
                CollectReferencedTypeParameters(refType.ElementType, knownUnionTypeParameters, usedUnionTypeParameters, seen);
                return;
            }

            if (type is IAddressTypeSymbol addressType)
            {
                CollectReferencedTypeParameters(addressType.ReferencedType, knownUnionTypeParameters, usedUnionTypeParameters, seen);
                return;
            }

            if (type is IArrayTypeSymbol arrayType)
            {
                CollectReferencedTypeParameters(arrayType.ElementType, knownUnionTypeParameters, usedUnionTypeParameters, seen);
                return;
            }

            if (type is INamedTypeSymbol namedType && !namedType.TypeArguments.IsDefaultOrEmpty)
            {
                foreach (var typeArgument in namedType.TypeArguments)
                    CollectReferencedTypeParameters(typeArgument, knownUnionTypeParameters, usedUnionTypeParameters, seen);
            }
        }

        var unionAccessibility = unionSymbol.DeclaredAccessibility;
        var knownUnionTypeParameters = new HashSet<ITypeParameterSymbol>(unionSymbol.TypeParameters, SymbolEqualityComparer.Default);

        foreach (var caseClause in unionDecl.Cases)
        {
            var caseSymbol = new SourceDiscriminatedUnionCaseTypeSymbol(
                caseClause.Identifier.ValueText,
                DiscriminatedUnionFacts.GetCaseMetadataBaseName(unionSymbol.Name, caseClause.Identifier.ValueText),
                ordinal++,
                unionSymbol,
                valueType!,
                (ISymbol?)namespaceSymbol ?? unionSymbol,
                null,
                namespaceSymbol,
                [caseClause.GetLocation()],
                [caseClause.GetReference()],
                unionAccessibility);

            namespaceSymbol?.AddMember(caseSymbol);
            RegisterMember(unionSymbol, caseSymbol);

            var rawParameters = new List<(ParameterSyntax Syntax, ITypeSymbol Type, RefKind RefKind, bool HasExplicitDefaultValue, object? ExplicitDefaultValue, bool IsMutable)>();
            var seenOptionalParameter = false;

            if (caseClause.ParameterList is { } parameterList)
            {
                foreach (var parameterSyntax in parameterList.Parameters)
                {
                    var typeSyntax = parameterSyntax.TypeAnnotation?.Type;
                    var refKindTokenKind = parameterSyntax.RefKindKeyword?.Kind;
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

                    ITypeSymbol parameterType;
                    if (typeSyntax is null)
                    {
                        parameterType = Compilation.ErrorTypeSymbol;
                    }
                    else
                    {
                        var boundTypeSyntax = refKind.IsByRef() && typeSyntax is ByRefTypeSyntax byRefType
                            ? byRefType.ElementType
                            : typeSyntax;
                        parameterType = unionBinder.BindTypeSyntaxDirect(boundTypeSyntax);
                    }

                    var defaultResult = TypeMemberBinder.ProcessParameterDefault(
                        parameterSyntax,
                        parameterType,
                        parameterSyntax.Identifier.ValueText,
                        unionBinder.Diagnostics,
                        ref seenOptionalParameter);

                    var isMutable = parameterSyntax.BindingKeyword?.Kind == SyntaxKind.VarKeyword;
                    rawParameters.Add((parameterSyntax, parameterType, refKind, defaultResult.HasExplicitDefaultValue, defaultResult.ExplicitDefaultValue, isMutable));
                }
            }

            var usedUnionTypeParameters = new List<ITypeParameterSymbol>();
            var seenUnionTypeParameters = new HashSet<ITypeParameterSymbol>(SymbolEqualityComparer.Default);
            foreach (var rawParameter in rawParameters)
            {
                CollectReferencedTypeParameters(
                    rawParameter.Type,
                    knownUnionTypeParameters,
                    usedUnionTypeParameters,
                    seenUnionTypeParameters);
            }

            var caseTypeParameterMap = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);
            var projectedUnionTypeParameterMappings = new List<(ITypeParameterSymbol CaseTypeParameter, ITypeParameterSymbol UnionTypeParameter)>();
            if (usedUnionTypeParameters.Count > 0)
            {
                var caseTypeParameters = ImmutableArray.CreateBuilder<ITypeParameterSymbol>(usedUnionTypeParameters.Count);

                for (var i = 0; i < usedUnionTypeParameters.Count; i++)
                {
                    var unionTypeParameter = usedUnionTypeParameters[i];
                    var sourceUnionTypeParameter = unionTypeParameter as SourceTypeParameterSymbol;

                    var caseTypeParameter = new SourceTypeParameterSymbol(
                        unionTypeParameter.Name,
                        caseSymbol,
                        caseSymbol,
                        namespaceSymbol,
                        sourceUnionTypeParameter?.Locations.ToArray() ?? [caseClause.GetLocation()],
                        sourceUnionTypeParameter?.DeclaringSyntaxReferences.ToArray() ?? [caseClause.GetReference()],
                        i,
                        sourceUnionTypeParameter?.ConstraintKind ?? TypeParameterConstraintKind.None,
                        sourceUnionTypeParameter?.ConstraintTypeReferences ?? ImmutableArray<SyntaxReference>.Empty,
                        sourceUnionTypeParameter?.Variance ?? VarianceKind.None);

                    if (sourceUnionTypeParameter is { HasResolvedConstraintTypes: true })
                        caseTypeParameter.SetConstraintTypes(sourceUnionTypeParameter.ConstraintTypes);

                    caseTypeParameters.Add(caseTypeParameter);
                    caseTypeParameterMap[unionTypeParameter] = caseTypeParameter;
                    projectedUnionTypeParameterMappings.Add((caseTypeParameter, unionTypeParameter));
                }

                caseSymbol.SetTypeParameters(caseTypeParameters.MoveToImmutable());
                caseSymbol.SetProjectedUnionTypeParameters(projectedUnionTypeParameterMappings);
            }

            ITypeSymbol caseTypeForUnionMembers = caseSymbol;
            if (usedUnionTypeParameters.Count > 0)
            {
                caseTypeForUnionMembers = caseSymbol.Construct(
                    usedUnionTypeParameters.Cast<ITypeSymbol>().ToArray());
            }

            var payloadField = new SourceFieldSymbol(
                DiscriminatedUnionFieldUtilities.GetPayloadFieldName(caseSymbol.Name),
                caseTypeForUnionMembers,
                isStatic: false,
                isMutable: true,
                isConst: false,
                constantValue: null,
                unionSymbol,
                unionSymbol,
                namespaceSymbol,
                [caseClause.GetLocation()],
                [caseClause.GetReference()],
                null,
                declaredAccessibility: Accessibility.Internal);

            payloadFields.Add(payloadField);

            var constructor = new SourceMethodSymbol(
                ".ctor",
                unitType,
                ImmutableArray<SourceParameterSymbol>.Empty,
                caseSymbol,
                caseSymbol,
                namespaceSymbol,
                [caseClause.GetLocation()],
                [caseClause.GetReference()],
                isStatic: false,
                methodKind: MethodKind.Constructor,
                declaredAccessibility: Accessibility.Public);

            RegisterCaseMember(constructor);

            var parameters = new List<SourceParameterSymbol>();
            foreach (var rawParameter in rawParameters)
            {
                var parameterSyntax = rawParameter.Syntax;
                var refKind = rawParameter.RefKind;
                var parameterType = rawParameter.Type;
                if (caseTypeParameterMap.Count > 0)
                    parameterType = SubstituteTypeParameters(parameterType, caseTypeParameterMap);

                var parameterSymbol = new SourceParameterSymbol(
                    parameterSyntax.Identifier.ValueText,
                    parameterType,
                    constructor,
                    caseSymbol,
                    namespaceSymbol,
                    [parameterSyntax.GetLocation()],
                    [parameterSyntax.GetReference()],
                    refKind,
                    rawParameter.HasExplicitDefaultValue,
                    rawParameter.ExplicitDefaultValue,
                    rawParameter.IsMutable);

                parameters.Add(parameterSymbol);

                if (refKind == RefKind.None)
                {
                    var parameterName = parameterSyntax.Identifier.ValueText;
                    var propertyName = DiscriminatedUnionFacts.GetCasePropertyName(parameterName);

                    var backingField = new SourceFieldSymbol(
                        $"<{parameterSyntax.Identifier.ValueText}>k__BackingField",
                        parameterType,
                        isStatic: false,
                        isMutable: parameterSymbol.IsMutable,
                        isConst: false,
                        constantValue: null,
                        caseSymbol,
                        caseSymbol,
                        namespaceSymbol,
                        [parameterSyntax.GetLocation()],
                        [parameterSyntax.GetReference()],
                        new BoundParameterAccess(parameterSymbol),
                        declaredAccessibility: Accessibility.Private);

                    RegisterCaseMember(backingField);

                    var propertySymbol = new SourcePropertySymbol(
                        propertyName,
                        parameterType,
                        caseSymbol,
                        caseSymbol,
                        namespaceSymbol,
                        [parameterSyntax.GetLocation()],
                        [parameterSyntax.GetReference()],
                        declaredAccessibility: Accessibility.Private);

                    RegisterCaseMember(propertySymbol);

                    var getterSymbol = new SourceMethodSymbol(
                        $"get_{propertyName}",
                        parameterType,
                        ImmutableArray<SourceParameterSymbol>.Empty,
                        propertySymbol,
                        caseSymbol,
                        namespaceSymbol,
                        [parameterSyntax.GetLocation()],
                        [parameterSyntax.GetReference()],
                        isStatic: false,
                        methodKind: MethodKind.PropertyGet,
                        declaredAccessibility: Accessibility.Public);

                    RegisterCaseMember(getterSymbol);

                    propertySymbol.SetBackingField(backingField);
                    propertySymbol.SetAccessors(getterSymbol, null);
                }
            }

            constructor.SetParameters(parameters);
            caseSymbol.SetConstructorParameters(parameters);

            if (parameters.Count > 0)
            {
                var deconstructMethod = new SourceMethodSymbol(
                    "Deconstruct",
                    unitType,
                    ImmutableArray<SourceParameterSymbol>.Empty,
                    caseSymbol,
                    caseSymbol,
                    namespaceSymbol,
                    [],
                    [],
                    isStatic: false,
                    methodKind: MethodKind.Ordinary,
                    declaredAccessibility: Accessibility.Public);

                var deconstructParameters = ImmutableArray.CreateBuilder<SourceParameterSymbol>();

                foreach (var parameter in parameters)
                {
                    if (parameter.RefKind != RefKind.None)
                        continue;

                    var propertyName = DiscriminatedUnionFacts.GetCasePropertyName(parameter.Name);
                    if (caseSymbol.GetMembers(propertyName).OfType<IPropertySymbol>().FirstOrDefault() is not { } property)
                        continue;

                    var deconstructParameter = new SourceParameterSymbol(
                        property.Name,
                        property.Type,
                        deconstructMethod,
                        caseSymbol,
                        namespaceSymbol,
                        [caseClause.GetLocation()],
                        [caseClause.GetReference()],
                        refKind: RefKind.Out);

                    deconstructParameters.Add(deconstructParameter);
                }

                if (deconstructParameters.Count > 0)
                {
                    deconstructMethod.SetParameters(deconstructParameters.ToImmutable());
                    RegisterCaseMember(deconstructMethod);
                }
            }

            var caseToString = new SourceMethodSymbol(
                "ToString",
                stringType!,
                ImmutableArray<SourceParameterSymbol>.Empty,
                caseSymbol,
                caseSymbol,
                namespaceSymbol,
                new[] { caseClause.GetLocation() },
                Array.Empty<SyntaxReference>(),
                isStatic: false,
                methodKind: MethodKind.Ordinary,
                isOverride: true,
                declaredAccessibility: Accessibility.Public);

            RegisterCaseMember(caseToString);

            caseToString.SetOverriddenMethod(objectToString);
            RegisterUnionCaseSymbol(caseClause, caseSymbol);
            caseSymbols.Add(caseSymbol);

            var conversionMethod = new SourceMethodSymbol(
                "op_Implicit",
                unionSymbol,
                ImmutableArray<SourceParameterSymbol>.Empty,
                unionSymbol,
                unionSymbol,
                namespaceSymbol,
                new[] { caseClause.GetLocation() },
                Array.Empty<SyntaxReference>(),
                isStatic: true,
                methodKind: MethodKind.Conversion,
                declaredAccessibility: Accessibility.Public);

            var conversionParameter = new SourceParameterSymbol(
                "value",
                caseTypeForUnionMembers,
                conversionMethod,
                unionSymbol,
                namespaceSymbol,
                new[] { caseClause.GetLocation() },
                Array.Empty<SyntaxReference>());

            conversionMethod.SetParameters(new[] { conversionParameter });

            RegisterCaseMember(conversionMethod);

            var tryGetMethod = new SourceMethodSymbol(
                "TryGetValue",
                boolType!,
                ImmutableArray<SourceParameterSymbol>.Empty,
                unionSymbol,
                unionSymbol,
                namespaceSymbol,
                new[] { caseClause.GetLocation() },
                Array.Empty<SyntaxReference>(),
                isStatic: false,
                methodKind: MethodKind.Ordinary,
                declaredAccessibility: Accessibility.Public);

            var tryGetParameter = new SourceParameterSymbol(
                "value",
                caseTypeForUnionMembers, // Should be nullable?
                tryGetMethod,
                unionSymbol,
                namespaceSymbol,
                new[] { caseClause.GetLocation() },
                Array.Empty<SyntaxReference>(),
                RefKind.Out);

            tryGetMethod.SetParameters(new[] { tryGetParameter });

            RegisterCaseMember(tryGetMethod);
        }

        unionSymbol.SetCases(caseSymbols);
        unionSymbol.SetPayloadFields(payloadFields);
    }

    private void ReportExternalTypeRedeclaration(
        INamespaceSymbol? namespaceSymbol,
        SyntaxToken identifier,
        int arity,
        DiagnosticBag diagnostics)
    {
        var mergedNamespace = GetMergedNamespace(namespaceSymbol);
        if (mergedNamespace is null)
            return;

        foreach (var member in mergedNamespace.GetMembers(identifier.ValueText).OfType<INamedTypeSymbol>())
        {
            if (member.Arity == arity && member.ContainingAssembly != Compilation.Assembly)
            {
                diagnostics.ReportTypeAlreadyDefined(identifier.ValueText, identifier.GetLocation());
                break;
            }
        }
    }

    private (UnionDeclarationBinder Binder, SourceDiscriminatedUnionSymbol Symbol) RegisterUnionDeclaration(
        UnionDeclarationSyntax unionDecl,
        Binder parentBinder,
        ISymbol declaringSymbol,
        SourceNamespaceSymbol? namespaceSymbol,
        SourceDiscriminatedUnionSymbol? existingSymbol = null)
    {
        var containingType = declaringSymbol as INamedTypeSymbol;
        var containingNamespace = declaringSymbol switch
        {
            INamespaceSymbol ns => ns,
            INamedTypeSymbol type => type.ContainingNamespace,
            _ => namespaceSymbol
        };

        var baseTypeSymbol = Compilation.GetSpecialType(SpecialType.System_ValueType);
        var unionAccessibility = AccessibilityUtilities.DetermineAccessibility(
            unionDecl.Modifiers,
            AccessibilityUtilities.GetDefaultTypeAccessibility(declaringSymbol));

        var unionSymbol = existingSymbol ?? new SourceDiscriminatedUnionSymbol(
            unionDecl.Identifier.ValueText,
            baseTypeSymbol!,
            declaringSymbol,
            containingType,
            containingNamespace,
            [unionDecl.GetLocation()],
            [unionDecl.GetReference()],
            unionAccessibility);

        if (unionSymbol.TypeParameters.IsDefaultOrEmpty)
            InitializeTypeParameters(unionSymbol, unionDecl.TypeParameterList, unionDecl.ConstraintClauses);

        var unionBinder = new UnionDeclarationBinder(parentBinder, unionSymbol, unionDecl);
        unionBinder.EnsureTypeParameterConstraintTypesResolved(unionSymbol.TypeParameters);
        CacheBinder(unionDecl, unionBinder);

        namespaceSymbol ??= unionSymbol.ContainingNamespace?.AsSourceNamespace();

        var discriminatorField = new SourceFieldSymbol(
            DiscriminatedUnionFieldUtilities.TagFieldName,
            Compilation.GetSpecialType(SpecialType.System_Byte),
            isStatic: false,
            isMutable: true,
            isConst: false,
            constantValue: 0,
            unionSymbol,
            unionSymbol,
            namespaceSymbol,
            [unionDecl.GetLocation()],
            [unionDecl.GetReference()],
            null,
            declaredAccessibility: Accessibility.Internal);

        var stringType = Compilation.GetSpecialType(SpecialType.System_String);
        var objectToString = GetObjectToStringMethod();

        var unionToString = new SourceMethodSymbol(
            "ToString",
            stringType!,
            ImmutableArray<SourceParameterSymbol>.Empty,
            unionSymbol,
            unionSymbol,
            namespaceSymbol,
            new[] { unionDecl.GetLocation() },
            Array.Empty<SyntaxReference>(),
            isStatic: false,
            methodKind: MethodKind.Ordinary,
            isOverride: true,
            declaredAccessibility: Accessibility.Public);

        unionToString.SetOverriddenMethod(objectToString);
        unionSymbol.SetDiscriminatorField(discriminatorField);

        RegisterUnionSymbol(unionDecl, unionSymbol);

        return (unionBinder, unionSymbol);
    }

    private IMethodSymbol GetObjectToStringMethod()
    {
        var objectType = Compilation.GetSpecialType(SpecialType.System_Object);
        return objectType!
            .GetMembers("ToString")
            .OfType<IMethodSymbol>()
            .First(m => m.Parameters.Length == 0);
    }

    private void RegisterClassMembers(TypeDeclarationSyntax classDecl, ClassDeclarationBinder classBinder)
    {
        if (classDecl is ClassDeclarationSyntax { ParameterList: not null }
            or RecordDeclarationSyntax { ParameterList: not null }
            or StructDeclarationSyntax { ParameterList: not null })
            RegisterPrimaryConstructor(classDecl, classBinder);

        var nestedClassBinders = new List<(TypeDeclarationSyntax Syntax, ClassDeclarationBinder Binder)>();
        var objectType = Compilation.GetTypeByMetadataName("System.Object");
        var valueType = Compilation.GetSpecialType(SpecialType.System_ValueType);
        var parentType = (INamedTypeSymbol)classBinder.ContainingSymbol;

        foreach (var member in classDecl.Members)
        {
            switch (member)
            {
                case FieldDeclarationSyntax fieldDecl:
                    var fieldBinder = new TypeMemberBinder(classBinder, (INamedTypeSymbol)classBinder.ContainingSymbol);
                    fieldBinder.BindFieldDeclaration(fieldDecl);
                    CacheBinder(fieldDecl, fieldBinder);
                    foreach (var decl in fieldDecl.Declaration.Declarators)
                        CacheBinder(decl, fieldBinder);
                    break;

                case MethodDeclarationSyntax methodDecl:
                    var memberBinder = new TypeMemberBinder(classBinder, (INamedTypeSymbol)classBinder.ContainingSymbol);
                    var methodBinder = memberBinder.BindMethodDeclaration(methodDecl);
                    CacheBinder(methodDecl, methodBinder);
                    if (methodBinder.ContainingSymbol is IMethodSymbol methodSymbol)
                        RegisterMethodSymbol(methodDecl, methodSymbol);
                    break;

                case OperatorDeclarationSyntax operatorDecl:
                    var operatorBinder = new TypeMemberBinder(classBinder, (INamedTypeSymbol)classBinder.ContainingSymbol);
                    var boundOperatorBinder = operatorBinder.BindOperatorDeclaration(operatorDecl);
                    CacheBinder(operatorDecl, boundOperatorBinder);
                    break;

                case ConversionOperatorDeclarationSyntax conversionDecl:
                    var conversionBinder = new TypeMemberBinder(classBinder, (INamedTypeSymbol)classBinder.ContainingSymbol);
                    var boundConversionBinder = conversionBinder.BindConversionOperatorDeclaration(conversionDecl);
                    CacheBinder(conversionDecl, boundConversionBinder);
                    break;

                case PropertyDeclarationSyntax propDecl:
                    var propMemberBinder = new TypeMemberBinder(classBinder, (INamedTypeSymbol)classBinder.ContainingSymbol);
                    var accessorBinders = propMemberBinder.BindPropertyDeclaration(propDecl);
                    CacheBinder(propDecl, propMemberBinder);
                    foreach (var kv in accessorBinders)
                        CacheBinder(kv.Key, kv.Value);
                    break;

                case EventDeclarationSyntax eventDecl:
                    var eventMemberBinder = new TypeMemberBinder(classBinder, (INamedTypeSymbol)classBinder.ContainingSymbol);
                    var eventAccessors = eventMemberBinder.BindEventDeclaration(eventDecl);
                    CacheBinder(eventDecl, eventMemberBinder);
                    foreach (var kv in eventAccessors)
                        CacheBinder(kv.Key, kv.Value);
                    break;

                case IndexerDeclarationSyntax indexerDecl:
                    var indexerMemberBinder = new TypeMemberBinder(classBinder, (INamedTypeSymbol)classBinder.ContainingSymbol);
                    var indexerAccessorBinders = indexerMemberBinder.BindIndexerDeclaration(indexerDecl);
                    CacheBinder(indexerDecl, indexerMemberBinder);
                    foreach (var kv in indexerAccessorBinders)
                        CacheBinder(kv.Key, kv.Value);
                    break;

                case ConstructorDeclarationSyntax ctorDecl:
                    var ctorMemberBinder = new TypeMemberBinder(classBinder, (INamedTypeSymbol)classBinder.ContainingSymbol);
                    var ctorBinder = ctorMemberBinder.BindConstructorDeclaration(ctorDecl);
                    CacheBinder(ctorDecl, ctorBinder);
                    break;

                case NamedConstructorDeclarationSyntax ctorDecl:
                    var namedCtorMemberBinder = new TypeMemberBinder(classBinder, (INamedTypeSymbol)classBinder.ContainingSymbol);
                    var namedCtorBinder = namedCtorMemberBinder.BindNamedConstructorDeclaration(ctorDecl);
                    CacheBinder(ctorDecl, namedCtorBinder);
                    break;

                case DelegateDeclarationSyntax del:
                    var delMemberBinder = new TypeMemberBinder(classBinder, (INamedTypeSymbol)classBinder.ContainingSymbol);
                    var delBinder = delMemberBinder.BindDelegateDeclaration(del);
                    CacheBinder(del, delBinder);
                    break;

                case TypeDeclarationSyntax nestedClass when nestedClass is ClassDeclarationSyntax or StructDeclarationSyntax or RecordDeclarationSyntax:
                    var nestedSymbol = GetDeclaredTypeSymbol(nestedClass);
                    var nestedBinder = new ClassDeclarationBinder(classBinder, nestedSymbol, nestedClass);
                    nestedBinder.EnsureTypeParameterConstraintTypesResolved(nestedSymbol.TypeParameters);
                    var defaultNestedBaseType = GetDefaultBaseTypeForNominalDeclaration(nestedClass, objectType, valueType);
                    var nestedDefaultInterfaces = GetDefaultNominalInterfaces(nestedSymbol);
                    var shape = nestedBinder.BindNominalTypeShape(nestedClass, defaultNestedBaseType, nestedDefaultInterfaces);
                    var nestedBaseType = shape.BaseType;
                    var nestedInterfaces = shape.Interfaces;

                    if (nestedBaseType is not null &&
                        !SymbolEqualityComparer.Default.Equals(nestedSymbol.BaseType, nestedBaseType) &&
                        SymbolEqualityComparer.Default.Equals(nestedSymbol.BaseType, defaultNestedBaseType))
                    {
                        nestedSymbol.SetBaseType(nestedBaseType);
                    }

                    if (!nestedInterfaces.IsDefaultOrEmpty)
                        nestedSymbol.SetInterfaces(MergeInterfaceSets(nestedSymbol.Interfaces, nestedInterfaces));
                    CacheBinder(nestedClass, nestedBinder);
                    RegisterClassSymbol(nestedClass, nestedSymbol);
                    RegisterClassMembers(nestedClass, nestedBinder);
                    nestedBinder.EnsureDefaultConstructor();
                    nestedClassBinders.Add((nestedClass, nestedBinder));
                    break;

                case UnionDeclarationSyntax nestedUnion:
                    {
                        var declaringSymbol = (ISymbol)classBinder.ContainingSymbol;
                        var namespaceSymbol = classBinder.CurrentNamespace?.AsSourceNamespace();
                        var unionSymbol = (SourceDiscriminatedUnionSymbol)GetDeclaredTypeSymbol(nestedUnion);
                        var (unionBinder, resolvedSymbol) = RegisterUnionDeclaration(
                            nestedUnion,
                            classBinder,
                            declaringSymbol,
                            namespaceSymbol,
                            unionSymbol);
                        RegisterUnionCases(nestedUnion, unionBinder, resolvedSymbol);
                        break;
                    }

                case InterfaceDeclarationSyntax nestedInterface:
                    ImmutableArray<INamedTypeSymbol> parentInterfaces = ImmutableArray<INamedTypeSymbol>.Empty;
                    if (nestedInterface.BaseList is not null)
                    {
                        var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
                        foreach (var t in nestedInterface.BaseList.Types)
                        {
                            if (classBinder.TryResolveNamedTypeFromTypeSyntax(t, out var resolved) &&
                                resolved is not null &&
                                resolved.TypeKind == TypeKind.Interface)
                                builder.Add(resolved);
                        }
                        if (builder.Count > 0)
                            parentInterfaces = builder.ToImmutable();
                    }

                    if (!parentInterfaces.IsDefaultOrEmpty)
                        GetDeclaredTypeSymbol(nestedInterface).SetInterfaces(
                            MergeInterfaceSets(GetDeclaredTypeSymbol(nestedInterface).Interfaces, parentInterfaces));
                    var nestedInterfaceSymbol = GetDeclaredTypeSymbol(nestedInterface);
                    var nestedInterfaceBinder = new InterfaceDeclarationBinder(classBinder, nestedInterfaceSymbol, nestedInterface);
                    nestedInterfaceBinder.EnsureTypeParameterConstraintTypesResolved(nestedInterfaceSymbol.TypeParameters);
                    CacheBinder(nestedInterface, nestedInterfaceBinder);
                    RegisterInterfaceMembers(nestedInterface, nestedInterfaceBinder);
                    break;

                case EnumDeclarationSyntax enumDecl:
                    {
                        var enumSymbol = GetDeclaredTypeSymbol(enumDecl);

                        var enumBinder = new EnumDeclarationBinder(classBinder, enumSymbol, enumDecl);
                        CacheBinder(enumDecl, enumBinder);

                        var enumUnderlyingType = ResolveEnumUnderlyingType(enumDecl, classBinder);
                        enumSymbol.SetEnumUnderlyingType(enumUnderlyingType);

                        RegisterEnumMembers(enumDecl, enumBinder, enumSymbol, classBinder.CurrentNamespace!.AsSourceNamespace());
                        break;
                    }
            }
        }

        var checkedNestedClasses = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);
        foreach (var (nestedSyntax, nestedBinder) in nestedClassBinders)
        {
            var nestedSymbol = (INamedTypeSymbol)nestedBinder.ContainingSymbol;
            if (!checkedNestedClasses.Add(nestedSymbol))
                continue;

            ReportMissingAbstractBaseMembers(nestedSymbol, nestedSyntax, nestedBinder.Diagnostics);
        }

        if (classBinder.ContainingSymbol is SourceNamedTypeSymbol { IsRecord: true })
            RegisterRecordValueMembers(classDecl, classBinder);
        classBinder.EnsureDefaultConstructor();

    }


    private void RegisterEnumMembers(
        EnumDeclarationSyntax enumDecl,
        EnumDeclarationBinder enumBinder,
        SourceNamedTypeSymbol enumSymbol,
        SourceNamespaceSymbol? containingNamespace)
    {
        // C#-like enum member semantics:
        // - If no initializer is present, the value is previous + 1 (first defaults to 0).
        // - If an initializer is present, it must be a constant expression.
        // - Previous enum members must be in scope while binding later initializers.
        var underlyingType = enumSymbol.EnumUnderlyingType;
        var nextValue = (object?)null;

        foreach (var enumMember in enumDecl.Members)
        {
            var memberValue = nextValue ?? GetDefaultEnumMemberValue(underlyingType);
            var equalsValue = enumMember.EqualsValue;
            if (equalsValue is not null)
            {
                var exprBinder = new BlockBinder(enumSymbol, enumBinder);
                var bound = exprBinder.BindExpression(equalsValue.Value);
                foreach (var diagnostic in exprBinder.Diagnostics.AsEnumerable())
                    enumBinder.Diagnostics.Report(diagnostic);

                CacheBoundNode(equalsValue.Value, bound, exprBinder);

                if (TryGetEnumMemberConstantValue(bound, out var constant) &&
                    TryConvertEnumMemberValue(underlyingType, constant, out memberValue))
                {
                    if (!TryIncrementEnumValue(memberValue, underlyingType, out nextValue))
                    {
                        enumBinder.Diagnostics.ReportEnumMemberValueCannotConvert(
                            enumMember.Identifier.ValueText,
                            underlyingType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            equalsValue.Value.GetLocation());
                        nextValue = null;
                    }
                }
                else
                {
                    var typeDisplay = underlyingType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
                    if (!TryGetEnumMemberConstantValue(bound, out _))
                    {
                        enumBinder.Diagnostics.ReportEnumMemberValueMustBeConstant(
                            enumMember.Identifier.ValueText,
                            equalsValue.Value.GetLocation());
                    }
                    else
                    {
                        enumBinder.Diagnostics.ReportEnumMemberValueCannotConvert(
                            enumMember.Identifier.ValueText,
                            typeDisplay,
                            equalsValue.Value.GetLocation());
                    }

                    memberValue = nextValue ?? GetDefaultEnumMemberValue(underlyingType);
                    if (!TryIncrementEnumValue(memberValue, underlyingType, out nextValue))
                        nextValue = null;
                }
            }
            else
            {
                if (!TryIncrementEnumValue(memberValue, underlyingType, out nextValue))
                {
                    enumBinder.Diagnostics.ReportEnumMemberValueCannotConvert(
                        enumMember.Identifier.ValueText,
                        underlyingType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        enumMember.Identifier.GetLocation());
                    nextValue = null;
                }
            }

            var fieldSymbol = new SourceFieldSymbol(
                enumMember.Identifier.ValueText,
                enumSymbol,
                isStatic: true,
                isMutable: false,
                isConst: true,
                constantValue: memberValue,
                enumSymbol,
                enumSymbol,
                containingNamespace,
                new[] { enumMember.GetLocation() },
                new[] { enumMember.GetReference() },
                null,
                declaredAccessibility: Accessibility.Public);

            // Ensure later member initializers can resolve earlier members.
            if (!enumSymbol.GetMembers().Any(m => SymbolEqualityComparer.Default.Equals(m, fieldSymbol)))
                enumSymbol.AddMember(fieldSymbol);
        }
    }

    private ITypeSymbol ResolveEnumUnderlyingType(EnumDeclarationSyntax enumDecl, Binder binder)
    {
        var defaultType = Compilation.GetSpecialType(SpecialType.System_Int32);

        if (enumDecl.BaseList is not { } baseList || baseList.Types.Count == 0)
            return defaultType;

        if (baseList.Types.Count > 1)
        {
            for (var i = 1; i < baseList.Types.Count; i++)
            {
                binder.Diagnostics.ReportEnumUnderlyingTypeMustBeSingle(baseList.Types[i].GetLocation());
            }
        }

        var underlyingTypeSyntax = baseList.Types[0];
        var resolvedResult = binder.BindTypeSyntax(underlyingTypeSyntax);
        if (!resolvedResult.Success)
        {
            binder.ReportResolveTypeResultDiagnostics(resolvedResult, underlyingTypeSyntax);
            return defaultType;
        }

        var resolvedType = resolvedResult.ResolvedType;

        if (resolvedType is null || resolvedType == Compilation.ErrorTypeSymbol)
            return defaultType;

        var normalizedType = UnwrapAliasType(resolvedType);
        if (normalizedType is NullableTypeSymbol || !IsValidEnumUnderlyingType(normalizedType))
        {
            binder.Diagnostics.ReportEnumUnderlyingTypeMustBeIntegral(
                resolvedType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                underlyingTypeSyntax.GetLocation());
            return defaultType;
        }

        return normalizedType;
    }

    private static bool IsValidEnumUnderlyingType(ITypeSymbol type)
    {
        return type.SpecialType is
            SpecialType.System_SByte or
            SpecialType.System_Byte or
            SpecialType.System_Int16 or
            SpecialType.System_UInt16 or
            SpecialType.System_Int32 or
            SpecialType.System_UInt32 or
            SpecialType.System_Int64 or
            SpecialType.System_UInt64 or
            SpecialType.System_Char;
    }

    private static ITypeSymbol UnwrapAliasType(ITypeSymbol type)
    {
        while (type is IAliasSymbol alias && alias.UnderlyingSymbol is ITypeSymbol underlying)
            type = underlying;

        return type;
    }

    private static object GetDefaultEnumMemberValue(ITypeSymbol underlyingType)
    {
        return underlyingType.SpecialType switch
        {
            SpecialType.System_SByte => (sbyte)0,
            SpecialType.System_Byte => (byte)0,
            SpecialType.System_Int16 => (short)0,
            SpecialType.System_UInt16 => (ushort)0,
            SpecialType.System_Int32 => 0,
            SpecialType.System_UInt32 => (uint)0,
            SpecialType.System_Int64 => 0L,
            SpecialType.System_UInt64 => 0UL,
            SpecialType.System_Char => '\0',
            _ => 0
        };
    }

    private static bool TryGetEnumMemberConstantValue(BoundExpression expression, out object? value)
    {
        switch (expression)
        {
            case BoundLiteralExpression literal when literal.Value is not null:
                value = literal.Value;
                return true;
            case BoundFieldAccess fieldAccess:
                {
                    var field = fieldAccess.Field;
                    if (field is not null && field.IsConst)
                    {
                        value = field.GetConstantValue();
                        return value is not null;
                    }

                    break;
                }
        }

        value = null;
        return false;
    }

    private bool TryConvertEnumMemberValue(ITypeSymbol underlyingType, object? value, out object? converted)
    {
        if (value is null)
        {
            converted = null;
            return false;
        }

        underlyingType = UnwrapAliasType(underlyingType);

        if (underlyingType.SpecialType == SpecialType.System_Char)
        {
            if (value is char ch)
            {
                converted = ch;
                return true;
            }

            if (ConstantValueEvaluator.TryConvert(
                    Compilation.GetSpecialType(SpecialType.System_UInt16),
                    value,
                    out var ushortValue) &&
                ushortValue is ushort u16)
            {
                converted = (char)u16;
                return true;
            }

            converted = null;
            return false;
        }

        return ConstantValueEvaluator.TryConvert(underlyingType, value, out converted);
    }

    private static bool TryIncrementEnumValue(object? value, ITypeSymbol underlyingType, out object? incremented)
    {
        switch (underlyingType.SpecialType)
        {
            case SpecialType.System_SByte when value is sbyte sb && sb < sbyte.MaxValue:
                incremented = (sbyte)(sb + 1);
                return true;
            case SpecialType.System_Byte when value is byte b && b < byte.MaxValue:
                incremented = (byte)(b + 1);
                return true;
            case SpecialType.System_Int16 when value is short s && s < short.MaxValue:
                incremented = (short)(s + 1);
                return true;
            case SpecialType.System_UInt16 when value is ushort us && us < ushort.MaxValue:
                incremented = (ushort)(us + 1);
                return true;
            case SpecialType.System_Int32 when value is int i && i < int.MaxValue:
                incremented = i + 1;
                return true;
            case SpecialType.System_UInt32 when value is uint ui && ui < uint.MaxValue:
                incremented = ui + 1;
                return true;
            case SpecialType.System_Int64 when value is long l && l < long.MaxValue:
                incremented = l + 1;
                return true;
            case SpecialType.System_UInt64 when value is ulong ul && ul < ulong.MaxValue:
                incremented = ul + 1;
                return true;
            case SpecialType.System_Char when value is char ch && ch < char.MaxValue:
                incremented = (char)(ch + 1);
                return true;
        }

        incremented = null;
        return false;
    }

    private void RegisterInterfaceMembers(InterfaceDeclarationSyntax interfaceDecl, InterfaceDeclarationBinder interfaceBinder)
    {
        foreach (var member in interfaceDecl.Members)
        {
            switch (member)
            {
                case MethodDeclarationSyntax methodDecl:
                    {
                        var memberBinder = new TypeMemberBinder(interfaceBinder, (INamedTypeSymbol)interfaceBinder.ContainingSymbol);
                        var methodBinder = memberBinder.BindMethodDeclaration(methodDecl);
                        CacheBinder(methodDecl, methodBinder);
                        break;
                    }
                case OperatorDeclarationSyntax operatorDecl:
                    {
                        var memberBinder = new TypeMemberBinder(interfaceBinder, (INamedTypeSymbol)interfaceBinder.ContainingSymbol);
                        var operatorBinder = memberBinder.BindOperatorDeclaration(operatorDecl);
                        CacheBinder(operatorDecl, operatorBinder);
                        break;
                    }
                case ConversionOperatorDeclarationSyntax conversionDecl:
                    {
                        var memberBinder = new TypeMemberBinder(interfaceBinder, (INamedTypeSymbol)interfaceBinder.ContainingSymbol);
                        var conversionBinder = memberBinder.BindConversionOperatorDeclaration(conversionDecl);
                        CacheBinder(conversionDecl, conversionBinder);
                        break;
                    }
                case PropertyDeclarationSyntax propertyDecl:
                    {
                        var propertyBinder = new TypeMemberBinder(interfaceBinder, (INamedTypeSymbol)interfaceBinder.ContainingSymbol);
                        var accessorBinders = propertyBinder.BindPropertyDeclaration(propertyDecl);
                        CacheBinder(propertyDecl, propertyBinder);
                        foreach (var kv in accessorBinders)
                            CacheBinder(kv.Key, kv.Value);
                        break;
                    }
                case EventDeclarationSyntax eventDecl:
                    {
                        var eventBinder = new TypeMemberBinder(interfaceBinder, (INamedTypeSymbol)interfaceBinder.ContainingSymbol);
                        var accessorBinders = eventBinder.BindEventDeclaration(eventDecl);
                        CacheBinder(eventDecl, eventBinder);
                        foreach (var kv in accessorBinders)
                            CacheBinder(kv.Key, kv.Value);
                        break;
                    }
                case IndexerDeclarationSyntax indexerDecl:
                    {
                        var indexerBinder = new TypeMemberBinder(interfaceBinder, (INamedTypeSymbol)interfaceBinder.ContainingSymbol);
                        var accessorBinders = indexerBinder.BindIndexerDeclaration(indexerDecl);
                        CacheBinder(indexerDecl, indexerBinder);
                        foreach (var kv in accessorBinders)
                            CacheBinder(kv.Key, kv.Value);
                        break;
                    }
                case InterfaceDeclarationSyntax nestedInterface:
                    {
                        var parentInterface = (INamedTypeSymbol)interfaceBinder.ContainingSymbol;
                        ImmutableArray<INamedTypeSymbol> parentInterfaces = ImmutableArray<INamedTypeSymbol>.Empty;
                        if (nestedInterface.BaseList is not null)
                        {
                            var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
                            foreach (var t in nestedInterface.BaseList.Types)
                            {
                                if (interfaceBinder.TryResolveNamedTypeFromTypeSyntax(t, out var resolved) &&
                                    resolved is not null &&
                                    resolved.TypeKind == TypeKind.Interface)
                                    builder.Add(resolved);
                            }

                            if (builder.Count > 0)
                                parentInterfaces = builder.ToImmutable();
                        }

                        var nestedInterfaceSymbol = new SourceNamedTypeSymbol(
                            nestedInterface.Identifier.ValueText,
                            Compilation.GetTypeByMetadataName("System.Object")!,
                            TypeKind.Interface,
                            parentInterface,
                            parentInterface,
                            interfaceBinder.CurrentNamespace!.AsSourceNamespace(),
                            [nestedInterface.GetLocation()],
                            [nestedInterface.GetReference()],
                            true,
                            isAbstract: true,
                            declaredAccessibility: AccessibilityUtilities.DetermineAccessibility(
                                nestedInterface.Modifiers,
                                AccessibilityUtilities.GetDefaultTypeAccessibility(parentInterface)));

                        if (!parentInterfaces.IsDefaultOrEmpty)
                            nestedInterfaceSymbol.SetInterfaces(parentInterfaces);

                        var nestedInterfaceBinder = new InterfaceDeclarationBinder(interfaceBinder, nestedInterfaceSymbol, nestedInterface);
                        nestedInterfaceBinder.EnsureTypeParameterConstraintTypesResolved(nestedInterfaceSymbol.TypeParameters);
                        CacheBinder(nestedInterface, nestedInterfaceBinder);
                        RegisterInterfaceMembers(nestedInterface, nestedInterfaceBinder);
                        break;
                    }
            }
        }
    }

    private void RegisterExtensionMembers(ExtensionDeclarationSyntax extensionDecl, ExtensionDeclarationBinder extensionBinder)
    {
        if (extensionBinder.ContainingSymbol is SourceNamedTypeSymbol extensionSymbol)
        {
            var receiverType = extensionBinder.BindTypeSyntaxDirect(extensionDecl.ReceiverType);
            extensionSymbol.SetExtensionReceiverType(receiverType);
        }

        foreach (var member in extensionDecl.Members)
        {
            switch (member)
            {
                case MethodDeclarationSyntax methodDecl:
                    {
                        var memberBinder = new TypeMemberBinder(extensionBinder, (INamedTypeSymbol)extensionBinder.ContainingSymbol, extensionDecl.ReceiverType);
                        var methodBinder = memberBinder.BindMethodDeclaration(methodDecl);
                        CacheBinder(methodDecl, methodBinder);
                        break;
                    }

                case OperatorDeclarationSyntax operatorDecl:
                    {
                        var memberBinder = new TypeMemberBinder(extensionBinder, (INamedTypeSymbol)extensionBinder.ContainingSymbol, extensionDecl.ReceiverType);
                        var operatorBinder = memberBinder.BindOperatorDeclaration(operatorDecl);
                        CacheBinder(operatorDecl, operatorBinder);
                        break;
                    }
                case ConversionOperatorDeclarationSyntax conversionDecl:
                    {
                        var memberBinder = new TypeMemberBinder(extensionBinder, (INamedTypeSymbol)extensionBinder.ContainingSymbol, extensionDecl.ReceiverType);
                        var conversionBinder = memberBinder.BindConversionOperatorDeclaration(conversionDecl);
                        CacheBinder(conversionDecl, conversionBinder);
                        break;
                    }

                case PropertyDeclarationSyntax propertyDecl:
                    {
                        var memberBinder = new TypeMemberBinder(extensionBinder, (INamedTypeSymbol)extensionBinder.ContainingSymbol, extensionDecl.ReceiverType);
                        var accessorBinders = memberBinder.BindPropertyDeclaration(propertyDecl);
                        CacheBinder(propertyDecl, memberBinder);
                        foreach (var kv in accessorBinders)
                            CacheBinder(kv.Key, kv.Value);
                        break;
                    }
                case EventDeclarationSyntax eventDecl:
                    {
                        var memberBinder = new TypeMemberBinder(extensionBinder, (INamedTypeSymbol)extensionBinder.ContainingSymbol, extensionDecl.ReceiverType);
                        var accessorBinders = memberBinder.BindEventDeclaration(eventDecl);
                        CacheBinder(eventDecl, memberBinder);
                        foreach (var kv in accessorBinders)
                            CacheBinder(kv.Key, kv.Value);
                        break;
                    }
            }
        }
    }

    private static void InitializeTypeParameters(
        SourceNamedTypeSymbol typeSymbol,
        TypeParameterListSyntax? typeParameterList,
        SyntaxList<TypeParameterConstraintClauseSyntax> constraintClauses)
    {
        if (typeParameterList is null || typeParameterList.Parameters.Count == 0)
            return;

        var builder = ImmutableArray.CreateBuilder<ITypeParameterSymbol>(typeParameterList.Parameters.Count);
        var ordinal = 0;

        // Optional: pre-index clauses by name for fast lookup and duplicate detection
        var clausesByName = new Dictionary<string, List<TypeParameterConstraintClauseSyntax>>(StringComparer.Ordinal);
        foreach (var clause in constraintClauses)
        {
            var name = clause.TypeParameter.Identifier.ValueText;
            if (!clausesByName.TryGetValue(name, out var list))
                clausesByName[name] = list = new List<TypeParameterConstraintClauseSyntax>();
            list.Add(clause);
        }

        foreach (var parameter in typeParameterList.Parameters)
        {
            var name = parameter.Identifier.ValueText;

            // 1) inline
            var (inlineKind, inlineRefs) = TypeParameterConstraintAnalyzer.AnalyzeInline(parameter);

            // 2) where clauses for this parameter
            TypeParameterConstraintKind clauseKind = TypeParameterConstraintKind.None;
            var clauseRefsBuilder = ImmutableArray.CreateBuilder<SyntaxReference>();

            if (clausesByName.TryGetValue(name, out var matchingClauses))
            {
                // If you want “at most one where per type parameter”, enforce here.
                // Otherwise, allow multiple and merge.
                foreach (var clause in matchingClauses)
                {
                    var (k, refs) = TypeParameterConstraintAnalyzer.AnalyzeClause(clause);
                    clauseKind |= k;
                    clauseRefsBuilder.AddRange(refs);
                }
            }

            var mergedKind = inlineKind | clauseKind;
            var mergedRefs = inlineRefs.AddRange(clauseRefsBuilder.ToImmutable());

            var variance = GetDeclaredVariance(parameter);

            var typeParameter = new SourceTypeParameterSymbol(
                name,
                typeSymbol,
                typeSymbol,
                typeSymbol.ContainingNamespace,
                [parameter.GetLocation()],
                [parameter.GetReference()],
                ordinal++,
                mergedKind,
                mergedRefs,
                variance);

            builder.Add(typeParameter);
        }

        // Optional: report where-clauses that reference unknown type parameters
        // (you likely want diagnostics for this — but that needs a DiagnosticBag)
        // You can do it in the binder or in the place where you call this helper.

        typeSymbol.SetTypeParameters(builder.MoveToImmutable());
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

    private void RegisterPrimaryConstructor(TypeDeclarationSyntax classDecl, ClassDeclarationBinder classBinder)
    {
        var classSymbol = (SourceNamedTypeSymbol)classBinder.ContainingSymbol;
        var namespaceSymbol = classBinder.CurrentNamespace!.AsSourceNamespace();
        var unitType = Compilation.GetSpecialType(SpecialType.System_Unit);
        var isRecord = classSymbol.IsRecord;

        if (classSymbol.IsStatic)
        {
            classBinder.Diagnostics.ReportStaticClassCannotContainInstanceMember(
                classSymbol.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                "init",
                classDecl.ParameterList!.GetLocation());
            return;
        }

        var constructorSymbol = new SourceMethodSymbol(
            ".ctor",
            unitType,
            ImmutableArray<SourceParameterSymbol>.Empty,
            classSymbol,
            classSymbol,
            namespaceSymbol,
            [classDecl.GetLocation()],
            [classDecl.GetReference()],
            isStatic: false,
            methodKind: MethodKind.Constructor,
            declaredAccessibility: classSymbol.DeclaredAccessibility);

        if (isRecord)
        {
            constructorSymbol.MarkSetsRequiredMembers();
        }

        var parameters = new List<SourceParameterSymbol>();
        var recordProperties = isRecord
            ? ImmutableArray.CreateBuilder<SourcePropertySymbol>()
            : null;

        var seenOptionalParameter = false;
        foreach (var parameterSyntax in classDecl.ParameterList!.Parameters)
        {
            var typeSyntax = parameterSyntax.TypeAnnotation?.Type;
            var refKindTokenKind = parameterSyntax.RefKindKeyword?.Kind;
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

            ITypeSymbol parameterType;
            if (typeSyntax is null)
            {
                parameterType = Compilation.ErrorTypeSymbol;
            }
            else
            {
                var boundTypeSyntax = refKind.IsByRef() && typeSyntax is ByRefTypeSyntax byRefType
                    ? byRefType.ElementType
                    : typeSyntax;
                parameterType = classBinder.BindTypeSyntaxDirect(boundTypeSyntax);
            }

            var defaultResult = TypeMemberBinder.ProcessParameterDefault(
                parameterSyntax,
                parameterType,
                parameterSyntax.Identifier.ValueText,
                classBinder.Diagnostics,
                ref seenOptionalParameter);
            var isMutable = parameterSyntax.BindingKeyword?.Kind == SyntaxKind.VarKeyword;
            var parameterSymbol = new SourceParameterSymbol(
                ToCamelCase(parameterSyntax.Identifier.ValueText),
                parameterType,
                constructorSymbol,
                classSymbol,
                namespaceSymbol,
                [parameterSyntax.GetLocation()],
                [parameterSyntax.GetReference()],
                refKind,
                defaultResult.HasExplicitDefaultValue,
                defaultResult.ExplicitDefaultValue,
                isMutable);

            parameters.Add(parameterSymbol);

            if (refKind == RefKind.None)
            {
                if (isRecord)
                {
                    var propertySymbol = CreateRecordPropertyFromPrimaryConstructor(
                        classSymbol,
                        parameterSymbol,
                        parameterSyntax,
                        parameterType,
                        namespaceSymbol,
                        classBinder);

                    if (propertySymbol is not null)
                        recordProperties?.Add(propertySymbol);
                }
                else
                {
                    _ = new SourceFieldSymbol(
                        parameterSyntax.Identifier.ValueText,
                        parameterType,
                        isStatic: false,
                        isMutable: parameterSymbol.IsMutable,
                        isConst: false,
                        constantValue: null,
                        classSymbol,
                        classSymbol,
                        namespaceSymbol,
                        [parameterSyntax.GetLocation()],
                        [parameterSyntax.GetReference()],
                        new BoundParameterAccess(parameterSymbol),
                        declaredAccessibility: Accessibility.Private);
                }
            }
        }

        constructorSymbol.SetParameters(parameters);

        if (recordProperties is not null)
            classSymbol.SetRecordProperties(recordProperties.ToImmutable());
    }

    private string ToCamelCase(string valueText)
    {
        return char.ToLowerInvariant(valueText[0]) + valueText[1..];
    }

    private SourcePropertySymbol? CreateRecordPropertyFromPrimaryConstructor(
        SourceNamedTypeSymbol classSymbol,
        SourceParameterSymbol parameterSymbol,
        ParameterSyntax parameterSyntax,
        ITypeSymbol parameterType,
        SourceNamespaceSymbol? namespaceSymbol,
        ClassDeclarationBinder classBinder)
    {
        var propertyName = parameterSyntax.Identifier.ValueText;
        if (classSymbol.IsMemberDefined(propertyName, out _))
        {
            classBinder.Diagnostics.ReportTypeAlreadyDefinesMember(
                classSymbol.Name,
                propertyName,
                parameterSyntax.Identifier.GetLocation());
            return null;
        }

        var location = parameterSyntax.GetLocation();
        var references = Array.Empty<SyntaxReference>();

        var propertySymbol = new SourcePropertySymbol(
            propertyName,
            parameterType,
            classSymbol,
            classSymbol,
            namespaceSymbol,
            [location],
            references,
            isStatic: false,
            declaredAccessibility: Accessibility.Public);

        propertySymbol.MarkAsRequired();

        var backingField = new SourceFieldSymbol(
            $"<{propertySymbol.Name}>k__BackingField",
            parameterType,
            isStatic: false,
            isMutable: parameterSymbol.IsMutable,
            isConst: false,
            constantValue: null,
            classSymbol,
            classSymbol,
            namespaceSymbol,
            [location],
            references,
            new BoundParameterAccess(parameterSymbol),
            declaredAccessibility: Accessibility.Private);

        propertySymbol.SetBackingField(backingField);

        var getMethod = new SourceMethodSymbol(
            $"get_{propertySymbol.Name}",
            parameterType,
            ImmutableArray<SourceParameterSymbol>.Empty,
            propertySymbol,
            classSymbol,
            namespaceSymbol,
            [location],
            references,
            isStatic: false,
            methodKind: MethodKind.PropertyGet,
            declaredAccessibility: Accessibility.Public);

        var setMethodKind = parameterSymbol.IsMutable ? MethodKind.PropertySet : MethodKind.InitOnly;
        var setMethod = new SourceMethodSymbol(
            $"set_{propertySymbol.Name}",
            Compilation.GetSpecialType(SpecialType.System_Unit),
            ImmutableArray<SourceParameterSymbol>.Empty,
            propertySymbol,
            classSymbol,
            namespaceSymbol,
            [location],
            references,
            isStatic: false,
            methodKind: setMethodKind,
            declaredAccessibility: Accessibility.Public);

        var valueParameter = new SourceParameterSymbol(
            "value",
            parameterType,
            setMethod,
            classSymbol,
            namespaceSymbol,
            [location],
            references);
        setMethod.SetParameters(ImmutableArray.Create(valueParameter));

        propertySymbol.SetAccessors(getMethod, setMethod);
        return propertySymbol;
    }

    private void RegisterRecordValueMembers(TypeDeclarationSyntax classDecl, ClassDeclarationBinder classBinder)
    {
        if (classBinder.ContainingSymbol is not SourceNamedTypeSymbol recordSymbol || !recordSymbol.IsRecord)
            return;

        var boolType = Compilation.GetSpecialType(SpecialType.System_Boolean);
        var intType = Compilation.GetSpecialType(SpecialType.System_Int32);
        var objectType = Compilation.GetSpecialType(SpecialType.System_Object);
        var stringType = Compilation.GetSpecialType(SpecialType.System_String);
        var unitType = Compilation.GetSpecialType(SpecialType.System_Unit);
        var namespaceSymbol = classBinder.CurrentNamespace!.AsSourceNamespace();
        var location = classDecl.GetLocation();
        var references = Array.Empty<SyntaxReference>();

        if (objectType is null || boolType is null || intType is null || unitType is null || stringType is null)
            return;

        var equalsObject = objectType.GetMembers(nameof(object.Equals))
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m => m.Parameters.Length == 1 && m.Parameters[0].Type is NullableTypeSymbol { UnderlyingType: { SpecialType: SpecialType.System_Object } });

        var getHashCode = objectType.GetMembers(nameof(object.GetHashCode))
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m => m.Parameters.Length == 0);

        if (equalsObject is null || getHashCode is null)
            return;

        var objectToString = GetObjectToStringMethod();

        if (!HasMethod(recordSymbol, "Equals", MethodKind.Ordinary, recordSymbol))
        {
            var equalsTyped = new SourceMethodSymbol(
                "Equals",
                boolType,
                ImmutableArray<SourceParameterSymbol>.Empty,
                recordSymbol,
                recordSymbol,
                namespaceSymbol,
                [location],
                references,
                isStatic: false,
                methodKind: MethodKind.Ordinary,
                declaredAccessibility: Accessibility.Public);

            var otherParameter = new SourceParameterSymbol(
                "other",
                recordSymbol,
                equalsTyped,
                recordSymbol,
                namespaceSymbol,
                [location],
                references);
            equalsTyped.SetParameters(ImmutableArray.Create(otherParameter));
        }

        if (!HasMethod(recordSymbol, "Equals", MethodKind.Ordinary, objectType))
        {
            var equalsObj = new SourceMethodSymbol(
                "Equals",
                boolType,
                ImmutableArray<SourceParameterSymbol>.Empty,
                recordSymbol,
                recordSymbol,
                namespaceSymbol,
                [location],
                references,
                isStatic: false,
                methodKind: MethodKind.Ordinary,
                isOverride: true,
                declaredAccessibility: Accessibility.Public);

            equalsObj.SetOverriddenMethod(equalsObject);

            var otherParameter = new SourceParameterSymbol(
                "obj",
                objectType,
                equalsObj,
                recordSymbol,
                namespaceSymbol,
                [location],
                references);
            equalsObj.SetParameters(ImmutableArray.Create(otherParameter));
        }

        if (!HasMethod(recordSymbol, "GetHashCode", MethodKind.Ordinary))
        {
            var hashMethod = new SourceMethodSymbol(
                "GetHashCode",
                intType,
                ImmutableArray<SourceParameterSymbol>.Empty,
                recordSymbol,
                recordSymbol,
                namespaceSymbol,
                [location],
                references,
                isStatic: false,
                methodKind: MethodKind.Ordinary,
                isOverride: true,
                declaredAccessibility: Accessibility.Public);

            hashMethod.SetOverriddenMethod(getHashCode);
        }

        if (!HasMethod(recordSymbol, "ToString", MethodKind.Ordinary))
        {
            var toStringMethod = new SourceMethodSymbol(
                "ToString",
                stringType,
                ImmutableArray<SourceParameterSymbol>.Empty,
                recordSymbol,
                recordSymbol,
                namespaceSymbol,
                [location],
                references,
                isStatic: false,
                methodKind: MethodKind.Ordinary,
                isOverride: true,
                declaredAccessibility: Accessibility.Public);

            toStringMethod.SetOverriddenMethod(objectToString);
        }

        if (!HasMethod(recordSymbol, "op_Equality", MethodKind.UserDefinedOperator, recordSymbol, recordSymbol))
        {
            var equalsOperator = new SourceMethodSymbol(
                "op_Equality",
                boolType,
                ImmutableArray<SourceParameterSymbol>.Empty,
                recordSymbol,
                recordSymbol,
                namespaceSymbol,
                [location],
                references,
                isStatic: true,
                methodKind: MethodKind.UserDefinedOperator,
                declaredAccessibility: Accessibility.Public);

            var leftParameter = new SourceParameterSymbol(
                "left",
                recordSymbol,
                equalsOperator,
                recordSymbol,
                namespaceSymbol,
                [location],
                references);
            var rightParameter = new SourceParameterSymbol(
                "right",
                recordSymbol,
                equalsOperator,
                recordSymbol,
                namespaceSymbol,
                [location],
                references);
            equalsOperator.SetParameters(ImmutableArray.Create(leftParameter, rightParameter));
        }

        if (!HasMethod(recordSymbol, "op_Inequality", MethodKind.UserDefinedOperator, recordSymbol, recordSymbol))
        {
            var notEqualsOperator = new SourceMethodSymbol(
                "op_Inequality",
                boolType,
                ImmutableArray<SourceParameterSymbol>.Empty,
                recordSymbol,
                recordSymbol,
                namespaceSymbol,
                [location],
                references,
                isStatic: true,
                methodKind: MethodKind.UserDefinedOperator,
                declaredAccessibility: Accessibility.Public);

            var leftParameter = new SourceParameterSymbol(
                "left",
                recordSymbol,
                notEqualsOperator,
                recordSymbol,
                namespaceSymbol,
                [location],
                references);
            var rightParameter = new SourceParameterSymbol(
                "right",
                recordSymbol,
                notEqualsOperator,
                recordSymbol,
                namespaceSymbol,
                [location],
                references);
            notEqualsOperator.SetParameters(ImmutableArray.Create(leftParameter, rightParameter));
        }

        if (!HasDeconstruct(recordSymbol))
        {
            var deconstructMethod = new SourceMethodSymbol(
                "Deconstruct",
                unitType,
                ImmutableArray<SourceParameterSymbol>.Empty,
                recordSymbol,
                recordSymbol,
                namespaceSymbol,
                [location],
                references,
                isStatic: false,
                methodKind: MethodKind.Ordinary,
                declaredAccessibility: Accessibility.Public);

            var parameters = ImmutableArray.CreateBuilder<SourceParameterSymbol>(recordSymbol.RecordProperties.Length);
            foreach (var property in recordSymbol.RecordProperties)
            {
                var parameter = new SourceParameterSymbol(
                    property.Name,
                    property.Type,
                    deconstructMethod,
                    recordSymbol,
                    namespaceSymbol,
                    [location],
                    references,
                    refKind: RefKind.Out);

                parameters.Add(parameter);
            }

            deconstructMethod.SetParameters(parameters.ToImmutable());
        }

        if (!HasCopyConstructor(recordSymbol))
        {
            var copyConstructor = new SourceMethodSymbol(
                ".ctor",
                unitType,
                ImmutableArray<SourceParameterSymbol>.Empty,
                recordSymbol,
                recordSymbol,
                namespaceSymbol,
                [location],
                references,
                isStatic: false,
                methodKind: MethodKind.Constructor,
                declaredAccessibility: Accessibility.Internal);

            var otherParameter = new SourceParameterSymbol(
                "other",
                recordSymbol,
                copyConstructor,
                recordSymbol,
                namespaceSymbol,
                [location],
                references);
            copyConstructor.SetParameters(ImmutableArray.Create(otherParameter));
        }
    }

    private static bool HasMethod(
        SourceNamedTypeSymbol typeSymbol,
        string name,
        MethodKind methodKind,
        params ITypeSymbol[] parameters)
    {
        foreach (var method in typeSymbol.GetMembers(name).OfType<IMethodSymbol>())
        {
            if (method.MethodKind != methodKind)
                continue;

            if (method.Parameters.Length != parameters.Length)
                continue;

            var matches = true;
            for (var i = 0; i < parameters.Length; i++)
            {
                if (!SymbolEqualityComparer.Default.Equals(method.Parameters[i].Type, parameters[i]))
                {
                    matches = false;
                    break;
                }
            }

            if (matches)
                return true;
        }

        return false;
    }

    private static bool HasDeconstruct(SourceNamedTypeSymbol typeSymbol)
    {
        foreach (var method in typeSymbol.GetMembers("Deconstruct").OfType<IMethodSymbol>())
        {
            if (method.MethodKind != MethodKind.Ordinary || method.IsStatic)
                continue;

            if (method.ReturnType.SpecialType != SpecialType.System_Unit)
                continue;

            if (method.Parameters.Any(parameter => parameter.RefKind != RefKind.Out))
                continue;

            if (method.Parameters.Length != typeSymbol.RecordProperties.Length)
                continue;

            return true;
        }

        return false;
    }

    private static bool HasCopyConstructor(SourceNamedTypeSymbol typeSymbol)
    {
        foreach (var constructor in typeSymbol.InstanceConstructors)
        {
            if (constructor.IsStatic)
                continue;

            if (constructor.Parameters.Length != 1)
                continue;

            if (SymbolEqualityComparer.Default.Equals(constructor.Parameters[0].Type, typeSymbol))
                return true;
        }

        return false;
    }

    private static TypeParameterListSyntax? GetTypeParameterList(TypeDeclarationSyntax declaration)
        => declaration switch
        {
            ClassDeclarationSyntax classDeclaration => classDeclaration.TypeParameterList,
            RecordDeclarationSyntax recordDeclaration => recordDeclaration.TypeParameterList,
            StructDeclarationSyntax structDeclaration => structDeclaration.TypeParameterList,
            _ => null
        };

    private static SyntaxList<TypeParameterConstraintClauseSyntax> GetConstraintClauses(TypeDeclarationSyntax declaration)
        => declaration switch
        {
            ClassDeclarationSyntax classDeclaration => classDeclaration.ConstraintClauses,
            RecordDeclarationSyntax recordDeclaration => recordDeclaration.ConstraintClauses,
            StructDeclarationSyntax structDeclaration => structDeclaration.ConstraintClauses,
            _ => SyntaxList<TypeParameterConstraintClauseSyntax>.Empty
        };

    private static BaseListSyntax? GetBaseList(TypeDeclarationSyntax declaration)
        => declaration switch
        {
            ClassDeclarationSyntax classDeclaration => classDeclaration.BaseList,
            RecordDeclarationSyntax recordDeclaration => recordDeclaration.BaseList,
            StructDeclarationSyntax structDeclaration => structDeclaration.BaseList,
            _ => null
        };

    private void ReportMissingAbstractBaseMembers(
        INamedTypeSymbol typeSymbol,
        TypeDeclarationSyntax declaration,
        DiagnosticBag diagnostics)
    {
        if (typeSymbol.IsAbstract)
            return;

        if (typeSymbol.BaseType is not INamedTypeSymbol baseType ||
            baseType.TypeKind != TypeKind.Class)
        {
            return;
        }

        foreach (var abstractMember in baseType.GetMembers().OfType<IMethodSymbol>())
        {
            if (!abstractMember.IsAbstract || abstractMember.IsStatic)
                continue;

            if (abstractMember.MethodKind is MethodKind.Constructor or MethodKind.StaticConstructor)
                continue;

            if (ImplementsAbstractMember(typeSymbol, abstractMember))
                continue;

            var memberDisplay = GetAbstractMemberDisplay(abstractMember);
            var baseTypeDisplay = baseType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);

            diagnostics.ReportTypeDoesNotImplementAbstractMember(
                typeSymbol.Name,
                memberDisplay,
                baseTypeDisplay,
                declaration.Identifier.GetLocation());
        }
    }

    private static bool ImplementsAbstractMember(INamedTypeSymbol typeSymbol, IMethodSymbol abstractMember)
    {
        foreach (var candidate in typeSymbol.GetMembers(abstractMember.Name).OfType<IMethodSymbol>())
        {
            if (!candidate.IsOverride)
                continue;

            if (candidate is SourceMethodSymbol sourceMethod &&
                sourceMethod.OverriddenMethod is not null &&
                SymbolEqualityComparer.Default.Equals(sourceMethod.OverriddenMethod, abstractMember))
            {
                return true;
            }
        }

        return false;
    }

    private static string GetAbstractMemberDisplay(IMethodSymbol abstractMember)
    {
        if (abstractMember.AssociatedSymbol is IPropertySymbol property)
        {
            var propertyFormat = SymbolDisplayFormat.MinimallyQualifiedFormat.WithMemberOptions(SymbolDisplayMemberOptions.None);
            return property.ToDisplayString(propertyFormat);
        }

        var methodFormat = SymbolDisplayFormat.MinimallyQualifiedFormat.WithMemberOptions(SymbolDisplayMemberOptions.IncludeParameters);
        return abstractMember.ToDisplayString(methodFormat);
    }

    internal BoundNode? TryGetCachedBoundNode(SyntaxNode node)
        => _boundNodeCache.TryGetValue(node, out var bound) ? bound : null;

    internal BoundNode? TryGetCachedLoweredBoundNode(SyntaxNode node)
        => _loweredBoundNodeCache.TryGetValue(node, out var bound) ? bound : null;

    internal void CacheBoundNode(SyntaxNode node, BoundNode bound, Binder? binder = null)
    {
        _boundNodeCache[node] = bound;
        _syntaxCache[bound] = node;
        if (IsDebuggingEnabled && binder is not null)
        {
            _boundNodeCache2.TryAdd(node, (binder, bound));
        }
    }

    internal void CacheLoweredBoundNode(SyntaxNode node, BoundNode bound, Binder? binder = null)
    {
        _loweredBoundNodeCache[node] = bound;
        _loweredSyntaxCache[bound] = node;
        PropagateLoweredSyntaxMappings(bound, node);
        if (IsDebuggingEnabled && binder is not null)
        {
            _loweredBoundNodeCache2[node] = (binder, bound);
        }
    }

    private void PropagateLoweredSyntaxMappings(BoundNode loweredRoot, SyntaxNode fallbackSyntax)
    {
        var resolving = new HashSet<BoundNode>(ReferenceEqualityComparer.Instance);
        if (!_loweredSyntaxCache.ContainsKey(loweredRoot))
            _loweredSyntaxCache[loweredRoot] = fallbackSyntax;

        foreach (var child in EnumerateBoundChildren(loweredRoot))
            _ = ResolveLoweredSyntax(child, fallbackSyntax, resolving);
    }

    private SyntaxNode ResolveLoweredSyntax(
        BoundNode node,
        SyntaxNode fallbackSyntax,
        HashSet<BoundNode> resolving)
    {
        if (_loweredSyntaxCache.TryGetValue(node, out var loweredSyntax))
            return loweredSyntax;

        if (_syntaxCache.TryGetValue(node, out var originalSyntax))
        {
            _loweredSyntaxCache[node] = originalSyntax;
            return originalSyntax;
        }

        if (!resolving.Add(node))
            return fallbackSyntax;

        SyntaxNode? resolvedSyntax = null;
        foreach (var child in EnumerateBoundChildren(node))
        {
            var childSyntax = ResolveLoweredSyntax(child, fallbackSyntax, resolving);
            if (childSyntax is null)
                continue;

            resolvedSyntax = ChoosePreferredLoweredSyntax(fallbackSyntax, resolvedSyntax, childSyntax);
        }

        resolving.Remove(node);

        resolvedSyntax ??= fallbackSyntax;
        _loweredSyntaxCache[node] = resolvedSyntax;
        return resolvedSyntax;
    }

    private static SyntaxNode ChoosePreferredLoweredSyntax(
        SyntaxNode fallbackSyntax,
        SyntaxNode? currentBest,
        SyntaxNode candidate)
    {
        if (currentBest is null)
            return candidate;

        var candidateScore = ScoreLoweredSyntaxCandidate(candidate, fallbackSyntax);
        var currentScore = ScoreLoweredSyntaxCandidate(currentBest, fallbackSyntax);

        if (candidateScore.CompareTo(currentScore) > 0)
            return candidate;

        return currentBest;
    }

    private static (int DiffersFromFallback, int IsContainedByFallback, int SpanLength, int EarlierStart) ScoreLoweredSyntaxCandidate(
        SyntaxNode candidate,
        SyntaxNode fallbackSyntax)
    {
        var differsFromFallback = candidate.Span != fallbackSyntax.Span ? 1 : 0;
        var isContainedByFallback = ContainsSpan(fallbackSyntax.Span, candidate.Span) ? 1 : 0;
        var spanLength = candidate.Span.Length;
        var earlierStart = -candidate.Span.Start;
        return (differsFromFallback, isContainedByFallback, spanLength, earlierStart);
    }

    private static bool ContainsSpan(TextSpan outer, TextSpan inner)
        => outer.Start <= inner.Start && outer.End >= inner.End;

    private static IEnumerable<BoundNode> EnumerateBoundChildren(BoundNode node)
    {
        foreach (var property in GetBoundChildProperties(node.GetType()))
        {
            var value = property.GetValue(node);
            switch (value)
            {
                case null:
                    continue;
                case BoundNode child:
                    yield return child;
                    break;
                case IEnumerable enumerable:
                    foreach (var item in enumerable)
                    {
                        if (item is BoundNode boundChild)
                            yield return boundChild;
                    }

                    break;
            }
        }
    }

    private static PropertyInfo[] GetBoundChildProperties(Type type)
    {
        lock (s_boundChildPropertyCacheGate)
        {
            if (s_boundChildPropertyCache.TryGetValue(type, out var cached))
                return cached;

            var properties = type.GetProperties(BindingFlags.Instance | BindingFlags.Public)
                .Where(static p => p.GetIndexParameters().Length == 0)
                .Where(static p => IsCandidateBoundChildProperty(p.PropertyType))
                .ToArray();

            s_boundChildPropertyCache[type] = properties;
            return properties;
        }
    }

    private static bool IsCandidateBoundChildProperty(Type propertyType)
    {
        if (typeof(BoundNode).IsAssignableFrom(propertyType))
            return true;

        return propertyType != typeof(string) && typeof(IEnumerable).IsAssignableFrom(propertyType);
    }

    internal void RemoveCachedBoundNode(SyntaxNode node)
    {
        if (_boundNodeCache.TryGetValue(node, out var bound))
            _syntaxCache.Remove(bound);

        _boundNodeCache.Remove(node);
        if (_loweredBoundNodeCache.TryGetValue(node, out var loweredBound))
            _loweredSyntaxCache.Remove(loweredBound);

        _loweredBoundNodeCache.Remove(node);

        if (IsDebuggingEnabled)
        {
            _boundNodeCache2.Remove(node);
            _loweredBoundNodeCache2.Remove(node);
        }
    }

    internal SyntaxNode? GetSyntax(BoundNode node)
        => _syntaxCache.TryGetValue(node, out var syntax)
            ? syntax
            : _loweredSyntaxCache.TryGetValue(node, out var loweredSyntax)
                ? loweredSyntax
                : null;

    private readonly Dictionary<SyntaxNodeMapKey, SourceNamedTypeSymbol> _declaredTypeSymbols = new();

    private void RegisterDeclaredTypeSymbol(SyntaxNode node, SourceNamedTypeSymbol symbol)
    {
        _declaredTypeSymbols[GetSyntaxNodeMapKey(node)] = symbol;

        if (node is TypeDeclarationSyntax typeDecl)
            RegisterClassSymbol(typeDecl, symbol);
        else if (node is UnionDeclarationSyntax unionDecl && symbol is SourceDiscriminatedUnionSymbol unionSymbol)
            RegisterUnionSymbol(unionDecl, unionSymbol);
    }

    private SourceNamedTypeSymbol GetDeclaredTypeSymbol(SyntaxNode node)
    {
        if (_declaredTypeSymbols.TryGetValue(GetSyntaxNodeMapKey(node), out var symbol))
            return symbol;

        // Defensive recovery: in some re-entrant binder flows, declarations may not
        // have been materialized for this semantic model yet.
        EnsureDeclarations();
        if (_declaredTypeSymbols.TryGetValue(GetSyntaxNodeMapKey(node), out symbol))
            return symbol;

        throw new InvalidOperationException($"Type symbol not declared for syntax node '{node}'.");
    }

    internal SourceNamedTypeSymbol GetDeclaredTypeSymbolForDeclaration(SyntaxNode node)
        => GetDeclaredTypeSymbol(node);

    private readonly Dictionary<SyntaxNodeMapKey, SourceNamedTypeSymbol> _classSymbols = new();

    internal void RegisterClassSymbol(TypeDeclarationSyntax node, SourceNamedTypeSymbol symbol)
        => _classSymbols[GetSyntaxNodeMapKey(node)] = symbol;

    internal SourceNamedTypeSymbol GetClassSymbol(TypeDeclarationSyntax node)
        => _classSymbols[GetSyntaxNodeMapKey(node)];

    internal bool TryGetClassSymbol(TypeDeclarationSyntax node, out SourceNamedTypeSymbol symbol)
        => _classSymbols.TryGetValue(GetSyntaxNodeMapKey(node), out symbol!);

    private readonly Dictionary<SyntaxNodeMapKey, SourceDiscriminatedUnionSymbol> _unionSymbols = new();
    private readonly Dictionary<SyntaxNodeMapKey, SourceDiscriminatedUnionCaseTypeSymbol> _unionCaseSymbols = new();

    internal void RegisterUnionSymbol(UnionDeclarationSyntax node, SourceDiscriminatedUnionSymbol symbol)
        => _unionSymbols[GetSyntaxNodeMapKey(node)] = symbol;

    internal SourceDiscriminatedUnionSymbol GetUnionSymbol(UnionDeclarationSyntax node)
        => _unionSymbols[GetSyntaxNodeMapKey(node)];

    internal bool TryGetUnionSymbol(UnionDeclarationSyntax node, out SourceDiscriminatedUnionSymbol symbol)
        => _unionSymbols.TryGetValue(GetSyntaxNodeMapKey(node), out symbol!);

    internal void RegisterUnionCaseSymbol(UnionCaseClauseSyntax node, SourceDiscriminatedUnionCaseTypeSymbol symbol)
        => _unionCaseSymbols[GetSyntaxNodeMapKey(node)] = symbol;

    internal SourceDiscriminatedUnionCaseTypeSymbol GetUnionCaseSymbol(UnionCaseClauseSyntax node)
        => _unionCaseSymbols[GetSyntaxNodeMapKey(node)];

    internal bool TryGetUnionCaseSymbol(UnionCaseClauseSyntax node, out SourceDiscriminatedUnionCaseTypeSymbol symbol)
        => _unionCaseSymbols.TryGetValue(GetSyntaxNodeMapKey(node), out symbol!);

    private readonly Dictionary<SyntaxNodeMapKey, IMethodSymbol> _methodSymbols = new();

    internal void RegisterMethodSymbol(MethodDeclarationSyntax node, IMethodSymbol symbol)
        => _methodSymbols[GetSyntaxNodeMapKey(node)] = symbol;

    internal bool TryGetMethodSymbol(MethodDeclarationSyntax node, out IMethodSymbol symbol)
        => _methodSymbols.TryGetValue(GetSyntaxNodeMapKey(node), out symbol!);

    private static SyntaxNodeMapKey GetSyntaxNodeMapKey(SyntaxNode node)
        => new(node.SyntaxTree, node.Span, node.Kind);

    private readonly record struct SyntaxNodeMapKey(SyntaxTree SyntaxTree, TextSpan Span, SyntaxKind Kind);
}
