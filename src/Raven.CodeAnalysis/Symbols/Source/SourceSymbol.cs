using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Symbols;

internal abstract class SourceSymbol : Symbol
{
    private ImmutableArray<AttributeData> _lazyCustomAttributes;
    private DocumentationComment? _lazyDocumentationComment;

    internal ImmutableArray<DocumentationComment> DocumentationComments { get; private set; }
    internal DocumentationComment? DocumentationComment { get; private set; }

    protected SourceSymbol(
        SymbolKind kind,
        string name,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        Accessibility declaredAccessibility = Accessibility.NotApplicable)
        : base(kind, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences, declaredAccessibility)
    {
        var parseOptions = GetParseOptions(declaringSyntaxReferences);
        DocumentationComments = DocumentationCommentUtilities.GetDocumentationComments(declaringSyntaxReferences, parseOptions);
        DocumentationComment = DocumentationCommentUtilities.GetMergedDocumentationComment(declaringSyntaxReferences, parseOptions);
    }

    public override IAssemblySymbol ContainingAssembly => ContainingNamespace?.ContainingAssembly;

    public override IModuleSymbol ContainingModule => ContainingNamespace?.ContainingModule;

    public override DocumentationComment? GetDocumentationComment()
    {
        if (_lazyDocumentationComment is not null)
            return _lazyDocumentationComment;

        _lazyDocumentationComment = DocumentationComment;
        return _lazyDocumentationComment;
    }

    public override ImmutableArray<AttributeData> GetAttributes()
    {
        if (_lazyCustomAttributes.IsDefault)
            _lazyCustomAttributes = ComputeAttributes();

        return _lazyCustomAttributes;
    }

    private ImmutableArray<AttributeData> ComputeAttributes()
    {
        if (DeclaringSyntaxReferences.IsDefaultOrEmpty)
            return ImmutableArray<AttributeData>.Empty;

        var compilation = GetDeclaringCompilation();
        if (compilation is null)
            return ImmutableArray<AttributeData>.Empty;

        var builder = ImmutableArray.CreateBuilder<AttributeData>();
        var seenAttributes = new Dictionary<AttributeTargets, HashSet<INamedTypeSymbol>>();
        var defaultTarget = AttributeUsageHelper.GetDefaultTargetForOwner(this);

        foreach (var syntaxReference in DeclaringSyntaxReferences)
        {
            if (!TryGetCurrentDeclaringSyntax(compilation, syntaxReference, out var syntax, out var semanticModel))
                continue;

            var attributeLists = GetAttributeListsForDeclaration(syntax);

            if (attributeLists is null)
                continue;

            foreach (var attribute in attributeLists.SelectMany(static list => list.Attributes))
            {
                if (attribute.IsMacroAttribute())
                    continue;

                if (!ShouldBindAttributeForOwner(attribute))
                    continue;

                if (this is IMethodSymbol method &&
                    method.MethodKind == MethodKind.Ordinary &&
                    AttributeUsageHelper.HasExplicitTarget(attribute, "return"))
                {
                    continue;
                }

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
                        seenAttributes))
                {
                    builder.Add(data);
                }
            }
        }

        return builder.ToImmutable();
    }

    protected bool TryGetCurrentDeclaringSyntax(
        Compilation compilation,
        SyntaxReference syntaxReference,
        out SyntaxNode syntax,
        out SemanticModel semanticModel)
    {
        if (compilation.TryGetSemanticModel(syntaxReference.SyntaxTree, out semanticModel))
        {
            syntax = syntaxReference.GetSyntax();
            return true;
        }

        var staleSyntax = syntaxReference.GetSyntax();
        var staleFilePath = syntaxReference.SyntaxTree.FilePath;
        if (string.IsNullOrWhiteSpace(staleFilePath))
        {
            syntax = null!;
            semanticModel = null!;
            return false;
        }

        var currentTree = compilation.SyntaxTrees.FirstOrDefault(tree =>
            string.Equals(tree.FilePath, staleFilePath, StringComparison.OrdinalIgnoreCase));
        if (currentTree is null ||
            !compilation.TryGetSemanticModel(currentTree, out semanticModel))
        {
            syntax = null!;
            semanticModel = null!;
            return false;
        }

        var root = currentTree.GetRoot();
        if (!root.FullSpan.Contains(syntaxReference.Span))
        {
            syntax = null!;
            semanticModel = null!;
            return false;
        }

        var candidate = root.FindNode(syntaxReference.Span, getInnermostNodeForTie: true);
        for (var current = candidate; current is not null; current = current.Parent)
        {
            if (current.Kind == staleSyntax.Kind && current.Span == syntaxReference.Span)
            {
                syntax = current;
                return true;
            }
        }

        syntax = null!;
        semanticModel = null!;
        return false;
    }

    private static ParseOptions GetParseOptions(IEnumerable<SyntaxReference> declaringSyntaxReferences)
    {
        var reference = declaringSyntaxReferences.FirstOrDefault();
        if (reference?.SyntaxTree?.Options is { } options)
            return options;

        return new ParseOptions();
    }

    protected AttributeData? CreateCompilerGeneratedAttribute()
    {
        var compilation = GetDeclaringCompilation();
        if (compilation is null)
            return null;

        if (DeclaringSyntaxReferences.IsDefaultOrEmpty)
            return null;

        var syntaxReference = DeclaringSyntaxReferences.FirstOrDefault();
        if (syntaxReference is null)
            return null;

        var attributeType = compilation.GetTypeByMetadataName("System.Runtime.CompilerServices.CompilerGeneratedAttribute");
        if (attributeType is not INamedTypeSymbol namedAttributeType)
            return null;

        var constructor = namedAttributeType.Constructors.FirstOrDefault(c => !c.IsStatic && c.Parameters.Length == 0);
        if (constructor is null)
            return null;

        return new AttributeData(
            namedAttributeType,
            constructor,
            ImmutableArray<TypedConstant>.Empty,
            ImmutableArray<KeyValuePair<string, TypedConstant>>.Empty,
            syntaxReference);
    }

    protected AttributeData? CreateRequiredMemberAttribute()
    {
        var compilation = GetDeclaringCompilation();
        if (compilation is null)
            return null;

        //if (DeclaringSyntaxReferences.IsDefaultOrEmpty)
        //    return null;

        var syntaxReference = DeclaringSyntaxReferences.FirstOrDefault();
        //if (syntaxReference is null)
        //    return null;

        var attributeType = compilation.GetTypeByMetadataName("System.Runtime.CompilerServices.RequiredMemberAttribute");
        if (attributeType is not INamedTypeSymbol namedAttributeType)
            return null;

        var constructor = namedAttributeType.Constructors.FirstOrDefault(c => !c.IsStatic && c.Parameters.Length == 0);
        if (constructor is null)
            return null;

        return new AttributeData(
            namedAttributeType,
            constructor,
            ImmutableArray<TypedConstant>.Empty,
            ImmutableArray<KeyValuePair<string, TypedConstant>>.Empty,
            syntaxReference);
    }

    protected Compilation? GetDeclaringCompilation()
    {
        if (this is SourceAssemblySymbol assemblySymbol)
            return assemblySymbol.Compilation;

        return ContainingAssembly is SourceAssemblySymbol containingAssembly
            ? containingAssembly.Compilation
            : null;
    }

    internal void AddDeclaration(Location location, SyntaxReference reference, bool preferAsPrimary = false)
    {
        if (preferAsPrimary)
        {
            Locations = ImmutableArray.Create(location).AddRange(Locations);
            DeclaringSyntaxReferences = ImmutableArray.Create(reference).AddRange(DeclaringSyntaxReferences);
        }
        else
        {
            Locations = Locations.Add(location);
            DeclaringSyntaxReferences = DeclaringSyntaxReferences.Add(reference);
        }

        RefreshSourceDeclarationCaches();
    }

    private void RefreshSourceDeclarationCaches()
    {
        _lazyCustomAttributes = default;
        _lazyDocumentationComment = null;

        var parseOptions = GetParseOptions(DeclaringSyntaxReferences);
        DocumentationComments = DocumentationCommentUtilities.GetDocumentationComments(DeclaringSyntaxReferences, parseOptions);
        DocumentationComment = DocumentationCommentUtilities.GetMergedDocumentationComment(DeclaringSyntaxReferences, parseOptions);
    }

    private IEnumerable<AttributeListSyntax> GetAttributeListsForDeclaration(SyntaxNode syntax)
        => syntax switch
        {
            CompilationUnitSyntax compilationUnit when this is IAssemblySymbol => compilationUnit
                .AttributeLists
                .OfType<AttributeListSyntax>()
                .Where(static list => HasExplicitTarget(list, "assembly")),
            CompilationUnitSyntax compilationUnit when this is IModuleSymbol => compilationUnit
                .AttributeLists
                .OfType<AttributeListSyntax>()
                .Where(static list => HasExplicitTarget(list, "module")),
            TypeDeclarationSyntax { ParameterList: not null } typeDeclaration when this is SourceMethodSymbol { MethodKind: MethodKind.Constructor } =>
                typeDeclaration.AttributeLists.Where(static list => HasExplicitTarget(list, "method")),
            BaseTypeDeclarationSyntax typeDeclaration when this is ITypeSymbol => typeDeclaration.AttributeLists,
            DelegateDeclarationSyntax delegateDeclaration when this is ITypeSymbol => delegateDeclaration.AttributeLists,
            EnumMemberDeclarationSyntax enumMember => enumMember.AttributeLists,
            MethodDeclarationSyntax methodDeclaration => methodDeclaration.AttributeLists,
            FunctionStatementSyntax functionStatement => functionStatement.AttributeLists,
            ConstructorDeclarationSyntax constructorDeclaration => constructorDeclaration.AttributeLists,
            ParameterlessConstructorDeclarationSyntax initDeclaration => initDeclaration.AttributeLists,
            InitializerBlockDeclarationSyntax initBlockDeclaration => initBlockDeclaration.AttributeLists,
            FinallyDeclarationSyntax finalDeclaration => finalDeclaration.AttributeLists,
            PropertyDeclarationSyntax propertyDeclaration => propertyDeclaration.AttributeLists,
            IndexerDeclarationSyntax indexerDeclaration => indexerDeclaration.AttributeLists,
            EventDeclarationSyntax eventDeclaration => eventDeclaration.AttributeLists,
            AccessorDeclarationSyntax accessorDeclaration => accessorDeclaration.AttributeLists,
            FieldDeclarationSyntax fieldDeclaration => fieldDeclaration.AttributeLists,
            ConstDeclarationSyntax constDeclaration => constDeclaration.AttributeLists,
            VariableDeclaratorSyntax variableDeclarator when variableDeclarator.Parent?.Parent is FieldDeclarationSyntax field => field.AttributeLists,
            VariableDeclaratorSyntax variableDeclarator when variableDeclarator.Parent?.Parent is ConstDeclarationSyntax @const => @const.AttributeLists,
            ParameterSyntax parameter => parameter.AttributeLists,
            ArrowTypeClauseSyntax arrowTypeClause => arrowTypeClause.AttributeLists,
            _ => Enumerable.Empty<AttributeListSyntax>()
        };

    private bool ShouldBindAttributeForOwner(AttributeSyntax attribute)
    {
        if (attribute.Parent is not AttributeListSyntax list || list.Target is null)
            return true;

        if (this is IAssemblySymbol)
            return HasExplicitTarget(list, "assembly") && IsAssemblyAttributeDeclarationContext(list);

        if (this is IModuleSymbol)
            return HasExplicitTarget(list, "module") && IsAssemblyAttributeDeclarationContext(list);

        if (this is ITypeSymbol &&
            list.Parent is TypeDeclarationSyntax { ParameterList: not null } &&
            HasExplicitTarget(list, "method"))
        {
            return false;
        }

        if (this is IFieldSymbol { AssociatedSymbol: IPropertySymbol or IEventSymbol } &&
            list.Parent is PropertyDeclarationSyntax or EventDeclarationSyntax)
        {
            return HasExplicitTarget(list, "field");
        }

        if (this is SourcePropertySymbol &&
            HasExplicitTarget(list, "field"))
        {
            return false;
        }

        if (this is SourceEventSymbol &&
            HasExplicitTarget(list, "field"))
        {
            return false;
        }

        return true;
    }

    private static bool HasExplicitTarget(AttributeListSyntax list, string targetName)
        => string.Equals(
            list.Target?.Identifier.ValueText,
            targetName,
            StringComparison.OrdinalIgnoreCase);

    private static bool IsAssemblyAttributeDeclarationContext(AttributeListSyntax list)
        => list.Parent is CompilationUnitSyntax;
}
