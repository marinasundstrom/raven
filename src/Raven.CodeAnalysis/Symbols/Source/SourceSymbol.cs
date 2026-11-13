using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Symbols;

internal abstract partial class SourceSymbol : SymbolBase
{
    private ImmutableArray<AttributeData> _lazyCustomAttributes;
    private ImmutableArray<SourceAttributeBinding> _lazyBoundAttributes;

    private DocumentationComment? _lazyDocumentationComment;

    internal ImmutableArray<DocumentationComment> DocumentationComments { get; private set; }
    internal DocumentationComment? DocumentationComment { get; private set; }

    protected SourceSymbol(
        SymbolKind kind,
        string name,
        Symbol? containingSymbol,
        NamedTypeSymbol? containingType,
        NamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        Accessibility declaredAccessibility = Accessibility.NotApplicable,
        bool addAsMember = true)
        : base(kind, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences, declaredAccessibility, addAsMember)
    {
        var parseOptions = GetParseOptions(declaringSyntaxReferences);
        DocumentationComments = DocumentationCommentUtilities.GetDocumentationComments(declaringSyntaxReferences, parseOptions);
        DocumentationComment = DocumentationCommentUtilities.GetMergedDocumentationComment(declaringSyntaxReferences, parseOptions);
    }

    public override AssemblySymbol? ContainingAssembly => ContainingNamespace?.ContainingAssembly;

    public override ModuleSymbol? ContainingModule => ContainingNamespace?.ContainingModule;

    public override DocumentationComment? GetDocumentationComment()
    {
        if (_lazyDocumentationComment is not null)
            return _lazyDocumentationComment;

        _lazyDocumentationComment = DocumentationComment;
        return _lazyDocumentationComment;
    }

    private bool _provisionalSourceCoreAttributes;
    private void RefreshSourceCoreAttributes()
    {
        var compilation = GetDeclaringServices();
        if (compilation?.UsesDeferredSourceAttributes != true)
            return;
        if (_provisionalSourceCoreAttributes && compilation.SourceDeclarationsComplete)
        {
            _lazyCustomAttributes = default;
            _lazyBoundAttributes = default;
        }
        _provisionalSourceCoreAttributes = !compilation.SourceDeclarationsComplete;
    }

    public override ImmutableArray<AttributeData> GetAttributes()
    {
        RefreshSourceCoreAttributes();
        if (_provisionalSourceCoreAttributes)
            return ImmutableArray<AttributeData>.Empty;
        if (_lazyCustomAttributes.IsDefault)
            _lazyCustomAttributes = ComputeAttributes();

        return _lazyCustomAttributes;
    }

    // Usage policies need bound data before validation: AttributeUsageAttribute may
    // describe itself, and other attribute classes may refer to each other.
    internal ImmutableArray<AttributeData> GetAttributesForUsageValidation()
        => GetBoundAttributes().Select(entry => entry.Data).ToImmutableArray();

    private ImmutableArray<SourceAttributeBinding> GetBoundAttributes()
    {
        RefreshSourceCoreAttributes();
        if (_provisionalSourceCoreAttributes)
            return ImmutableArray<SourceAttributeBinding>.Empty;
        if (_lazyBoundAttributes.IsDefault)
            _lazyBoundAttributes = BindAttributes();
        return _lazyBoundAttributes;
    }

    private ImmutableArray<AttributeData> ComputeAttributes()
    {
        var compilation = GetDeclaringServices();
        if (compilation is null)
            return ImmutableArray<AttributeData>.Empty;
        var builder = ImmutableArray.CreateBuilder<AttributeData>();
        var seen = new Dictionary<AttributeTargets, HashSet<NamedTypeSymbol>>();
        var defaultTarget = AttributeUsageHelper.GetDefaultTargetForOwner(this);
        foreach (var entry in GetBoundAttributes())
        {
            var valid = AttributeUsageHelper.TryValidateAttribute(compilation, entry.Diagnostics,
                this, entry.Syntax, entry.Data, defaultTarget, seen);
            if (compilation.IsNamespaceContainer(this))
                compilation.PublishAttributeDiagnostics(entry);
            if (valid)
                builder.Add(entry.Data);
        }
        return builder.ToImmutable();
    }

    private ImmutableArray<SourceAttributeBinding> BindAttributes()
    {
        if (DeclaringSyntaxReferences.IsDefaultOrEmpty)
            return ImmutableArray<SourceAttributeBinding>.Empty;

        var compilation = GetDeclaringServices();
        if (compilation is null)
            return ImmutableArray<SourceAttributeBinding>.Empty;

        var builder = ImmutableArray.CreateBuilder<SourceAttributeBinding>();
        var processedAttributeLocations = new HashSet<(SyntaxTree SyntaxTree, int SpanStart, int SpanLength)>();
        IEnumerable<SyntaxReference> declaringSyntaxReferences = DeclaringSyntaxReferences;
        if (compilation.IsNamespaceContainer(this))
        {
            var syntaxTreeOrder = compilation.SyntaxTrees
                .Select(static (tree, index) => (tree, index))
                .ToDictionary(static item => item.tree, static item => item.index);
            declaringSyntaxReferences = declaringSyntaxReferences
                .OrderBy(reference => syntaxTreeOrder[reference.SyntaxTree])
                .ThenBy(static reference => reference.Span.Start);
        }

        foreach (var syntaxReference in declaringSyntaxReferences)
        {
            if (!compilation.TryGetCurrentDeclaringSyntax(syntaxReference, out var syntax))
                continue;

            var attributeLists = GetAttributeListsForDeclaration(syntax);

            if (attributeLists is null)
                continue;

            foreach (var attribute in attributeLists.SelectMany(static list => list.Attributes))
            {
                if (!processedAttributeLocations.Add((attribute.SyntaxTree, attribute.Span.Start, attribute.Span.Length)))
                    continue;

                if (attribute.IsMacroAttribute())
                    continue;

                if (!ShouldBindAttributeForOwner(attribute))
                    continue;

                if (this is MethodSymbol method &&
                    method.MethodKind == MethodKind.Ordinary &&
                    AttributeUsageHelper.HasExplicitTarget(attribute, "return"))
                {
                    continue;
                }

                if (compilation.BindAttribute(this, attribute) is { } entry)
                    builder.Add(entry);
            }
        }

        return builder.ToImmutable();
    }

    private static ParseOptions GetParseOptions(IEnumerable<SyntaxReference> declaringSyntaxReferences)
    {
        var reference = declaringSyntaxReferences.FirstOrDefault();
        if (reference?.SyntaxTree?.Options is { } options)
            return options;

        return new ParseOptions();
    }

    protected AttributeData? CreateCompilerGeneratedAttribute()
        => CreateCompilerMarkerAttribute("System.Runtime.CompilerServices.CompilerGeneratedAttribute");

    protected AttributeData? CreateIsReadOnlyAttribute()
        => CreateCompilerMarkerAttribute("System.Runtime.CompilerServices.IsReadOnlyAttribute");

    private AttributeData? CreateCompilerMarkerAttribute(string metadataName)
    {
        var attributeType = GetDeclaringServices()?.GetTypeByMetadataName(metadataName);
        var constructor = attributeType?.Constructors.FirstOrDefault(c => !c.IsStatic && c.Parameters.Length == 0);
        return attributeType is null || constructor is null ? null : new AttributeData(
            attributeType, constructor, ImmutableArray<TypedConstant>.Empty,
            ImmutableArray<KeyValuePair<string, TypedConstant>>.Empty,
            DeclaringSyntaxReferences.FirstOrDefault());
    }

    protected AttributeData? CreateRequiredMemberAttribute()
        => CreateCompilerMarkerAttribute("System.Runtime.CompilerServices.RequiredMemberAttribute");

    protected SourceDeclarationServices? GetDeclaringServices()
        => this is SourceDeclarationServiceProvider provider
            ? provider.DeclarationServices
            : (ContainingAssembly as SourceDeclarationServiceProvider)?.DeclarationServices;

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
        _lazyBoundAttributes = default;
        _lazyDocumentationComment = null;

        var parseOptions = GetParseOptions(DeclaringSyntaxReferences);
        DocumentationComments = DocumentationCommentUtilities.GetDocumentationComments(DeclaringSyntaxReferences, parseOptions);
        DocumentationComment = DocumentationCommentUtilities.GetMergedDocumentationComment(DeclaringSyntaxReferences, parseOptions);
    }

    private SourceAttributeDeclarationContext GetAttributeDeclarationContext()
        => GetDeclaringServices()?.GetAttributeDeclarationContext(this) ?? default;

    private IEnumerable<AttributeListSyntax> GetAttributeListsForDeclaration(SyntaxNode syntax)
        => SourceAttributeDeclarationFacts.GetAttributeListsForDeclaration(this, GetAttributeDeclarationContext(), syntax);

    private bool ShouldBindAttributeForOwner(AttributeSyntax attribute)
        => SourceAttributeDeclarationFacts.ShouldBindAttributeForOwner(this, GetAttributeDeclarationContext(), attribute);
}
