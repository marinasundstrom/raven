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
            var syntax = syntaxReference.GetSyntax();
            var attributeLists = GetAttributeListsForDeclaration(syntax);

            if (attributeLists.Count == 0)
                continue;

            var semanticModel = compilation.GetSemanticModel(syntax.SyntaxTree);

            foreach (var attribute in attributeLists.SelectMany(static list => list.Attributes))
            {
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

    private static ParseOptions GetParseOptions(IEnumerable<SyntaxReference> declaringSyntaxReferences)
    {
        var reference = declaringSyntaxReferences.FirstOrDefault();
        if (reference is not null && reference.SyntaxTree.Options is { } options)
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

    private SyntaxList<AttributeListSyntax> GetAttributeListsForDeclaration(SyntaxNode syntax)
        => syntax switch
        {
            CompilationUnitSyntax compilationUnit when this is IAssemblySymbol => compilationUnit.AttributeLists,
            BaseTypeDeclarationSyntax typeDeclaration when this is ITypeSymbol => typeDeclaration.AttributeLists,
            EnumMemberDeclarationSyntax enumMember => enumMember.AttributeLists,
            MethodDeclarationSyntax methodDeclaration => methodDeclaration.AttributeLists,
            FunctionStatementSyntax functionStatement => functionStatement.AttributeLists,
            ConstructorDeclarationSyntax constructorDeclaration => constructorDeclaration.AttributeLists,
            NamedConstructorDeclarationSyntax namedConstructorDeclaration => namedConstructorDeclaration.AttributeLists,
            PropertyDeclarationSyntax propertyDeclaration => propertyDeclaration.AttributeLists,
            IndexerDeclarationSyntax indexerDeclaration => indexerDeclaration.AttributeLists,
            EventDeclarationSyntax eventDeclaration => eventDeclaration.AttributeLists,
            AccessorDeclarationSyntax accessorDeclaration => accessorDeclaration.AttributeLists,
            FieldDeclarationSyntax fieldDeclaration => fieldDeclaration.AttributeLists,
            VariableDeclaratorSyntax variableDeclarator when variableDeclarator.Parent?.Parent is FieldDeclarationSyntax field => field.AttributeLists,
            ParameterSyntax parameter => parameter.AttributeLists,
            ArrowTypeClauseSyntax arrowTypeClause => arrowTypeClause.AttributeLists,
            _ => default
        };
}
