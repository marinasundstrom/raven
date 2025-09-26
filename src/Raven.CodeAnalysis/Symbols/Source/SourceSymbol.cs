using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Symbols;

internal abstract class SourceSymbol : Symbol
{
    private ImmutableArray<AttributeData> _lazyCustomAttributes;

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
    }

    public override IAssemblySymbol ContainingAssembly => ContainingNamespace?.ContainingAssembly;

    public override IModuleSymbol ContainingModule => ContainingNamespace?.ContainingModule;

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

        foreach (var syntaxReference in DeclaringSyntaxReferences)
        {
            var syntax = syntaxReference.GetSyntax();
            var attributeLists = GetAttributeListsForDeclaration(syntax);

            if (attributeLists.Count == 0)
                continue;

            var semanticModel = compilation.GetSemanticModel(syntax.SyntaxTree);

            foreach (var attribute in attributeLists.SelectMany(static list => list.Attributes))
            {
                var data = semanticModel.BindAttribute(attribute);
                if (data is not null)
                    builder.Add(data);
            }
        }

        return builder.ToImmutable();
    }

    private Compilation? GetDeclaringCompilation()
    {
        if (this is SourceAssemblySymbol assemblySymbol)
            return assemblySymbol.Compilation;

        return ContainingAssembly is SourceAssemblySymbol containingAssembly
            ? containingAssembly.Compilation
            : null;
    }

    private static SyntaxList<AttributeListSyntax> GetAttributeListsForDeclaration(SyntaxNode syntax)
        => syntax switch
        {
            CompilationUnitSyntax compilationUnit => compilationUnit.AttributeLists,
            BaseTypeDeclarationSyntax typeDeclaration => typeDeclaration.AttributeLists,
            EnumMemberDeclarationSyntax enumMember => enumMember.AttributeLists,
            MethodDeclarationSyntax methodDeclaration => methodDeclaration.AttributeLists,
            ConstructorDeclarationSyntax constructorDeclaration => constructorDeclaration.AttributeLists,
            NamedConstructorDeclarationSyntax namedConstructorDeclaration => namedConstructorDeclaration.AttributeLists,
            PropertyDeclarationSyntax propertyDeclaration => propertyDeclaration.AttributeLists,
            IndexerDeclarationSyntax indexerDeclaration => indexerDeclaration.AttributeLists,
            AccessorDeclarationSyntax accessorDeclaration => accessorDeclaration.AttributeLists,
            FieldDeclarationSyntax fieldDeclaration => fieldDeclaration.AttributeLists,
            VariableDeclaratorSyntax variableDeclarator when variableDeclarator.Parent?.Parent is FieldDeclarationSyntax field => field.AttributeLists,
            _ => default
        };
}
