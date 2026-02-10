using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private readonly Dictionary<SyntaxTree, SemanticModel> _semanticModels = new();

    /// <summary>
    /// Gets completion items available at a position in a syntax tree within this compilation.
    /// </summary>
    /// <param name="syntaxTree">The syntax tree to query.</param>
    /// <param name="position">The zero-based position in the syntax tree.</param>
    /// <returns>A sequence of completion items.</returns>
    public IEnumerable<CompletionItem> GetCompletions(SyntaxTree syntaxTree, int position)
    {
        ArgumentNullException.ThrowIfNull(syntaxTree);
        return GetSemanticModel(syntaxTree).GetCompletions(position);
    }

    public SemanticModel GetSemanticModel(SyntaxTree syntaxTree)
    {
        EnsureSetup();

        EnsureSemanticModelsCreated();
        EnsureSourceDeclarationsComplete();

        if (_semanticModels.TryGetValue(syntaxTree, out var semanticModel))
        {
            return semanticModel;
        }

        if (!_syntaxTrees.Contains(syntaxTree))
        {
            throw new ArgumentNullException(nameof(syntaxTree), "Syntax tree is not part of compilation");
        }

        semanticModel = new SemanticModel(this, syntaxTree);
        _semanticModels[syntaxTree] = semanticModel;
        return semanticModel;
    }

    internal void EnsureSourceDeclarationsComplete()
    {
        EnsureSetup();

        if (_sourceDeclarationsComplete || _isDeclaringSourceTypes)
            return;

        lock (_declarationGate)
        {
            if (_sourceDeclarationsComplete || _isDeclaringSourceTypes)
                return;

            _isDeclaringSourceTypes = true;
            try
            {
                EnsureSemanticModelsCreated();

                foreach (var model in _semanticModels.Values)
                    model.EnsureDeclarations();

                EnsureDefaultConstructorsDeclared();

                foreach (var model in _semanticModels.Values)
                    model.EnsureRootBinderCreated();

                _sourceDeclarationsComplete = true;
            }
            finally
            {
                _isDeclaringSourceTypes = false;
            }
        }
    }

    private void EnsureSemanticModelsCreated()
    {
        if (_sourceTypesInitialized || _isPopulatingSourceTypes)
            return;

        try
        {
            _isPopulatingSourceTypes = true;

            foreach (var syntaxTree in _syntaxTrees)
            {
                if (_semanticModels.ContainsKey(syntaxTree))
                    continue;

                var model = new SemanticModel(this, syntaxTree);
                _semanticModels[syntaxTree] = model;
            }

            _sourceTypesInitialized = true;
        }
        finally
        {
            _isPopulatingSourceTypes = false;
        }
    }

    private void EnsureDefaultConstructorsDeclared()
    {
        var constructorFlags = new Dictionary<ISymbol, ConstructorDeclarationFlags>(SymbolEqualityComparer.Default);

        foreach (var syntaxTree in _syntaxTrees)
        {
            if (!_semanticModels.TryGetValue(syntaxTree, out var model))
                continue;

            if (syntaxTree.GetRoot() is not CompilationUnitSyntax root)
                continue;

            var typeDeclarations = root.DescendantNodes()
                .OfType<TypeDeclarationSyntax>()
                .Where(typeDecl => typeDecl is ClassDeclarationSyntax or StructDeclarationSyntax or RecordDeclarationSyntax);

            foreach (var classDecl in typeDeclarations)
            {
                var symbol = model.GetDeclaredTypeSymbolForDeclaration(classDecl);

                constructorFlags.TryGetValue(symbol, out var flags);

                if (classDecl is ClassDeclarationSyntax { ParameterList: not null } or RecordDeclarationSyntax { ParameterList: not null })
                    flags.HasPrimaryConstructor = true;

                if (classDecl.Members.OfType<ConstructorDeclarationSyntax>()
                    .Any(c => !c.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword)))
                {
                    flags.HasExplicitInstanceConstructor = true;
                }

                if (classDecl.Members.OfType<NamedConstructorDeclarationSyntax>().Any())
                    flags.HasNamedConstructor = true;

                constructorFlags[symbol] = flags;
            }
        }

        foreach (var (symbol, flags) in constructorFlags)
        {
            if (symbol is not SourceNamedTypeSymbol sourceType)
                continue;

            if (sourceType.IsStatic)
                continue;

            if (flags.HasPrimaryConstructor || flags.HasExplicitInstanceConstructor || flags.HasNamedConstructor)
                continue;

            if (sourceType.Constructors.Any(c => !c.IsStatic && c.Parameters.Length == 0))
                continue;

            var location = sourceType.Locations.FirstOrDefault() ?? Location.None;
            var reference = sourceType.DeclaringSyntaxReferences.FirstOrDefault();
            var references = reference is null ? Array.Empty<SyntaxReference>() : new[] { reference };

            _ = new SourceMethodSymbol(
                ".ctor",
                GetSpecialType(SpecialType.System_Unit),
                ImmutableArray<SourceParameterSymbol>.Empty,
                sourceType,
                sourceType,
                sourceType.ContainingNamespace?.AsSourceNamespace(),
                new[] { location },
                references,
                isStatic: false,
                methodKind: MethodKind.Constructor,
                declaredAccessibility: Accessibility.Public);
        }
    }

    private struct ConstructorDeclarationFlags
    {
        public bool HasPrimaryConstructor;
        public bool HasExplicitInstanceConstructor;
        public bool HasNamedConstructor;
    }
}
