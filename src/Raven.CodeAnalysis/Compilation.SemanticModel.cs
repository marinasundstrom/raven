using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private readonly ConcurrentDictionary<SyntaxTree, SemanticModel> _semanticModels = new();
    private readonly ConcurrentDictionary<SyntaxTree, SemanticModel> _generatedSemanticModels = new();

    internal bool SourceDeclarationsComplete => _sourceDeclarationsComplete;

    internal bool SourceDeclarationsDeclared => _sourceDeclarationsDeclared;

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

    /// <summary>
    /// Gets completion items available at a position in a syntax tree within this compilation asynchronously.
    /// </summary>
    /// <param name="syntaxTree">The syntax tree to query.</param>
    /// <param name="position">The zero-based position in the syntax tree.</param>
    /// <param name="cancellationToken">Token used to cancel the operation.</param>
    /// <returns>A materialized set of completion items.</returns>
    public Task<ImmutableArray<CompletionItem>> GetCompletionsAsync(
        SyntaxTree syntaxTree,
        int position,
        CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(syntaxTree);
        return GetSemanticModel(syntaxTree).GetCompletionsAsync(position, cancellationToken);
    }

    public SemanticModel GetSemanticModel(SyntaxTree syntaxTree)
    {
        EnsureSetup();

        return GetOrCreateSemanticModel(syntaxTree);
    }

    internal bool TryGetSemanticModel(SyntaxTree syntaxTree, out SemanticModel semanticModel)
    {
        EnsureSetup();

        if (!_syntaxTrees.Contains(syntaxTree) &&
            !_generatedSemanticModels.ContainsKey(syntaxTree))
        {
            semanticModel = null!;
            return false;
        }

        semanticModel = GetOrCreateSemanticModel(syntaxTree);
        return true;
    }

    internal bool TryGetSemanticModelForDeclarationBinding(SyntaxTree syntaxTree, out SemanticModel semanticModel)
    {
        EnsureSetup();

        if (!_syntaxTrees.Contains(syntaxTree) &&
            !_generatedSemanticModels.ContainsKey(syntaxTree))
        {
            semanticModel = null!;
            return false;
        }

        semanticModel = GetOrCreateSemanticModel(syntaxTree);
        return true;
    }

    private SemanticModel GetOrCreateSemanticModel(SyntaxTree syntaxTree)
    {
        if (_generatedSemanticModels.TryGetValue(syntaxTree, out var semanticModel))
        {
            return semanticModel;
        }

        if (!_syntaxTrees.Contains(syntaxTree))
        {
            throw new ArgumentNullException(nameof(syntaxTree), "Syntax tree is not part of compilation");
        }

        return _semanticModels.GetOrAdd(syntaxTree, tree => new SemanticModel(this, tree));
    }

    internal SemanticModel CreateTransientSemanticModel(SyntaxTree syntaxTree)
    {
        EnsureSetup();
        EnsureSourceDeclarationsComplete();

        if (_generatedSemanticModels.TryGetValue(syntaxTree, out var generatedSemanticModel))
            return generatedSemanticModel;

        if (!_syntaxTrees.Contains(syntaxTree))
            throw new ArgumentNullException(nameof(syntaxTree), "Syntax tree is not part of compilation");

        return new SemanticModel(this, syntaxTree);
    }

    internal void RegisterGeneratedSyntaxTree(SyntaxTree syntaxTree, SemanticModel semanticModel)
    {
        ArgumentNullException.ThrowIfNull(syntaxTree);
        ArgumentNullException.ThrowIfNull(semanticModel);

        if (_syntaxTrees.Contains(syntaxTree))
            return;

        _generatedSemanticModels[syntaxTree] = semanticModel;
    }

    internal void EnsureSourceDeclarationsComplete()
    {
        EnsureSetup();

        if (_sourceDeclarationsComplete)
            return;

        var currentThreadId = Environment.CurrentManagedThreadId;
        if (_isDeclaringSourceTypes && _sourceDeclarationThreadId == currentThreadId)
            return;

        PerformanceInstrumentation.Setup.RecordEnsureSourceDeclarationsCompleteCall();

        EnsureSourceDeclarationsDeclared();

        if (_sourceDeclarationsComplete)
            return;

        lock (_declarationGate)
        {
            while (_isDeclaringSourceTypes && _sourceDeclarationThreadId != currentThreadId)
                Monitor.Wait(_declarationGate);

            if (_sourceDeclarationsComplete || _isDeclaringSourceTypes)
                return;

            _isDeclaringSourceTypes = true;
            _sourceDeclarationThreadId = currentThreadId;
            try
            {
                EnsureSemanticModelsCreated();
                var semanticModels = _semanticModels.Values.ToArray();

                foreach (var model in semanticModels)
                    model.EnsureCompilationUnitDeclarationBindersCreated();

                _sourceDeclarationsComplete = true;
            }
            finally
            {
                _sourceDeclarationThreadId = 0;
                _isDeclaringSourceTypes = false;
                Monitor.PulseAll(_declarationGate);
            }
        }
    }

    internal void EnsureSourceDeclarationsDeclared()
    {
        EnsureSetup();

        if (_sourceDeclarationsDeclared)
            return;

        var currentThreadId = Environment.CurrentManagedThreadId;
        if (_isDeclaringSourceTypes && _sourceDeclarationThreadId == currentThreadId)
            return;

        PerformanceInstrumentation.Setup.RecordEnsureSourceDeclarationsDeclaredCall();

        lock (_declarationGate)
        {
            while (_isDeclaringSourceTypes && _sourceDeclarationThreadId != currentThreadId)
                Monitor.Wait(_declarationGate);

            if (_sourceDeclarationsDeclared || _isDeclaringSourceTypes)
                return;

            _isDeclaringSourceTypes = true;
            _sourceDeclarationThreadId = currentThreadId;
            try
            {
                EnsureSemanticModelsCreated();
                var semanticModels = _semanticModels.Values.ToArray();

                foreach (var model in semanticModels)
                    model.EnsureDeclarations();

                foreach (var model in semanticModels)
                    model.EnsureMemberSignaturesDeclared();

                EnsureDefaultConstructorsDeclared();

                _sourceDeclarationsDeclared = true;
            }
            finally
            {
                _sourceDeclarationThreadId = 0;
                _isDeclaringSourceTypes = false;
                Monitor.PulseAll(_declarationGate);
            }
        }
    }

    private void EnsureSemanticModelsCreated()
    {
        PerformanceInstrumentation.Setup.RecordEnsureSemanticModelsCreatedCall();

        if (_sourceTypesInitialized)
            return;

        var currentThreadId = Environment.CurrentManagedThreadId;

        lock (_semanticModelSetupGate)
        {
            while (_isPopulatingSourceTypes && _semanticModelSetupThreadId != currentThreadId)
                Monitor.Wait(_semanticModelSetupGate);

            if (_sourceTypesInitialized || _isPopulatingSourceTypes)
                return;

            _isPopulatingSourceTypes = true;
            _semanticModelSetupThreadId = currentThreadId;

            try
            {
                foreach (var syntaxTree in _syntaxTrees)
                {
                    _semanticModels.GetOrAdd(syntaxTree, tree =>
                    {
                        PerformanceInstrumentation.Setup.RecordSemanticModelCreated();
                        return new SemanticModel(this, tree);
                    });
                }

                _sourceTypesInitialized = true;
            }
            finally
            {
                _semanticModelSetupThreadId = 0;
                _isPopulatingSourceTypes = false;
                Monitor.PulseAll(_semanticModelSetupGate);
            }
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
                .Where(typeDecl => typeDecl.Parent is not TypeDeclarationStatementSyntax)
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

                constructorFlags[symbol] = flags;
            }
        }

        foreach (var (symbol, flags) in constructorFlags)
        {
            if (symbol is not SourceNamedTypeSymbol sourceType)
                continue;

            if (sourceType is IUnionSymbol)
                continue;

            if (sourceType.IsStatic)
                continue;

            if (flags.HasPrimaryConstructor || flags.HasExplicitInstanceConstructor)
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
    }
}
