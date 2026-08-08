using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

using Raven.CodeAnalysis.Macros;
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
    /// Gets macro signature help at a position in a syntax tree within this compilation.
    /// </summary>
    public MacroSignatureHelp? GetMacroSignatureHelp(SyntaxTree syntaxTree, int position)
    {
        ArgumentNullException.ThrowIfNull(syntaxTree);
        return GetSemanticModel(syntaxTree).GetMacroSignatureHelp(position);
    }

    /// <summary>
    /// Gets ordinary Raven fragment regions reported for a token-tree macro invocation.
    /// </summary>
    public ImmutableArray<MacroFragmentRegion> GetMacroFragmentRegions(
        FreestandingMacroExpressionSyntax expression,
        CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(expression);
        var syntaxTree = expression.SyntaxTree
            ?? throw new ArgumentException("Macro invocation is not attached to a syntax tree.", nameof(expression));
        return GetSemanticModel(syntaxTree).GetMacroFragmentRegions(expression, cancellationToken);
    }

    /// <summary>
    /// Gets the token stream and optional classifications for a token-tree macro invocation.
    /// </summary>
    public ImmutableArray<MacroTokenInfo> GetMacroTokens(
        FreestandingMacroExpressionSyntax expression,
        CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(expression);
        var syntaxTree = expression.SyntaxTree
            ?? throw new ArgumentException("Macro invocation is not attached to a syntax tree.", nameof(expression));
        return GetSemanticModel(syntaxTree).GetMacroTokens(expression, cancellationToken);
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
        ArgumentNullException.ThrowIfNull(syntaxTree);

        EnsureSetup();

        if (_macroSyntaxTrees.Contains(syntaxTree))
            return _macroSignatureCompilation!.GetSemanticModel(syntaxTree);

        return GetOrCreateSemanticModel(syntaxTree);
    }

    /// <summary>
    /// Gets the semantic model that owns an authored position, including when
    /// <paramref name="syntaxTree"/> was split into local macro and consumer
    /// projections.
    /// </summary>
    public SemanticModel GetSemanticModel(SyntaxTree syntaxTree, int position)
    {
        ArgumentNullException.ThrowIfNull(syntaxTree);
        if ((uint)position > (uint)syntaxTree.Length)
            throw new ArgumentOutOfRangeException(nameof(position));

        EnsureSetup();

        if (_syntaxTrees.Contains(syntaxTree) || _macroSyntaxTrees.Contains(syntaxTree))
            return GetSemanticModel(syntaxTree);

        var preferMacro = LocalMacroSyntaxClassifier.IsLocalMacroPosition(syntaxTree, position);
        var primaryTrees = preferMacro ? _macroSyntaxTrees : _syntaxTrees;
        var secondaryTrees = preferMacro ? _syntaxTrees : _macroSyntaxTrees;
        var projectedTree = FindProjectedSemanticTree(
            syntaxTree,
            primaryTrees,
            secondaryTrees,
            preferMacro);
        if (projectedTree is null)
            throw new ArgumentException("Syntax tree is not part of compilation", nameof(syntaxTree));

        return GetSemanticModel(projectedTree);
    }

    internal bool TryGetSemanticModel(SyntaxTree syntaxTree, out SemanticModel semanticModel)
    {
        EnsureSetup();

        if (_macroSyntaxTrees.Contains(syntaxTree))
            return _macroSignatureCompilation!.TryGetSemanticModel(syntaxTree, out semanticModel);

        if (!_syntaxTrees.Contains(syntaxTree) &&
            !_generatedSemanticModels.ContainsKey(syntaxTree))
        {
            semanticModel = null!;
            return false;
        }

        semanticModel = GetOrCreateSemanticModel(syntaxTree);
        return true;
    }

    internal bool TryGetExistingSemanticModel(SyntaxTree syntaxTree, out SemanticModel semanticModel)
    {
        if (_macroSyntaxTrees.Contains(syntaxTree) &&
            _macroSignatureCompilation is not null)
        {
            return _macroSignatureCompilation.TryGetExistingSemanticModel(syntaxTree, out semanticModel);
        }

        if (_generatedSemanticModels.TryGetValue(syntaxTree, out semanticModel!))
            return true;

        if (_semanticModels.TryGetValue(syntaxTree, out semanticModel!))
            return true;

        semanticModel = null!;
        return false;
    }

    internal ImmutableArray<string> GetObservedMacroFilePaths()
        => _semanticModels.Values
            .Concat(_generatedSemanticModels.Values)
            .SelectMany(static model => model.GetObservedMacroFilePaths())
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .Order(StringComparer.OrdinalIgnoreCase)
            .ToImmutableArray();

    internal bool TryGetSemanticModelForDeclarationBinding(SyntaxTree syntaxTree, out SemanticModel semanticModel)
    {
        EnsureSetup();

        if (_macroSyntaxTrees.Contains(syntaxTree))
            return _macroSignatureCompilation!.TryGetSemanticModelForDeclarationBinding(syntaxTree, out semanticModel);

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
            throw new ArgumentException("Syntax tree is not part of compilation", nameof(syntaxTree));
        }

        return _semanticModels.GetOrAdd(syntaxTree, tree => new SemanticModel(this, tree));
    }

    private static SyntaxTree? FindProjectedSemanticTree(
        SyntaxTree authoredTree,
        SyntaxTree[] primaryTrees,
        SyntaxTree[] secondaryTrees,
        bool preferMacro)
    {
        var partition = LocalMacroSyntaxClassifier.Partition(authoredTree);
        var projectedTree = preferMacro
            ? partition.MacroTree ?? partition.ConsumerTree
            : partition.ConsumerTree ?? partition.MacroTree;
        var projectedText = projectedTree?.GetText()?.ToString();
        if (projectedText is null)
            return null;

        if (!string.IsNullOrWhiteSpace(authoredTree.FilePath))
        {
            var pathMatch = FindTreeByPathAndText(
                primaryTrees,
                authoredTree.FilePath,
                projectedText);
            pathMatch ??= FindTreeByPathAndText(
                secondaryTrees,
                authoredTree.FilePath,
                projectedText);
            if (pathMatch is not null)
                return pathMatch;
        }

        return primaryTrees
            .Concat(secondaryTrees)
            .FirstOrDefault(compilationTree =>
                string.Equals(
                    compilationTree.GetText()?.ToString(),
                    projectedText,
                    StringComparison.Ordinal));

        static SyntaxTree? FindTreeByPathAndText(
            IEnumerable<SyntaxTree> trees,
            string filePath,
            string projectedText)
            => trees.FirstOrDefault(compilationTree =>
                !string.IsNullOrWhiteSpace(compilationTree.FilePath) &&
                string.Equals(
                    compilationTree.FilePath,
                    filePath,
                    StringComparison.OrdinalIgnoreCase) &&
                string.Equals(
                    compilationTree.GetText()?.ToString(),
                    projectedText,
                    StringComparison.Ordinal));
    }

    internal SemanticModel CreateTransientSemanticModel(SyntaxTree syntaxTree)
    {
        EnsureSetup();

        if (_macroSyntaxTrees.Contains(syntaxTree))
            return _macroSignatureCompilation!.CreateTransientSemanticModel(syntaxTree);

        EnsureSourceDeclarationsComplete();

        if (_generatedSemanticModels.TryGetValue(syntaxTree, out var generatedSemanticModel))
            return generatedSemanticModel;

        if (!_syntaxTrees.Contains(syntaxTree))
            throw new ArgumentException("Syntax tree is not part of compilation", nameof(syntaxTree));

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

                _sourceTypeDeclarationsDeclared = true;

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

    internal void EnsureSourceTypeDeclarationsDeclared()
    {
        EnsureSetup();

        if (_sourceTypeDeclarationsDeclared || _sourceDeclarationsDeclared)
            return;

        var currentThreadId = Environment.CurrentManagedThreadId;
        if (_isDeclaringSourceTypes && _sourceDeclarationThreadId == currentThreadId)
            return;

        lock (_declarationGate)
        {
            while (_isDeclaringSourceTypes && _sourceDeclarationThreadId != currentThreadId)
                Monitor.Wait(_declarationGate);

            if (_sourceTypeDeclarationsDeclared || _sourceDeclarationsDeclared || _isDeclaringSourceTypes)
                return;

            _isDeclaringSourceTypes = true;
            _sourceDeclarationThreadId = currentThreadId;
            try
            {
                EnsureSemanticModelsCreated();
                var semanticModels = _semanticModels.Values.ToArray();

                foreach (var model in semanticModels)
                    model.EnsureDeclarations();

                _sourceTypeDeclarationsDeclared = true;
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
                .Where(typeDecl => !typeDecl.Ancestors().OfType<UnionDeclarationSyntax>().Any())
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
