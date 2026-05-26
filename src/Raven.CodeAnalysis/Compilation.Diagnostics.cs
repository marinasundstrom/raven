using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    public ImmutableArray<Diagnostic> GetDiagnostics(CompilationWithAnalyzersOptions? analyzerOptions = null, CancellationToken cancellationToken = default)
    {
        var diagnostics = new List<Diagnostic>();

        EnsureSetup();

        foreach (var syntaxTree in SyntaxTrees)
        {
            cancellationToken.ThrowIfCancellationRequested();
            AddTreeDiagnostics(syntaxTree);
        }

        cancellationToken.ThrowIfCancellationRequested();
        foreach (var diagnostic in GetMacroRegistry().Diagnostics)
            Add(diagnostic);

        cancellationToken.ThrowIfCancellationRequested();
        var entryPointDiagnostics = GetEntryPointDiagnostics(cancellationToken);
        foreach (var diagnostic in entryPointDiagnostics)
            Add(diagnostic);

        diagnostics.RemoveAll(ShouldSuppressAsyncLacksAwait);

        if (Options.OutputKind == OutputKind.ConsoleApplication
            && entryPointDiagnostics.IsDefaultOrEmpty
            && GetEntryPoint(cancellationToken) is null)
        {
            Add(Diagnostic.Create(CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint, Location.None));
        }

        if (Options.AllowUnsafe)
            Add(Diagnostic.Create(CompilerDiagnostics.UnsafeModeEnabled, Location.None));

        if (Options.MembersPublicByDefaultConfigured && !Options.MembersPublicByDefault)
            Add(Diagnostic.Create(CompilerDiagnostics.ExplicitPublicAccessibilityRequired, Location.None));

        return diagnostics.OrderBy(x => x.Location).ToImmutableArray();

        void AddTreeDiagnostics(SyntaxTree syntaxTree)
        {
            foreach (var diagnostic in syntaxTree.GetDiagnostics(cancellationToken))
            {
                cancellationToken.ThrowIfCancellationRequested();
                Add(diagnostic);
            }

            var model = GetSemanticModel(syntaxTree);
            foreach (var diagnostic in model.GetDiagnostics(cancellationToken))
            {
                cancellationToken.ThrowIfCancellationRequested();
                Add(diagnostic);
            }
        }

        void Add(Diagnostic diagnostic)
        {
            var mapped = ApplyCompilationOptions(diagnostic, analyzerOptions?.ReportSuppressedDiagnostics ?? false, cancellationToken);
            if (mapped is not null)
                diagnostics.Add(mapped);
        }

        bool ShouldSuppressAsyncLacksAwait(Diagnostic diagnostic)
        {
            if (diagnostic.Id != CompilerDiagnostics.AsyncLacksAwait.Id)
                return false;

            var sourceTree = diagnostic.Location.SourceTree;
            if (sourceTree is null)
                return false;

            var root = sourceTree.GetRoot(cancellationToken);
            var node = root.FindNode(diagnostic.Location.SourceSpan, getInnermostNodeForTie: true);

            var asyncNode = node.AncestorsAndSelf()
                .FirstOrDefault(n => n is FunctionStatementSyntax or MethodDeclarationSyntax or AccessorDeclarationSyntax);

            if (asyncNode is null)
                return false;

            var model = GetSemanticModel(sourceTree);
            SyntaxNode? bodySyntax = asyncNode switch
            {
                FunctionStatementSyntax function => (SyntaxNode?)function.Body ?? function.ExpressionBody?.Expression,
                MethodDeclarationSyntax method => (SyntaxNode?)method.Body ?? method.ExpressionBody?.Expression,
                AccessorDeclarationSyntax accessor => (SyntaxNode?)accessor.Body ?? accessor.ExpressionBody?.Expression,
                _ => null,
            };

            if (bodySyntax is null)
                return false;

            if (asyncNode is AccessorDeclarationSyntax accessorDeclaration &&
                ShouldSuppressAwaitlessTaskAccessor(model, accessorDeclaration, bodySyntax))
            {
                return true;
            }

            var hasAwaitSyntax = ContainsAwaitExpressionOutsideNestedFunctions(bodySyntax);
            var hasAwaitBound = model.GetBoundNode(bodySyntax) is BoundNode bound && AsyncLowerer.ContainsAwait(bound);

            return hasAwaitSyntax || hasAwaitBound;
        }
    }

    public ImmutableArray<Diagnostic> GetDiagnostics(
        SyntaxTree syntaxTree,
        CompilationWithAnalyzersOptions? analyzerOptions = null,
        CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(syntaxTree);

        if (!SyntaxTrees.Contains(syntaxTree))
            throw new ArgumentException("Syntax tree is not part of compilation", nameof(syntaxTree));

        var diagnostics = new List<Diagnostic>();

        EnsureSetup();

        foreach (var diagnostic in syntaxTree.GetDiagnostics(cancellationToken))
        {
            cancellationToken.ThrowIfCancellationRequested();
            Add(diagnostic);
        }

        var model = GetSemanticModel(syntaxTree);
        foreach (var diagnostic in model.GetDiagnostics(cancellationToken))
        {
            cancellationToken.ThrowIfCancellationRequested();
            Add(diagnostic);
        }

        foreach (var diagnostic in GetMacroRegistry().Diagnostics)
        {
            cancellationToken.ThrowIfCancellationRequested();
            if (BelongsToTree(diagnostic, syntaxTree))
                Add(diagnostic);
        }

        foreach (var diagnostic in GetEntryPointDiagnostics(cancellationToken))
        {
            cancellationToken.ThrowIfCancellationRequested();
            if (BelongsToTree(diagnostic, syntaxTree))
                Add(diagnostic);
        }

        diagnostics.RemoveAll(ShouldSuppressAsyncLacksAwait);
        return diagnostics.OrderBy(x => x.Location).ToImmutableArray();

        void Add(Diagnostic diagnostic)
        {
            var mapped = ApplyCompilationOptions(diagnostic, analyzerOptions?.ReportSuppressedDiagnostics ?? false, cancellationToken);
            if (mapped is not null)
                diagnostics.Add(mapped);
        }

        bool ShouldSuppressAsyncLacksAwait(Diagnostic diagnostic)
        {
            if (diagnostic.Id != CompilerDiagnostics.AsyncLacksAwait.Id)
                return false;

            var sourceTree = diagnostic.Location.SourceTree;
            if (sourceTree is null || !ReferenceEquals(sourceTree, syntaxTree))
                return false;

            var root = sourceTree.GetRoot(cancellationToken);
            var node = root.FindNode(diagnostic.Location.SourceSpan, getInnermostNodeForTie: true);

            var asyncNode = node.AncestorsAndSelf()
                .FirstOrDefault(n => n is FunctionStatementSyntax or MethodDeclarationSyntax or AccessorDeclarationSyntax);

            if (asyncNode is null)
                return false;

            SyntaxNode? bodySyntax = asyncNode switch
            {
                FunctionStatementSyntax function => (SyntaxNode?)function.Body ?? function.ExpressionBody?.Expression,
                MethodDeclarationSyntax method => (SyntaxNode?)method.Body ?? method.ExpressionBody?.Expression,
                AccessorDeclarationSyntax accessor => (SyntaxNode?)accessor.Body ?? accessor.ExpressionBody?.Expression,
                _ => null,
            };

            if (bodySyntax is null)
                return false;

            if (asyncNode is AccessorDeclarationSyntax accessorDeclaration &&
                ShouldSuppressAwaitlessTaskAccessor(model, accessorDeclaration, bodySyntax))
            {
                return true;
            }

            var hasAwaitSyntax = ContainsAwaitExpressionOutsideNestedFunctions(bodySyntax);
            var hasAwaitBound = model.GetBoundNode(bodySyntax) is BoundNode bound && AsyncLowerer.ContainsAwait(bound);

            return hasAwaitSyntax || hasAwaitBound;
        }

        static bool BelongsToTree(Diagnostic diagnostic, SyntaxTree syntaxTree)
        {
            if (ReferenceEquals(diagnostic.Location.SourceTree, syntaxTree))
                return true;

            var diagnosticPath = diagnostic.Location.GetLineSpan().Path;
            return !string.IsNullOrWhiteSpace(diagnosticPath) &&
                   !string.IsNullOrWhiteSpace(syntaxTree.FilePath) &&
                   string.Equals(diagnosticPath, syntaxTree.FilePath, StringComparison.OrdinalIgnoreCase);
        }

    }

    private bool ShouldSuppressAwaitlessTaskAccessor(SemanticModel model, AccessorDeclarationSyntax accessor, SyntaxNode bodySyntax)
    {
        if (model.GetDeclaredSymbol(accessor) is not IMethodSymbol method ||
            method.ReturnType is not { TypeKind: not TypeKind.Error } returnType ||
            AsyncReturnTypeUtilities.ExtractAsyncResultType(this, returnType) is not
            { SpecialType: SpecialType.System_Unit or SpecialType.System_Void })
        {
            return false;
        }

        if (model.GetBoundNode(bodySyntax) is not BoundBlockStatement block)
            return false;

        return block.Statements
            .OfType<BoundReturnStatement>()
            .Any(returnStatement =>
                returnStatement.Expression?.Type is { TypeKind: not TypeKind.Error } expressionType &&
                ClassifyConversion(expressionType, returnType, includeUserDefined: true).Exists);
    }

    internal ImmutableArray<Diagnostic> GetDocumentDiagnostics(
        SyntaxTree syntaxTree,
        CompilationWithAnalyzersOptions? analyzerOptions = null,
        CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(syntaxTree);

        if (!SyntaxTrees.Contains(syntaxTree))
            throw new ArgumentException("Syntax tree is not part of compilation", nameof(syntaxTree));

        var diagnostics = new List<Diagnostic>();

        EnsureSetup();

        foreach (var diagnostic in syntaxTree.GetDiagnostics(cancellationToken))
        {
            cancellationToken.ThrowIfCancellationRequested();
            Add(diagnostic);
        }

        var model = GetSemanticModel(syntaxTree);
        foreach (var diagnostic in model.GetDocumentDiagnostics(cancellationToken))
        {
            cancellationToken.ThrowIfCancellationRequested();
            Add(diagnostic);
        }

        return diagnostics.OrderBy(x => x.Location).ToImmutableArray();

        void Add(Diagnostic diagnostic)
        {
            var mapped = ApplyCompilationOptions(diagnostic, analyzerOptions?.ReportSuppressedDiagnostics ?? false, cancellationToken);
            if (mapped is not null)
                diagnostics.Add(mapped);
        }
    }

    public ImmutableArray<Diagnostic> GetSyntaxDiagnostics(
        SyntaxTree syntaxTree,
        CompilationWithAnalyzersOptions? analyzerOptions = null,
        CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(syntaxTree);

        if (!SyntaxTrees.Contains(syntaxTree))
            throw new ArgumentException("Syntax tree is not part of compilation", nameof(syntaxTree));

        var diagnostics = new List<Diagnostic>();

        EnsureSetup();

        foreach (var diagnostic in syntaxTree.GetDiagnostics(cancellationToken))
        {
            cancellationToken.ThrowIfCancellationRequested();
            var mapped = ApplyCompilationOptions(diagnostic, analyzerOptions?.ReportSuppressedDiagnostics ?? false, cancellationToken);
            if (mapped is not null)
                diagnostics.Add(mapped);
        }

        return diagnostics.OrderBy(x => x.Location).ToImmutableArray();
    }

    internal Diagnostic? ApplyCompilationOptions(
        Diagnostic diagnostic,
        bool reportSuppressedDiagnostics = false,
        CancellationToken cancellationToken = default)
    {
        if (!Options.EnableSuggestions &&
            diagnostic.Properties.ContainsKey(Diagnostics.SuggestionsDiagnosticProperties.OriginalCodeKey) &&
            diagnostic.Properties.ContainsKey(Diagnostics.SuggestionsDiagnosticProperties.RewrittenCodeKey))
        {
            return null;
        }

        var mappedDiagnostic = diagnostic;
        var isSuppressed = false;

        if (TryGetReportDiagnostic(diagnostic.Descriptor.Id, out var report))
        {
            if (report == ReportDiagnostic.Suppress)
                isSuppressed = true;

            if (!isSuppressed && report != ReportDiagnostic.Default)
            {
                var severity = report switch
                {
                    ReportDiagnostic.Error => DiagnosticSeverity.Error,
                    ReportDiagnostic.Warn => DiagnosticSeverity.Warning,
                    ReportDiagnostic.Info => DiagnosticSeverity.Info,
                    ReportDiagnostic.Hidden => DiagnosticSeverity.Hidden,
                    _ => mappedDiagnostic.Severity
                };

                if (severity != mappedDiagnostic.Severity)
                    mappedDiagnostic = mappedDiagnostic.WithSeverity(severity);
            }
        }

        if (IsSuppressedInSource(mappedDiagnostic))
            isSuppressed = true;

        if (isSuppressed)
            return reportSuppressedDiagnostics ? mappedDiagnostic.WithSuppression(true) : null;

        return mappedDiagnostic;

        bool IsSuppressedInSource(Diagnostic candidate)
        {
            var sourceTree = candidate.Location.SourceTree;
            if (sourceTree is null)
                return false;

            var suppressionMap = _sourceDiagnosticSuppressionMaps.GetOrAdd(
                sourceTree,
                static syntaxTree => SourceDiagnosticSuppressionMap.Create(syntaxTree));

            return suppressionMap.IsSuppressed(candidate);
        }

        bool TryGetReportDiagnostic(string diagnosticId, out ReportDiagnostic mappedReport)
        {
            if (Options.SpecificDiagnosticOptions.TryGetValue(diagnosticId, out mappedReport))
                return true;

            return Options.SpecificDiagnosticOptions.TryGetValue("*", out mappedReport);
        }
    }
}
