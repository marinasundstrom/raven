using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    public ImmutableArray<Diagnostic> GetDiagnostics(CompilationWithAnalyzersOptions? analyzerOptions = null, CancellationToken cancellationToken = default)
    {
        var diagnostics = new List<Diagnostic>();

        EnsureSetup();

        foreach (var syntaxTree in SyntaxTrees)
        {
            foreach (var diagnostic in syntaxTree.GetDiagnostics(cancellationToken))
                Add(diagnostic);

            var model = GetSemanticModel(syntaxTree);
            foreach (var diagnostic in model.GetDiagnostics(cancellationToken))
                Add(diagnostic);
        }

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

        return diagnostics.OrderBy(x => x.Location).ToImmutableArray();

        void Add(Diagnostic diagnostic)
        {
            var mapped = ApplyCompilationOptions(diagnostic, analyzerOptions?.ReportSuppressedDiagnostics ?? false);
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

            var hasAwaitSyntax = ContainsAwaitExpressionOutsideNestedFunctions(bodySyntax);
            var hasAwaitBound = model.GetBoundNode(bodySyntax) is BoundNode bound && AsyncLowerer.ContainsAwait(bound);

            return hasAwaitSyntax || hasAwaitBound;
        }
    }

    internal Diagnostic? ApplyCompilationOptions(Diagnostic diagnostic, bool reportSuppressedDiagnostics = false)
    {
        if (!Options.EnableSuggestions &&
            diagnostic.Properties.ContainsKey(Diagnostics.SuggestionsDiagnosticProperties.OriginalCodeKey) &&
            diagnostic.Properties.ContainsKey(Diagnostics.SuggestionsDiagnosticProperties.RewrittenCodeKey))
        {
            return null;
        }

        if (TryGetReportDiagnostic(diagnostic.Descriptor.Id, out var report))
        {
            if (report == ReportDiagnostic.Suppress)
                return reportSuppressedDiagnostics ? diagnostic.WithSuppression(true) : null;

            if (report != ReportDiagnostic.Default)
            {
                var severity = report switch
                {
                    ReportDiagnostic.Error => DiagnosticSeverity.Error,
                    ReportDiagnostic.Warn => DiagnosticSeverity.Warning,
                    ReportDiagnostic.Info => DiagnosticSeverity.Info,
                    ReportDiagnostic.Hidden => DiagnosticSeverity.Hidden,
                    _ => diagnostic.Severity
                };

                if (severity != diagnostic.Severity)
                    return diagnostic.WithSeverity(severity);
            }
        }

        return diagnostic;

        bool TryGetReportDiagnostic(string diagnosticId, out ReportDiagnostic mappedReport)
        {
            if (Options.SpecificDiagnosticOptions.TryGetValue(diagnosticId, out mappedReport))
                return true;

            return Options.SpecificDiagnosticOptions.TryGetValue("*", out mappedReport);
        }
    }
}
